type params = {
    min_speed : float
  ; max_speed : float
  ; max_cadence : float option
  ; exponent : float
  ; min_points : int
  ; bootstrap : int
  ; block : float
  ; seed : int
}

let default_params =
  {
    min_speed = 3.0
  ; max_speed = 6.0
  ; max_cadence = Some 40.0
  ; exponent = 1.0 /. 3.0
  ; min_points = 20
  ; bootstrap = 500
  ; block = 60.0
  ; seed = 1
  }

type regression = {
    k : float
  ; u : float
  ; k_se : float
  ; u_se : float
  ; sigma : float
  ; rss : float
  ; n : int
}

type dir_stats = {
    count : int
  ; mean_speed : float
  ; mean_cadence : float
  ; cadence_p10 : float
  ; cadence_p90 : float
}

type bootstrap = { low : float; high : float; replicates : int }

type overlap = {
    cadence_low : float
  ; cadence_high : float
  ; downstream : int
  ; upstream : int
}

type result = {
    regression : regression
  ; ci_normal : float * float
  ; ci_bootstrap : bootstrap option
  ; best_exponent : float
  ; stream_at_best_exponent : float
  ; overlap : overlap option
  ; downstream : dir_stats
  ; upstream : dir_stats
  ; warnings : string list
}

type reason =
  | No_usable_points
  | One_direction_only of Direction.dir
  | Too_few_points of { required : int; downstream : int; upstream : int }

type outcome = Estimate of result | No_estimate of reason

(* ---- selection -------------------------------------------------------- *)

let select params moving =
  let keep (m : Direction.moving) =
    m.speed >= params.min_speed
    && m.speed <= params.max_speed
    && m.cadence > 0.0
    && Option.fold params.max_cadence ~none:true ~some:(fun c -> m.cadence <= c)
  in
  moving |> Array.to_seq |> Seq.filter keep |> Array.of_seq

let count moving =
  Array.fold_left
    (fun (down, up) (m : Direction.moving) ->
      match m.dir with
      | Direction.Downstream -> (down + 1, up)
      | Direction.Upstream -> (down, up + 1))
    (0, 0) moving

(* ---- least squares ---------------------------------------------------- *)

type sums = {
    n : int
  ; see : float
  ; sed : float
  ; sdd : float
  ; sey : float
  ; sdy : float
  ; syy : float
}
(** Sufficient statistics of the regression of speed [y] on effort [e] and
    direction [d]. They add up, which makes resampling blocks cheap. *)

let zero = { n = 0; see = 0.; sed = 0.; sdd = 0.; sey = 0.; sdy = 0.; syy = 0. }

let add a b =
  {
    n = a.n + b.n
  ; see = a.see +. b.see
  ; sed = a.sed +. b.sed
  ; sdd = a.sdd +. b.sdd
  ; sey = a.sey +. b.sey
  ; sdy = a.sdy +. b.sdy
  ; syy = a.syy +. b.syy
  }

let sign = function Direction.Downstream -> 1.0 | Direction.Upstream -> -1.0

let sums_of ~exponent moving =
  Array.fold_left
    (fun s (m : Direction.moving) ->
      let e = m.cadence ** exponent and d = sign m.dir and y = m.speed in
      add s
        {
          n = 1
        ; see = e *. e
        ; sed = e *. d
        ; sdd = 1.0
        ; sey = e *. y
        ; sdy = d *. y
        ; syy = y *. y
        })
    zero moving

(** Solve the 2x2 normal equations. [None] if the design is degenerate or there
    are no residual degrees of freedom. *)
let solve s =
  let det = (s.see *. s.sdd) -. (s.sed *. s.sed) in
  if s.n < 3 || det <= 1e-12 *. s.see *. s.sdd then None
  else
    let k = ((s.sdd *. s.sey) -. (s.sed *. s.sdy)) /. det in
    let u = ((s.see *. s.sdy) -. (s.sed *. s.sey)) /. det in
    let rss = Float.max 0.0 (s.syy -. (k *. s.sey) -. (u *. s.sdy)) in
    let var = rss /. float (s.n - 2) in
    Some
      {
        k
      ; u
      ; k_se = sqrt (var *. s.sdd /. det)
      ; u_se = sqrt (var *. s.see /. det)
      ; sigma = sqrt var
      ; rss
      ; n = s.n
      }

(* ---- summaries -------------------------------------------------------- *)

(** [percentile q sorted] with linear interpolation; [sorted] is non-empty *)
let percentile q sorted =
  let pos = q *. float (Array.length sorted - 1) in
  let lo = int_of_float (floor pos) in
  let hi = min (Array.length sorted - 1) (lo + 1) in
  sorted.(lo) +. ((pos -. float lo) *. (sorted.(hi) -. sorted.(lo)))

let dir_stats moving =
  let n = Array.length moving in
  let mean f =
    Array.fold_left (fun acc m -> acc +. f m) 0.0 moving /. float n
  in
  let cadences = Array.map (fun (m : Direction.moving) -> m.cadence) moving in
  Array.sort Float.compare cadences;
  {
    count = n
  ; mean_speed = mean (fun (m : Direction.moving) -> m.speed)
  ; mean_cadence = mean (fun (m : Direction.moving) -> m.cadence)
  ; cadence_p10 = percentile 0.10 cadences
  ; cadence_p90 = percentile 0.90 cadences
  }

(* ---- bootstrap -------------------------------------------------------- *)

(** sums per consecutive [block]-second block of the (time-ordered) points *)
let block_sums params moving =
  let t0 = moving.(0).Direction.time in
  let index (m : Direction.moving) =
    int_of_float (floor ((m.time -. t0) /. params.block))
  in
  let groups =
    Array.fold_left
      (fun acc m ->
        match acc with
        | (i, ms) :: rest when i = index m -> (i, m :: ms) :: rest
        | _ -> (index m, [ m ]) :: acc)
      [] moving
  in
  groups
  |> List.rev_map (fun (_, ms) ->
      sums_of ~exponent:params.exponent (Array.of_list ms))
  |> Array.of_list

(** Resample blocks with replacement, which respects the strong autocorrelation
    of neighbouring GPS samples. *)
let bootstrap_ci params blocks =
  let nb = Array.length blocks in
  let rng = Random.State.make [| params.seed |] in
  let replicate _ =
    let total = ref zero in
    for _ = 1 to nb do
      total := add !total blocks.(Random.State.int rng nb)
    done;
    Option.map (fun r -> r.u) (solve !total)
  in
  let us =
    Array.init params.bootstrap replicate
    |> Array.to_list |> List.filter_map Fun.id |> Array.of_list
  in
  if Array.length us < 2 then None
  else (
    Array.sort Float.compare us;
    Some
      {
        low = percentile 0.025 us
      ; high = percentile 0.975 us
      ; replicates = Array.length us
      })

(* ---- diagnostics ------------------------------------------------------ *)

(** exponent (0.05 .. 1.50) with the smallest residual sum of squares; the model
    is linear in k and u for every fixed exponent *)
let best_exponent moving =
  let candidates =
    List.init 146 (fun i ->
        Float.round ((0.05 +. (0.01 *. float i)) *. 100.0) /. 100.0)
  in
  let fit p =
    Option.map (fun r -> (p, r)) (solve (sums_of ~exponent:p moving))
  in
  candidates |> List.filter_map fit
  |> List.fold_left
       (fun best (p, r) ->
         match best with
         | Some (_, b) when b.rss <= r.rss -> best
         | _ -> Some (p, r))
       None

let sorted_cadences moving =
  let cadences = Array.map (fun (m : Direction.moving) -> m.cadence) moving in
  Array.sort Float.compare cadences;
  cadences

let count_within low high cadences =
  Array.fold_left
    (fun n c -> if c >= low && c <= high then n + 1 else n)
    0 cadences

(** The stroke rate range (5th to 95th percentile) that both directions have in
    common; the stream is identified by comparing both directions at equal
    effort. [None] if the ranges are disjoint. *)
let overlap ~down ~up =
  let low = Float.max (percentile 0.05 down) (percentile 0.05 up) in
  let high = Float.min (percentile 0.95 down) (percentile 0.95 up) in
  if low > high then None
  else
    Some
      {
        cadence_low = low
      ; cadence_high = high
      ; downstream = count_within low high down
      ; upstream = count_within low high up
      }

let warnings ~params ~regression ~overlap ~blocks =
  List.filter_map Fun.id
    [
      (if regression.u +. (1.96 *. regression.u_se) < 0.0 then
         Some
           "negative stream: the water seems to flow towards the 'upstream' \
            direction; check the downstream bearing"
       else None)
    ; (match overlap with
      | None ->
          Some
            "the stroke rates used in the two directions do not overlap; \
             stream and effort cannot be separated"
      | Some (o : overlap) when min o.downstream o.upstream < params.min_points
        ->
          Some
            (Printf.sprintf
               "only %d downstream and %d upstream points share a stroke rate \
                range; the stream is weakly identified"
               o.downstream o.upstream)
      | Some _ -> None)
    ; (if params.bootstrap > 0 && blocks < 10 then
         Some
           (Printf.sprintf
              "only %d bootstrap blocks; the bootstrap interval is unreliable"
              blocks)
       else None)
    ]

(* ---- driver ----------------------------------------------------------- *)

let in_direction dir moving =
  moving |> Array.to_seq
  |> Seq.filter (fun (m : Direction.moving) -> m.dir = dir)
  |> Array.of_seq

let fit_result params moving regression =
  let down = dir_stats (in_direction Direction.Downstream moving) in
  let up = dir_stats (in_direction Direction.Upstream moving) in
  let blocks = block_sums params moving in
  let overlap =
    overlap
      ~down:(sorted_cadences (in_direction Direction.Downstream moving))
      ~up:(sorted_cadences (in_direction Direction.Upstream moving))
  in
  let bootstrap =
    if params.bootstrap > 0 then bootstrap_ci params blocks else None
  in
  let best, stream_at_best =
    match best_exponent moving with
    | Some (p, r) -> (p, r.u)
    | None -> (params.exponent, regression.u)
  in
  let half = 1.96 *. regression.u_se in
  {
    regression
  ; ci_normal = (regression.u -. half, regression.u +. half)
  ; ci_bootstrap = bootstrap
  ; best_exponent = best
  ; stream_at_best_exponent = stream_at_best
  ; overlap
  ; downstream = down
  ; upstream = up
  ; warnings =
      warnings ~params ~regression ~overlap ~blocks:(Array.length blocks)
  }

let run params moving =
  match count moving with
  | 0, 0 -> No_estimate No_usable_points
  | _, 0 -> No_estimate (One_direction_only Direction.Downstream)
  | 0, _ -> No_estimate (One_direction_only Direction.Upstream)
  | downstream, upstream when min downstream upstream < params.min_points ->
      No_estimate
        (Too_few_points { required = params.min_points; downstream; upstream })
  | downstream, upstream -> (
      match solve (sums_of ~exponent:params.exponent moving) with
      | Some regression -> Estimate (fit_result params moving regression)
      | None ->
          No_estimate
            (Too_few_points
               { required = params.min_points; downstream; upstream }))
