open Estimate

let num x =
  if Float.is_finite x then `Float (Float.of_string (Printf.sprintf "%.6f" x))
  else `Null

(** "2026-10-12 17:32:28 UTC": the time as recorded in the FIT file *)
let time_json t =
  match Ptime.of_float_s t with
  | None -> `Null
  | Some p ->
      let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time p in
      `String
        (Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d UTC" y m d hh mm ss)

let option_time = function Some t -> time_json t | None -> `Null

let duration_json (a : Analysis.t) =
  match (a.first_time, a.last_time) with
  | Some t0, Some t1 ->
      let s = int_of_float (Float.round (t1 -. t0)) in
      `String
        (Printf.sprintf "%d:%02d:%02d" (s / 3600) (s mod 3600 / 60) (s mod 60))
  | _ -> `Null

let session_fields (a : Analysis.t) =
  [
    ("start", option_time a.first_time)
  ; ("end", option_time a.last_time)
  ; ("duration", duration_json a)
  ]

let data_json (a : Analysis.t) =
  `Assoc
    [
      ("records", `Int a.records)
    ; ("complete_records", `Int a.complete)
    ; ( "points_with_direction"
      , `Assoc
          [
            ("downstream", `Int a.labelled.downstream)
          ; ("upstream", `Int a.labelled.upstream)
          ] )
    ; ( "points_used"
      , `Assoc
          [
            ("downstream", `Int a.selected.downstream)
          ; ("upstream", `Int a.selected.upstream)
          ] )
    ]

let dir_stats_json (s : dir_stats) =
  `Assoc
    [
      ("points", `Int s.count)
    ; ("mean_speed_ms", num s.mean_speed)
    ; ("mean_cadence", num s.mean_cadence)
    ; ("cadence_p10", num s.cadence_p10)
    ; ("cadence_p90", num s.cadence_p90)
    ]

let bootstrap_json = function
  | None -> `Null
  | Some b ->
      `Assoc
        [
          ("low", num b.low)
        ; ("high", num b.high)
        ; ("replicates", `Int b.replicates)
        ]

let overlap_json = function
  | None -> `Null
  | Some (o : overlap) ->
      `Assoc
        [
          ("cadence_low", num o.cadence_low)
        ; ("cadence_high", num o.cadence_high)
        ; ("downstream_points", `Int o.downstream)
        ; ("upstream_points", `Int o.upstream)
        ]

let estimate_fields (params : params) (a : Analysis.t) (r : Estimate.result) =
  let reg = r.regression in
  let low, high = r.ci_normal in
  [ ("status", `String "ok") ]
  @ session_fields a
  @ [
      ( "stream"
      , `Assoc
          [
            ("speed_ms", num reg.u)
          ; ("speed_kmh", num (reg.u *. 3.6))
          ; ("std_error", num reg.u_se)
          ; ("ci95_normal", `List [ num low; num high ])
          ; ("ci95_bootstrap", bootstrap_json r.ci_bootstrap)
          ] )
    ; ( "model"
      , `Assoc
          [
            ("form", `String "ground_speed = k * cadence^exponent + d * stream")
          ; ("exponent", num params.exponent)
          ; ("k", num reg.k)
          ; ("k_std_error", num reg.k_se)
          ; ("residual_sd_ms", num reg.sigma)
          ; ("points", `Int reg.n)
          ] )
    ; ("data", data_json a)
    ; ( "directions"
      , `Assoc
          [
            ("downstream", dir_stats_json r.downstream)
          ; ("upstream", dir_stats_json r.upstream)
          ] )
    ; ( "diagnostics"
      , `Assoc
          [
            ("best_exponent", num r.best_exponent)
          ; ("stream_at_best_exponent_ms", num r.stream_at_best_exponent)
          ; ("shared_cadence_range", overlap_json r.overlap)
          ; ("warnings", `List (List.map (fun w -> `String w) r.warnings))
          ] )
    ]

let reason_json = function
  | No_usable_points ->
      ( "no_usable_points"
      , "no points with position, speed and cadence that pass the speed \
         threshold" )
  | One_direction_only dir ->
      ( "one_direction_only"
      , Printf.sprintf
          "all movement is %s; with a single direction the stream cannot be \
           separated from the boat's own speed"
          (Direction.to_string dir) )
  | Too_few_points { required; downstream; upstream } ->
      ( "too_few_points"
      , Printf.sprintf
          "at least %d points are needed in each direction but there are %d \
           downstream and %d upstream"
          required downstream upstream )

let no_estimate_fields (a : Analysis.t) reason =
  let code, message = reason_json reason in
  [ ("status", `String "no_estimate") ]
  @ session_fields a
  @ [
      ("reason", `String code)
    ; ("message", `String message)
    ; ("stream", `Null)
    ; ("data", data_json a)
    ]

let file (config : Analysis.config) path result =
  let fields =
    match result with
    | Error message ->
        [ ("status", `String "error"); ("message", `String message) ]
    | Ok (a : Analysis.t) -> (
        match a.outcome with
        | Estimate r -> estimate_fields config.params a r
        | No_estimate reason -> no_estimate_fields a reason)
  in
  `Assoc (("file", `String path) :: fields)

let document ~tool ~version (config : Analysis.config) files =
  let p = config.params in
  `Assoc
    [
      ("tool", `String tool)
    ; ("version", `String version)
    ; ( "convention"
      , `String
          "stream speed is positive when the water flows in the downstream \
           direction; downstream is along the river axis towards the given \
           bearing" )
    ; ( "parameters"
      , `Assoc
          [
            ("min_speed_ms", num p.min_speed)
          ; ("max_speed_ms", num p.max_speed)
          ; ( "max_cadence"
            , match p.max_cadence with Some c -> num c | None -> `Null )
          ; ("exponent", num p.exponent)
          ; ("downstream_bearing_deg", num config.bearing)
          ; ("direction_window_s", num config.window)
          ; ("min_points_per_direction", `Int p.min_points)
          ; ("bootstrap_replicates", `Int p.bootstrap)
          ; ("bootstrap_block_s", num p.block)
          ; ("seed", `Int p.seed)
          ] )
    ; ("files", `List files)
    ]
