type dir = Downstream | Upstream

let to_string = function Downstream -> "downstream" | Upstream -> "upstream"

type moving = {
    time : float
  ; speed : float
  ; cadence : float
  ; along : float
  ; dir : dir
}

let pi = 4.0 *. atan 1.0
let metres_per_degree_lat = 110_540.0
let metres_per_degree_lon = 111_320.0

let mean f xs =
  Array.fold_left (fun acc x -> acc +. f x) 0.0 xs /. float (Array.length xs)

(** local equirectangular coordinates in metres around the track's centre *)
let local_xy (points : Track.point array) =
  let lat0 = mean (fun (p : Track.point) -> p.lat) points in
  let lon0 = mean (fun (p : Track.point) -> p.lon) points in
  let lon_scale = metres_per_degree_lon *. cos (lat0 *. pi /. 180.0) in
  Array.map
    (fun (p : Track.point) ->
      ((p.lon -. lon0) *. lon_scale, (p.lat -. lat0) *. metres_per_degree_lat))
    points

(** unit vector along the principal axis of the point cloud *)
let principal_axis xy =
  let mx = mean fst xy and my = mean snd xy in
  let sum f = Array.fold_left (fun acc p -> acc +. f p) 0.0 xy in
  let sxx = sum (fun (x, _) -> (x -. mx) *. (x -. mx)) in
  let syy = sum (fun (_, y) -> (y -. my) *. (y -. my)) in
  let sxy = sum (fun (x, y) -> (x -. mx) *. (y -. my)) in
  let theta = 0.5 *. atan2 (2.0 *. sxy) (sxx -. syy) in
  (cos theta, sin theta)

(** flip the axis if needed so that it points towards the given bearing *)
let orient ~bearing (ax, ay) =
  let b = bearing *. pi /. 180.0 in
  let dot = (ax *. sin b) +. (ay *. cos b) in
  if dot < 0.0 then (-.ax, -.ay) else (ax, ay)

(** least-squares slope of [ys] over [ts] for indices [lo..hi]; [None] if the
    times do not vary *)
let slope ts ys lo hi =
  let n = float (hi - lo + 1) in
  let sum xs =
    let rec loop i acc = if i > hi then acc else loop (i + 1) (acc +. xs.(i)) in
    loop lo 0.0
  in
  let mt = sum ts /. n and my = sum ys /. n in
  let rec loop i num den =
    if i > hi then (num, den)
    else
      let dt = ts.(i) -. mt in
      loop (i + 1) (num +. (dt *. (ys.(i) -. my))) (den +. (dt *. dt))
  in
  let num, den = loop lo 0.0 0.0 in
  if den > 0.0 then Some (num /. den) else None

let min_window_points = 3

(** index range of all points within [window] seconds of point [i] *)
let window_range ts ~window i =
  let n = Array.length ts in
  let rec down j =
    if j > 0 && ts.(j - 1) >= ts.(i) -. window then down (j - 1) else j
  in
  let rec up j =
    if j < n - 1 && ts.(j + 1) <= ts.(i) +. window then up (j + 1) else j
  in
  (down i, up i)

let direction_of_slope = function
  | s when s > 0.0 -> Some Downstream
  | s when s < 0.0 -> Some Upstream
  | _ -> None

let label ~bearing ~window points =
  if Array.length points = 0 then [||]
  else
    let xy = local_xy points in
    let ax, ay = orient ~bearing (principal_axis xy) in
    let along = Array.map (fun (x, y) -> (x *. ax) +. (y *. ay)) xy in
    let ts = Array.map (fun (p : Track.point) -> p.time) points in
    let classify i (p : Track.point) =
      let lo, hi = window_range ts ~window i in
      if hi - lo + 1 < min_window_points then None
      else
        Option.bind (slope ts along lo hi) direction_of_slope
        |> Option.map (fun dir ->
            {
              time = p.time
            ; speed = p.speed
            ; cadence = p.cadence
            ; along = along.(i)
            ; dir
            })
    in
    points |> Array.to_list |> List.mapi classify |> List.filter_map Fun.id
    |> Array.of_list
