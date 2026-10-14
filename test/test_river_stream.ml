(* Tests for river-stream. FIT fixtures live in data/; see data/README.md *)

open River_stream

let failures = ref 0

let check name ok =
  if ok then Printf.printf "ok    %s\n%!" name
  else (
    incr failures;
    Printf.printf "FAIL  %s\n%!" name)

let close ~tol a b = Float.abs (a -. b) <= tol
let config = Analysis.default_config

let analyse ?(config = config) path =
  match Analysis.of_file config path with
  | Ok a -> a
  | Error msg -> failwith (path ^ ": " ^ msg)

let estimate ?config path =
  match (analyse ?config path).outcome with
  | Estimate.Estimate r -> r
  | Estimate.No_estimate _ -> failwith (path ^ ": expected an estimate")

let with_bearing bearing = { config with Analysis.bearing }
let session1 = "data/cnn-norwich-2515498-2026-10-12-1832.fit"
let session2 = "data/cnn-norwich-3134525-2026-10-13-0510.fit"

(* ---- real sessions: values agree with the R analysis ----------------- *)

let () =
  let r1 = estimate session1 and r2 = estimate session2 in
  check "session 1: stream is about 0.100 m/s downstream"
    (close ~tol:0.002 r1.regression.u 0.1005);
  check "session 2: stream is about 0.145 m/s downstream"
    (close ~tol:0.002 r2.regression.u 0.1449);
  check "the two sessions get different streams"
    (r2.regression.u -. r1.regression.u > 0.04);
  check "crew constants are about 1.27 / 1.26"
    (close ~tol:0.015 r1.regression.k 1.2735
    && close ~tol:0.015 r2.regression.k 1.2586);
  check "best exponent is close to 1/3"
    (List.for_all
       (fun (r : Estimate.result) ->
         r.best_exponent > 0.25 && r.best_exponent < 0.45)
       [ r1; r2 ]);
  let inside (r : Estimate.result) =
    match r.ci_bootstrap with
    | Some b ->
        b.low < r.regression.u && r.regression.u < b.high && b.replicates > 400
    | None -> false
  in
  check "bootstrap interval contains the estimate" (inside r1 && inside r2);
  check "both directions were used"
    (r1.downstream.count > 100 && r1.upstream.count > 100)

let () =
  let a = (estimate session1).ci_bootstrap
  and b = (estimate session1).ci_bootstrap in
  check "bootstrap is deterministic for a fixed seed" (a = b)

(* ---- default filters and timestamps ------------------------------------ *)

let () =
  let p = Estimate.default_params in
  check "default speed limits are 3 to 6 m/s"
    (p.min_speed = 3.0 && p.max_speed = 6.0);
  check "default maximum cadence is 40" (p.max_cadence = Some 40.0);
  let unfiltered =
    {
      config with
      Analysis.params = { p with max_cadence = None; max_speed = 10.0 }
    }
  in
  let n a = (a : Analysis.t).selected.downstream + a.selected.upstream in
  check "the cadence limit removes glitchy samples"
    (n (analyse session1) < n (analyse ~config:unfiltered session1));
  let a = analyse session1 in
  check "start and end are the first and last time in the file"
    (a.first_time = Some 1791826348.0 && a.last_time = Some 1791830692.0);
  let json = Yojson.Safe.to_string (Report.file config session1 (Ok a)) in
  let contains sub =
    let n = String.length sub and m = String.length json in
    let rec go i = i + n <= m && (String.sub json i n = sub || go (i + 1)) in
    go 0
  in
  check "report shows readable UTC start and end"
    (contains "\"start\":\"2026-10-12 17:32:28 UTC\""
    && contains "\"end\":\"2026-10-12 18:44:52 UTC\"");
  check "report shows the duration" (contains "\"duration\":\"1:12:24\"")

(* ---- direction convention -------------------------------------------- *)

let () =
  let north = (estimate ~config:(with_bearing 0.) session1).regression.u in
  let south = (estimate ~config:(with_bearing 180.) session1).regression.u in
  check "reversing the downstream bearing flips the sign"
    (close ~tol:1e-9 north (-.south));
  let w = (estimate ~config:(with_bearing 0.) session1).warnings in
  let w_south = (estimate ~config:(with_bearing 180.) session1).warnings in
  check "no warning with the right bearing" (w = []);
  check "negative stream triggers a warning" (w_south <> [])

(* ---- no estimate when movement is in one direction ------------------- *)

let () =
  let a = analyse "data/norwich-one-direction.fit" in
  check "one direction only: no estimate"
    (a.outcome
   = Estimate.No_estimate (Estimate.One_direction_only Direction.Downstream));
  check "one direction only: no upstream points"
    (a.selected.upstream = 0 && a.selected.downstream > 100)

let () =
  let a = analyse "data/norwich-tiny.fit" in
  check "slow points only: no usable points"
    (a.outcome = Estimate.No_estimate Estimate.No_usable_points)

let () =
  let a = analyse "data/carolin.fit" in
  check "file without stroke rate: no usable points"
    (a.outcome = Estimate.No_estimate Estimate.No_usable_points)

let () =
  let path = "data/norwich-short-return.fit" in
  check "short return leg: estimate with default minimum"
    (match (analyse path).outcome with
    | Estimate.Estimate _ -> true
    | _ -> false);
  let strict =
    { config with Analysis.params = { config.params with min_points = 50 } }
  in
  check "short return leg: too few points with a stricter minimum"
    (match (analyse ~config:strict path).outcome with
    | Estimate.No_estimate (Estimate.Too_few_points { upstream; _ }) ->
        upstream < 50
    | _ -> false)

(* ---- other real files from the fit repository ------------------------- *)

let () =
  let r =
    estimate ~config:(with_bearing 225.) "data/SpeedCoach-GPS-Workout.fit"
  in
  check "SpeedCoach file (Schuylkill, flows south-west): about 0.46 m/s"
    (close ~tol:0.03 r.regression.u 0.46);
  let r = estimate "data/xpress-4x-2022-08-03.fit" in
  check "xpress file gives a plausible stream"
    (r.regression.u > 0.05 && r.regression.u < 0.3)

(* ---- errors ----------------------------------------------------------- *)

let () =
  check "missing file is reported as an error"
    (Result.is_error (Analysis.of_file config "data/does-not-exist.fit"))

(* ---- synthetic data: exact recovery ----------------------------------- *)

let synthetic ~k ~u ~directions =
  (* noise-free: speed = k * cadence^(1/3) + d * u *)
  let make i dir =
    let cadence = 16.0 +. float (i mod 17) in
    let d =
      match dir with Direction.Downstream -> 1.0 | Direction.Upstream -> -1.0
    in
    {
      Direction.time = float i
    ; speed = (k *. (cadence ** (1.0 /. 3.0))) +. (d *. u)
    ; cadence
    ; along = float i
    ; dir
    }
  in
  Array.init 200 (fun i ->
      make i (List.nth directions (i mod List.length directions)))

let () =
  let params = { Estimate.default_params with bootstrap = 50 } in
  (match
     Estimate.run params
       (synthetic ~k:1.3 ~u:0.2
          ~directions:[ Direction.Downstream; Direction.Upstream ])
   with
  | Estimate.Estimate r ->
      check "synthetic: stream recovered exactly"
        (close ~tol:1e-9 r.regression.u 0.2);
      check "synthetic: k recovered exactly"
        (close ~tol:1e-9 r.regression.k 1.3);
      check "synthetic: zero residual" (r.regression.sigma < 1e-6)
  | Estimate.No_estimate _ -> check "synthetic: estimate expected" false);
  check "synthetic: upstream only gives no estimate"
    (Estimate.run params
       (synthetic ~k:1.3 ~u:0.2 ~directions:[ Direction.Upstream ])
    = Estimate.No_estimate (Estimate.One_direction_only Direction.Upstream));
  check "synthetic: no points gives no estimate"
    (Estimate.run params [||] = Estimate.No_estimate Estimate.No_usable_points)

let () =
  if !failures > 0 then (
    Printf.printf "%d test(s) failed\n" !failures;
    exit 1)
