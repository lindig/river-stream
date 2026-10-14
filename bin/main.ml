(* river-stream: estimate the stream of a river from rowing FIT files *)

open Cmdliner
open River_stream

let tool = "river-stream"
let version = "1.1.0"

let validate (c : Analysis.config) =
  let p = c.params in
  let check ok msg = if ok then Ok () else Error msg in
  let ( let* ) = Result.bind in
  let* () = check (p.min_speed >= 0.0) "--min-speed must not be negative" in
  let* () =
    check (p.max_speed > p.min_speed) "--max-speed must exceed --min-speed"
  in
  let* () = check (p.exponent > 0.0) "--exponent must be positive" in
  let* () = check (c.window > 0.0) "--window must be positive" in
  let* () = check (p.min_points >= 3) "--min-points must be at least 3" in
  let* () = check (p.bootstrap >= 0) "--bootstrap must not be negative" in
  let* () = check (p.block > 0.0) "--block must be positive" in
  check (c.max_size > 0) "--max-size must be positive"

let analyse config path = (path, Analysis.of_file config path)

let run config compact paths =
  match validate config with
  | Error msg ->
      prerr_endline (tool ^ ": " ^ msg);
      Cmd.Exit.cli_error
  | Ok () ->
      let results = List.map (analyse config) paths in
      let files =
        List.map (fun (path, r) -> Report.file config path r) results
      in
      let json = Report.document ~tool ~version config files in
      if compact then Yojson.Safe.to_channel stdout json
      else Yojson.Safe.pretty_to_channel stdout json;
      print_newline ();
      if List.exists (fun (_, r) -> Result.is_error r) results then 1 else 0

let config_term =
  let d = Analysis.default_config in
  let p = d.params in
  let min_speed =
    let doc = "Ignore GPS points slower than $(docv) m/s (turns, starts)." in
    Arg.(value & opt float p.min_speed & info [ "min-speed" ] ~docv:"M/S" ~doc)
  in
  let max_speed =
    let doc =
      "Ignore GPS points faster than $(docv) m/s (implausible glitches)."
    in
    Arg.(value & opt float p.max_speed & info [ "max-speed" ] ~docv:"M/S" ~doc)
  in
  let max_cadence =
    let doc =
      "Ignore points with a stroke rate above $(docv) (glitches). Use inf to \
       disable."
    in
    Arg.(
      value
      & opt (some float) p.max_cadence
      & info [ "max-cadence" ] ~docv:"SPM" ~doc)
  in
  let exponent =
    let doc = "Effort exponent: boat speed ~ stroke rate^$(docv)." in
    Arg.(value & opt float p.exponent & info [ "exponent" ] ~docv:"P" ~doc)
  in
  let bearing =
    let doc =
      "Compass bearing in degrees (0 = north, 90 = east) in which the river \
       flows on average. It defines which direction along the river is \
       downstream. The default is north, which suits the Cam from Cambridge \
       towards Ely."
    in
    Arg.(
      value & opt float d.bearing
      & info [ "downstream-bearing" ] ~docv:"DEG" ~doc)
  in
  let window =
    let doc =
      "Half-width in seconds of the window that decides the direction of \
       travel."
    in
    Arg.(value & opt float d.window & info [ "window" ] ~docv:"SECONDS" ~doc)
  in
  let min_points =
    let doc = "Minimum number of points required in each direction." in
    Arg.(value & opt int p.min_points & info [ "min-points" ] ~docv:"N" ~doc)
  in
  let bootstrap =
    let doc =
      "Number of block-bootstrap replicates for the confidence interval (0 \
       disables)."
    in
    Arg.(value & opt int p.bootstrap & info [ "bootstrap" ] ~docv:"N" ~doc)
  in
  let block =
    let doc = "Length in seconds of the blocks resampled by the bootstrap." in
    Arg.(value & opt float p.block & info [ "block" ] ~docv:"SECONDS" ~doc)
  in
  let seed =
    let doc = "Seed of the random number generator used by the bootstrap." in
    Arg.(value & opt int p.seed & info [ "seed" ] ~docv:"N" ~doc)
  in
  let max_size =
    let doc = "Largest FIT file accepted, in KiB." in
    Arg.(
      value & opt int (d.max_size / 1024) & info [ "max-size" ] ~docv:"KIB" ~doc)
  in
  let make min_speed max_speed max_cadence exponent bearing window min_points
      bootstrap block seed max_size =
    {
      Analysis.params =
        {
          Estimate.min_speed
        ; max_speed
        ; max_cadence
        ; exponent
        ; min_points
        ; bootstrap
        ; block
        ; seed
        }
    ; bearing
    ; window
    ; max_size = max_size * 1024
    }
  in
  Term.(
    const make $ min_speed $ max_speed $ max_cadence $ exponent $ bearing
    $ window $ min_points $ bootstrap $ block $ seed $ max_size)

let compact =
  let doc = "Emit compact instead of indented JSON." in
  Arg.(value & flag & info [ "compact" ] ~doc)

let files =
  let doc = "FIT file to analyse. Each file is treated as one session." in
  Arg.(non_empty & pos_all string [] & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc = "estimate the stream of a river from rowing FIT files" in
  let man =
    [
      `S Manpage.s_description
    ; `P
        "Reads GPS speed and stroke rate from one or more FIT files and \
         estimates, for each file separately, the speed of the river's stream. \
         Each GPS point is labelled downstream or upstream. The ground speed \
         is modelled as $(b,k * cadence^p + d * u) where $(b,d) is +1 \
         downstream and -1 upstream, $(b,k) is a crew constant and $(b,u) is \
         the stream, which acts equally on both directions."
    ; `P
        "A stream can only be estimated when the boat moved in both \
         directions. If all movement is in one direction, the file is reported \
         with status $(b,no_estimate)."
    ; `P
        "The result is a JSON document on standard output with one entry per \
         file. $(b,stream.speed_ms) is positive when the water flows \
         downstream."
    ; `S Manpage.s_exit_status
    ; `P "0 on success, including files for which no stream can be estimated."
    ; `P "1 if at least one file could not be read or parsed."
    ]
  in
  let exits = Cmd.Exit.defaults in
  Cmd.v
    (Cmd.info tool ~version ~doc ~man ~exits)
    Term.(const run $ config_term $ compact $ files)

let () = exit (Cmd.eval' cmd)
