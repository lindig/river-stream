type config = {
    params : Estimate.params
  ; bearing : float
  ; window : float
  ; max_size : int
}

let default_config =
  {
    params = Estimate.default_params
  ; bearing = 0.0
  ; window = 15.0
  ; max_size = 4 * 1024 * 1024
  }

type counts = { downstream : int; upstream : int }

type t = {
    records : int
  ; complete : int
  ; labelled : counts
  ; selected : counts
  ; first_time : float option
  ; last_time : float option
  ; outcome : Estimate.outcome
}

let counts moving =
  let downstream, upstream = Estimate.count moving in
  { downstream; upstream }

let of_track config (track : Track.t) =
  let points = track.points in
  let labelled =
    Direction.label ~bearing:config.bearing ~window:config.window points
  in
  let selected = Estimate.select config.params labelled in
  {
    records = track.records
  ; complete = Array.length points
  ; labelled = counts labelled
  ; selected = counts selected
  ; first_time = track.first_time
  ; last_time = track.last_time
  ; outcome = Estimate.run config.params selected
  }

let of_file config path =
  Track.load ~max_size:config.max_size path |> Result.map (of_track config)
