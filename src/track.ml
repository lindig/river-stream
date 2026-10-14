type point = {
    time : float
  ; lat : float
  ; lon : float
  ; speed : float
  ; cadence : float
}

type t = {
    records : int
  ; first_time : float option
  ; last_time : float option
  ; points : point array
}

let point_of_record =
  Fit.Record.(
    function
    | {
        timestamp = Some time
      ; latitude = Some lat
      ; longitude = Some lon
      ; speed = Some speed
      ; cadence = Some cadence
      ; _
      } ->
        Some { time; lat; lon; speed; cadence }
    | _ -> None)

let of_records records =
  let points =
    records
    |> List.filter_map point_of_record
    |> List.stable_sort (fun a b -> Float.compare a.time b.time)
    |> Array.of_list
  in
  let times = List.filter_map (fun (r : Fit.Record.t) -> r.timestamp) records in
  let bound pick = function
    | [] -> None
    | t :: ts -> Some (List.fold_left pick t ts)
  in
  {
    records = List.length records
  ; first_time = bound Float.min times
  ; last_time = bound Float.max times
  ; points
  }

let load ~max_size path =
  let strip msg =
    (* the parser sometimes starts messages with ": " *)
    if String.length msg > 2 && String.sub msg 0 2 = ": " then
      String.sub msg 2 (String.length msg - 2)
    else msg
  in
  Fit.read ~max_size path
  |> Result.map (fun fit -> of_records (Fit.records ~join:true fit))
  |> Result.map_error strip
