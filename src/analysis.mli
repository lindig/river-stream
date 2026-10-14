(** The complete pipeline for one FIT file: read, label directions, select
    points and estimate the stream. *)

type config = {
    params : Estimate.params
  ; bearing : float  (** compass bearing (degrees) of the river's flow *)
  ; window : float  (** half-width of the direction window, seconds *)
  ; max_size : int  (** largest FIT file accepted, bytes *)
}

val default_config : config

type counts = { downstream : int; upstream : int }

type t = {
    records : int  (** "record" messages in the file *)
  ; complete : int  (** records with position, speed and cadence *)
  ; labelled : counts  (** points with a direction of travel *)
  ; selected : counts  (** points that enter the estimate *)
  ; first_time : float option
        (** first timestamp in the FIT file (UTC epoch) *)
  ; last_time : float option  (** last timestamp in the FIT file (UTC epoch) *)
  ; outcome : Estimate.outcome
}

val of_track : config -> Track.t -> t
(** Pure part of the analysis. *)

val of_file : config -> string -> (t, string) result
(** Read the FIT file at the given path and analyse it. *)
