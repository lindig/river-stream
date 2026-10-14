(** GPS tracks read from FIT files.

    Only records that carry all of timestamp, position, speed and cadence are
    usable for stream estimation; they are kept as {!point}s, sorted by time. *)

type point = {
    time : float  (** Unix epoch, seconds *)
  ; lat : float  (** degrees *)
  ; lon : float  (** degrees *)
  ; speed : float  (** ground speed in m/s *)
  ; cadence : float  (** stroke rate, strokes per minute *)
}

type t = {
    records : int  (** number of "record" messages in the FIT file *)
  ; first_time : float option
        (** earliest timestamp of any record, Unix epoch *)
  ; last_time : float option  (** latest timestamp of any record, Unix epoch *)
  ; points : point array  (** complete records, ordered by time *)
}

val of_records : Fit.Record.t list -> t
(** Build a track from decoded FIT records (in any order). *)

val load : max_size:int -> string -> (t, string) result
(** [load ~max_size path] reads the FIT file at [path]. Files larger than
    [max_size] bytes are rejected. *)
