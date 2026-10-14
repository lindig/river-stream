(** Assigning a direction of travel (downstream / upstream) to GPS points.

    The river axis is approximated by the principal axis of the track in local
    metric coordinates. It is oriented such that it points roughly along a given
    compass bearing, which defines "downstream". The direction of each point is
    the sign of the local slope of the position along that axis over time. *)

type dir = Downstream | Upstream

val to_string : dir -> string
(** ["downstream"] or ["upstream"] *)

type moving = {
    time : float  (** Unix epoch, seconds *)
  ; speed : float  (** ground speed, m/s *)
  ; cadence : float  (** strokes per minute *)
  ; along : float  (** position along the river axis in m; downstream is + *)
  ; dir : dir
}

val label : bearing:float -> window:float -> Track.point array -> moving array
(** [label ~bearing ~window points] labels the time-ordered [points]. [bearing]
    is the compass bearing in degrees (0 = north, 90 = east) in which the river
    flows on average. [window] is the half-width in seconds of the time window
    used to determine the direction of travel. Points without enough neighbours
    in their window, or without any movement along the axis, are dropped. *)
