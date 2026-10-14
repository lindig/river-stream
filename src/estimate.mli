(** Estimating the stream of a river from boat speed and stroke rate.

    Model: for each selected GPS point,

    {v ground_speed = k * cadence^exponent + d * u v}

    where [d] is +1 downstream and -1 upstream, [k] is a constant for the
    crew/session and [u] is the stream speed (positive when the water flows
    downstream). For a fixed exponent this is a linear least-squares problem in
    [k] and [u]. The stream acts equally on both directions; it is identified by
    comparing speeds at equal effort in both directions, so movement in both
    directions is required. *)

type params = {
    min_speed : float  (** ignore points slower than this, m/s *)
  ; max_speed : float  (** ignore points faster than this (GPS glitches), m/s *)
  ; max_cadence : float option  (** ignore points with a higher stroke rate *)
  ; exponent : float  (** effort exponent: speed ~ cadence^exponent *)
  ; min_points : int  (** required number of points in each direction (>= 3) *)
  ; bootstrap : int  (** bootstrap replicates; 0 disables the bootstrap *)
  ; block : float  (** bootstrap block length, seconds *)
  ; seed : int  (** random seed for the bootstrap *)
}

val default_params : params

type regression = {
    k : float  (** speed per cadence^exponent *)
  ; u : float  (** stream speed in m/s, positive downstream *)
  ; k_se : float  (** standard error of [k] *)
  ; u_se : float  (** standard error of [u] (ignores autocorrelation) *)
  ; sigma : float  (** residual standard deviation, m/s *)
  ; rss : float  (** residual sum of squares *)
  ; n : int  (** points used *)
}

type dir_stats = {
    count : int
  ; mean_speed : float
  ; mean_cadence : float
  ; cadence_p10 : float
  ; cadence_p90 : float
}

type bootstrap = { low : float; high : float; replicates : int }
(** 95% percentile interval of the stream over block-bootstrap replicates *)

type overlap = {
    cadence_low : float
  ; cadence_high : float  (** stroke rate range shared by both directions *)
  ; downstream : int  (** downstream points inside that range *)
  ; upstream : int  (** upstream points inside that range *)
}

type result = {
    regression : regression
  ; ci_normal : float * float  (** u +/- 1.96 standard errors *)
  ; ci_bootstrap : bootstrap option
  ; best_exponent : float
        (** exponent in [0.05, 1.5] with the smallest misfit *)
  ; stream_at_best_exponent : float
  ; overlap : overlap option
        (** stroke rates (5th to 95th percentile) used in both directions *)
  ; downstream : dir_stats
  ; upstream : dir_stats
  ; warnings : string list
}

type reason =
  | No_usable_points
  | One_direction_only of Direction.dir
      (** all selected movement is in this direction *)
  | Too_few_points of { required : int; downstream : int; upstream : int }

type outcome = Estimate of result | No_estimate of reason

val select : params -> Direction.moving array -> Direction.moving array
(** Keep points that are fast enough and have a usable stroke rate. *)

val count : Direction.moving array -> int * int
(** [(downstream, upstream)] number of points *)

val run : params -> Direction.moving array -> outcome
(** Estimate the stream from points that were already {!select}ed (and are in
    time order). Yields [No_estimate] when the stream cannot be identified, in
    particular when all movement is in one direction. *)
