(** JSON output. *)

val file :
  Analysis.config -> string -> (Analysis.t, string) result -> Yojson.Safe.t
(** [file config path result] is the JSON object describing the analysis of one
    file, or the error that prevented it. Its ["status"] member is ["ok"],
    ["no_estimate"] or ["error"]. *)

val document :
     tool:string
  -> version:string
  -> Analysis.config
  -> Yojson.Safe.t list
  -> Yojson.Safe.t
(** The top-level object: the parameters used and one entry per file. *)
