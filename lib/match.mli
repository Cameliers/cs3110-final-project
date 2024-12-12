type t
(** The type representing a match. A match has:
    - a match id (int)
    - an a-side team (string)
    - a b-side team (string)
    - odds represented as a string. *)

val match_id : t -> int
(** [match_id x] returns the unique identifier of match [x]. *)

val a_side : t -> string
(** [a_side x] returns the a-side team of match [x]. *)

val b_side : t -> string
(** [b_side x] returns the b-side team of match [x]. *)

val match_odds : t -> string
(** [match_odds x] returns the odds associated with match [x], as a string. *)

val make_match : int -> string -> string -> t
(** [make_match id a b odds] creates a new match with identifier [id], a-side
    team [a], b-side team [b]. *)

val to_string : t -> string
(** [to_string match] returns a string representation of [match]. The string
    includes the match id, the a-side team, the b-side team, and the odds in a
    human-readable format. *)

val of_string : string -> t
(** [of_string str] returns a match parsed from [str]. It expects a format
    matching that produced by [to_string]. Raises [Failure] if the format is
    invalid. *)
