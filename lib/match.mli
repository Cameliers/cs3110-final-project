type t
(* The type representing a match. A match has a match id, an a team, a b team,
   and the match odds.*)

val match_id : t -> int

(* [match_id x] returns the id associated with match [x].*)
val a_side : t -> string

(* [a_side x] returns the a team associated with match [x].*)
val b_side : t -> string

(* [b_side x] returns the b team associated with match [x].*)
val match_odds : t -> string

(* [match_odds x] returns the odds associated with match [x].*)
val make_match : int -> string -> string -> string -> t
(* [make_match x] creates a new match given an id, an a team, a b team, and
   odds.*)

val to_string : t -> string
(** [to_string match] returns a string representation of the match [match]. The
    string will include the a-side team, b-side team, and match odds in a
    readable format. *)

val of_string : string -> t
(** [of_string str] returns a match that is parsed from its string
    representation [str]*)
