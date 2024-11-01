type t
(* The type representing a match. A match has a match id, an a team, a b team,
   and the match odds.*)

(* [match_id x] returns the id associated with match [x].*)
val a_side : t -> string

(* [a_side x] returns the a team associated with match [x].*)
val b_side : t -> string

(* [b_side x] returns the b team associated with match [x].*)
val match_odds : t -> string

(* [match_odds x] returns the odds associated with match [x].*)
val make_match : string -> string -> string -> t
(* [make_match x] creates a new match given an id, an a team, a b team, and
   odds.*)
