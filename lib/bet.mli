type t

(* The type representing a bet. A bet has a bet id, a match id, a team, and an
   amount*)
val bet_id : t -> int

(* [bet_id bet] returns the bet id associated with bet [bet].*)
val match_id : t -> int

(* [match_id bet] returns *)
val bet_amount : t -> float
val bet_team : t -> string
val make_bet : int -> string -> float -> t
