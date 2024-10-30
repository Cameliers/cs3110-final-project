type t

(* The type representing a bet. A bet has a bet id, a match id, a team, and an
   amount*)

(* [bet_id bet] returns the bet id associated with bet [bet].*)

(* [match_id bet] returns *)
val bet_amount : t -> float
val bet_team : t -> string
val bet_game : t -> Match.t
val make_bet : Match.t -> string -> float -> t
