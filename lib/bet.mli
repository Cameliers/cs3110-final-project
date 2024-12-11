type t
(** The type representing a bet. A bet is associated with:
    - a specific match (of type [Match.t])
    - a chosen team (a string)
    - an amount of money wagered (a float). *)

val bet_amount : t -> float
(** [bet_amount bet] returns the amount of money associated with [bet]. *)

val bet_team : t -> string
(** [bet_team bet] returns the team on which the user placed [bet]. *)

val bet_game : t -> Match.t
(** [bet_game bet] returns the match associated with [bet]. *)

val make_bet : Match.t -> string -> float -> t
(** [make_bet game team amount] creates a new bet on [team] with [amount] of
    money for the given [game]. *)

val to_string : t -> string
(** [to_string bet] returns a string representation of the bet [bet]. The string
    format will contain the match representation, the team, and the amount
    wagered, separated by commas. *)

val of_string : string -> t
(** [of_string str] parses [str] to produce a [bet]. It expects a string in the
    format produced by [to_string]. Raises [Failure] if the format is invalid. *)
