type t

val make_user : unit -> t
(** Makes a record for the user with a balance of $0, empty active bets list,
    empty bets history list, and auto bet set to false. *)

val create : float -> Bet.t list -> Bet.t list -> t
(** Generates a new user with the input parameters*)

val balance : t -> float
(** Returns the user balance. *)

val bets_active : t -> Bet.t list
(** Returns the list of active bets for the user. *)

val bets_history : t -> Bet.t list
(** Returns the list of bets history for the user. *)

val change_balance : t -> float -> unit
(** Change the user balance by an amount. *)

val balance_history : t -> float list * float

val add_bet : t -> Match.t -> string -> float -> unit
(** Call make_bet to make a bet on a match with a team and an amount. *)

val complete_bets : t -> unit
(** Completes a user's bets given a [user], changing the user's balance to
    reflect whether how much they made/lost off of the bets that have been
    completed. If a user's active bets are from games that haven't been
    completed, then nothing happens with that bet. All completed bets are taken
    off the user's active bets. *)
