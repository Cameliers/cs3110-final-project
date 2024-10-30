type t

val make : unit -> t
(** Makes a record for the user with a balance of $0, empty active bets list,
    empty bets history list, and auto bet set to false. *)

val balance : t -> float
(** Returns the user balance. *)

val bets_active : t -> int list
(** Returns the list of active bets for the user. *)

val bets_history : t -> int list
(** Returns the list of bets history for the user. *)

val change_balance : t -> float -> unit
(** Change the user balance by an amount. *)

(*val bet : 'a*)
(** Call make_bet to make a bet on a match with a team and an amount. *)
