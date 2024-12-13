type t
(** The type representing a user. A user has:
    - a balance (float)
    - a list of active bets ([Bet.t list])
    - a list of past bets ([Bet.t list])
    - a balance history (float list * float), where the list tracks historical
      balances and the float tracks overall gains/losses. *)

exception Insufficient_Balance
(** Raised when the user attempts an action requiring more funds than are
    available in their balance. *)

val make_user : unit -> t
(** [make_user ()] creates a new user with:
    - an initial balance of 1000.0
    - empty active bets
    - empty bet history
    - a balance history initialized accordingly. *)

val create : float -> Bet.t list -> Bet.t list -> float list * float -> t
(** [create balance active_bets bet_history balance_history] creates a user with
    the specified [balance], lists of [active_bets], [bet_history], and a
    [balance_history]. *)

val balance : t -> float
(** [balance user] returns the current balance of [user]. *)

val bets_active : t -> Bet.t list
(** [bets_active user] returns the list of active bets for [user]. *)

val bets_history : t -> Bet.t list
(** [bets_history user] returns the list of completed bets for [user]. *)

val change_balance : t -> float -> unit
(** [change_balance user amount] updates [user]'s balance by [amount]. This may
    be positive (deposit) or negative (withdrawal/wager). Also updates the
    user's balance history. *)

val remove_bet : t -> Bet.t -> unit
(** [remove_bet user bet] removes [bet] from [user]'s active bets and refunds
    the bet amount back into the user's balance. *)

val modify_bet : t -> Bet.t -> float -> unit
(** [modify_bet user bet extra_amount] increases the stake of [bet] by
    [extra_amount], deducted from [user]'s current balance. If the balance is
    insufficient, raises [Insufficient_Balance]. *)

val balance_history : t -> float list * float
(** [balance_history user] returns the balance history of [user] as a pair:
    - a list of balances over time
    - the overall gains or losses. *)

val add_bet : t -> Match.t -> string -> float -> unit
(** [add_bet user match team amount] places a new bet of [amount] on [team] in
    [match], deducting [amount] from [user]'s balance and adding the new bet to
    the active bets. Raises [Insufficient_Balance] if insufficient funds are
    available. *)

val calculate_win : float * float -> Bet.t -> float
(** [calculate_win result bet] calculate the amount of money a winning bet makes
    given a tuple of floats. If the bet was placed on the A team then the amount
    returned is the first tuple element multiplied by the bet amount, if the bet
    was placed on the B team then the amount returned is the second tuple
    element multiplied by the bet amount *)

val complete_bets : t -> unit
(** [complete_bets user] checks all active bets of [user]. For each bet on a
    match that has a known result:
    - If the match was won by the user's chosen team, the user is awarded
      winnings.
    - If the match was a draw or cancelled, the bet is refunded.
    - If the match was lost, the user wins nothing. The completed bets are moved
      from active bets to bet history, and the user's balance is updated
      accordingly. Bets for matches not yet completed remain active. *)
