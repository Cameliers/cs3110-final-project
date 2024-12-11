(** The Profile module provides functionality to save and load user profiles to
    and from a file, as well as helper functions to serialize and deserialize
    bets and balance histories. *)

val bets_to_string : Bet.t list -> string
(** [bets_to_string bets] returns a string representation of the list of bets
    [bets]. The resulting string is in the format:
    ["[ bet1_str; bet2_str; ... ]"] where each [betX_str] is the string
    representation of a single bet. *)

val string_to_bets : string -> Bet.t list
(** [string_to_bets str] parses [str] to produce a list of bets. It expects a
    format like ["[ bet1_str; bet2_str; ... ]"], which should match the format
    produced by [bets_to_string]. If [str] is "[]", an empty list is returned.
    Raises [Failure] if the format is invalid. *)

val balance_history_to_string : float list * float -> string
(** [balance_history_to_string (floats, f)] returns a string representation of
    the balance history [(floats, f)]. The format is: ["([f1; f2; ...], f)"]
    where each [fi] is a float and [f] is the overall gains/losses. For example:
    "([1000.0; 950.0; 1100.0], 100.0)" *)

val string_to_balance_history : string -> float list * float
(** [string_to_balance_history str] parses [str] into a balance history tuple: a
    list of floats and a float. The string should match the format
    ["([f1; f2; ...], f)"] as produced by [balance_history_to_string]. Returns
    the corresponding [(float list, float)] pair. Raises [Failure] if the format
    is invalid or parsing fails. *)

val save_to_file : string -> User.t -> unit
(** [save_to_file filename user] saves the [user] profile to the file specified
    by [filename]. The file will contain:
    - The user's balance
    - The user's active bets
    - The user's bets history
    - The user's balance history Each component is stored in a specific
      serialized format as handled by the helper functions above. Overwrites the
      file if it already exists. *)

val load_from_file : string -> User.t
(** [load_from_file filename] loads a user profile from the file specified by
    [filename]. The file is expected to contain the user's balance, active bets,
<<<<<<< HEAD
    and bet history in the appropriate format. Raises an exception if the file
    is malformed. *)

=======
    bets history, and balance history in the formats handled by the helper
    functions. Returns a [User.t] representing the loaded user. Raises an
    exception if the file is malformed or cannot be parsed. *)
>>>>>>> ac059284bf068d85f06ce0b1ad65e5c462def8e8
