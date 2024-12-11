(** The Profile module provides functionality to save and load user profiles to
    and from a file. *)

val save_to_file : string -> User.t -> unit
(** [save_to_file filename user] saves the [user] profile to the file specified
    by [filename]. The file will include the user's balance, active bets, and
    bet history. *)

val load_from_file : string -> User.t
(** [load_from_file filename] loads a user profile from the file specified by
    [filename]. The file is expected to contain the user's balance, active bets,
    and bet history in the appropriate format. Raises an exception if the file
    is malformed. *)

