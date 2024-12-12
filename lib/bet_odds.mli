(* This OCaml library provides functions for calculating match odds between two teams based on their past performance. *)

exception InvalidData

(** Computes the average of a list of floats.
    @param lst The list of floats.
    @raise InvalidData if the list is empty.
    @return The average of the list. *)
val average : float list -> float

(** Computes the average goals scored for and against a team from a list of results.
    @param results A list of pairs of floats, where each pair represents (goals_for, goals_against).
    @raise InvalidData if the list is empty.
    @return A pair of floats representing the average goals for and against. *)
val avg_for_against : (float * float) list -> (float * float)

(** Computes the Poisson probability mass function (PMF) for a given lambda and k.
    @param lambda The average rate of events.
    @param k The number of events.
    @return The Poisson PMF for the given lambda and k. *)
val poisson_pmf : float -> int -> float

(** Computes the match odds between two teams based on their past performance.
    @param a_results A list of results for team A.
    @param b_results A list of results for team B.
    @return A string representing the odds in the format "a to b". *)
val match_odds : (float * float) list -> (float * float) list -> string