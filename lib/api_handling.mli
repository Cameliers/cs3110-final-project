(* Fetches a list of upcoming matches for today. Returns a list of tuples where
   each tuple contains: - An integer fixture ID (unique identifier for the
   match). - A string for the home team name. - A string for the away team name.
   Limits the results to a maximum of 10 matches. *)
val get_upcoming_matches : unit -> (int * string * string) list

(* Fetches the result of a specific match by fixture ID. Takes an integer
   fixture ID as input and returns a string indicating: - The name of the
   winning team if the match is finished. - "Draw" if the match ended in a tie.
   - "Not Finished" if the match has not yet been played. - "Cancelled" if the
   match was cancelled. - "Unknown Status" if the match status is unclear. In
   case of errors, returns appropriate error messages as strings. *)
val get_match_result : int -> string
val get_match_winner_odds : int -> (float * float * float) option
