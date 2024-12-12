(* Fetches a list of upcoming matches for today. Returns a list of tuples where
   each tuple contains: - An integer fixture ID (unique identifier for the
   match). - A string for the home team name. - A string for the away team name.
   Limits the results to a maximum of 10 matches. *)
val get_upcoming_matches :
  ?http_get:
    (Uri.t -> Cohttp.Header.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
  unit ->
  (int * string * string) list

(* Fetches the result of a specific match by fixture ID. Takes an integer
   fixture ID as input and returns a string indicating: - The name of the
   winning team if the match is finished. - "Draw" if the match ended in a tie.
   - "Not Finished" if the match has not yet been played. - "Cancelled" if the
   match was cancelled. - "Unknown Status" if the match status is unclear. In
   case of errors, returns appropriate error messages as strings. *)
val get_match_result :
  ?http_get:
    (Uri.t -> Cohttp.Header.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
  int ->
  string

val format_date : float -> string
(** [format_date timestamp] converts a Unix timestamp to a formatted date string
    (YYYY-MM-DD) *)

val get_match_winner_odds : int -> (float * float * float) option
(** Fetches the match winner odds for a given fixture ID from the API.
    @param fixture_id:
      An integer representing the fixture ID for which the odds are to be
      fetched.
    @return
      : An option type that contains a tuple of three floats representing the
      odds for the home team, draw, and away team, or [None] if the odds are not
      available or an error occurs. *)
