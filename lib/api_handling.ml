open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Unix

let api_key = "120de2f2f904e24bb531a8199f5faa25"

(* Abstracted HTTP GET function *)
let http_get uri headers = Client.call ~headers `GET uri

(* Helper function to format Unix timestamp to YYYY-MM-DD *)
let format_date timestamp =
  let tm = Unix.gmtime timestamp in
  Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

(* Fetches a list of the next 10 upcoming matches that have not started *)
let get_upcoming_matches ?(http_get = http_get) () :
    (int * string * string) list =
  let today = Unix.time () |> format_date in
  let uri =
    Uri.of_string
      (Printf.sprintf
         "https://v3.football.api-sports.io/fixtures?date=%s&status=NS" today)
  in
  let headers =
    Header.init () |> fun h ->
    Header.add h "x-rapidapi-key" api_key |> fun h ->
    Header.add h "x-rapidapi-host" "v3.football.api-sports.io"
  in
  Lwt_main.run
    ( http_get uri headers >>= fun (resp, body) ->
      match resp.status with
      | `OK -> (
          body |> Cohttp_lwt.Body.to_string >|= fun body_str ->
          try
            let json = Yojson.Basic.from_string body_str in
            let matches = json |> member "response" |> to_list in
            List.filteri
              (fun i _ -> i < 10)
              (List.map
                 (fun m ->
                   let fixture_id =
                     m |> member "fixture" |> member "id" |> to_int
                   in
                   let home_team =
                     m |> member "teams" |> member "home" |> member "name"
                     |> to_string
                   in
                   let away_team =
                     m |> member "teams" |> member "away" |> member "name"
                     |> to_string
                   in
                   (fixture_id, home_team, away_team))
                 matches)
          with
          | Yojson.Json_error _ -> []
          | _ -> [])
      | _ -> Lwt.return [] )

(* Fetches the result of a specific match by fixture ID *)
let get_match_result ?(http_get = http_get) (fixture_id : int) : string =
  let uri =
    Uri.of_string
      (Printf.sprintf "https://v3.football.api-sports.io/fixtures?id=%d"
         fixture_id)
  in
  let headers =
    Header.init () |> fun h ->
    Header.add h "x-rapidapi-key" api_key |> fun h ->
    Header.add h "x-rapidapi-host" "v3.football.api-sports.io"
  in
  Lwt_main.run
    ( http_get uri headers >>= fun (resp, body) ->
      match resp.status with
      | `OK -> (
          body |> Cohttp_lwt.Body.to_string >|= fun body_str ->
          try
            let json = Yojson.Basic.from_string body_str in
            let match_info = json |> member "response" |> to_list |> List.hd in
            let home_team =
              match_info |> member "teams" |> member "home" |> member "name"
              |> to_string
            in
            let away_team =
              match_info |> member "teams" |> member "away" |> member "name"
              |> to_string
            in
            let status =
              match_info |> member "fixture" |> member "status"
              |> member "short" |> to_string
            in
            match status with
            | "FT" ->
                let home_score =
                  match_info |> member "goals" |> member "home" |> to_int
                in
                let away_score =
                  match_info |> member "goals" |> member "away" |> to_int
                in
                if home_score > away_score then home_team
                else if away_score > home_score then away_team
                else "Draw"
            | "NS" -> "Not Finished"
            | "CANC" -> "Cancelled"
            | _ -> "Unknown Status"
          with
          | Yojson.Json_error _ -> "Error parsing match result"
          | _ -> "Unexpected error")
      | _ -> Lwt.return "Error fetching match result" )

(* Fetches match winner odds for a given fixture ID *)
let get_match_winner_odds ?(http_get = http_get) (fixture_id : int) :
    (float * float * float) option =
  let uri =
    Uri.of_string
      (Printf.sprintf "https://v3.football.api-sports.io/odds?fixture=%d"
         fixture_id)
  in
  let headers =
    Header.init () |> fun h ->
    Header.add h "x-rapidapi-key" api_key |> fun h ->
    Header.add h "x-rapidapi-host" "v3.football.api-sports.io"
  in
  Lwt_main.run
    ( http_get uri headers >>= fun (resp, body) ->
      match resp.status with
      | `OK -> (
          body |> Cohttp_lwt.Body.to_string >|= fun body_str ->
          try
            let json = Yojson.Basic.from_string body_str in
            let odds_response = json |> member "response" |> to_list in
            if odds_response = [] then None
            else
              let bookmaker =
                odds_response |> List.hd |> member "bookmakers" |> to_list
                |> List.hd
              in
              let bets = bookmaker |> member "bets" |> to_list in
              match
                List.find_opt
                  (fun bet ->
                    bet |> member "name" |> to_string = "Match Winner")
                  bets
              with
              | None -> None
              | Some match_winner_bet ->
                  let values = match_winner_bet |> member "values" |> to_list in
                  let home_odd =
                    List.find
                      (fun v -> v |> member "value" |> to_string = "Home")
                      values
                    |> member "odd" |> to_string |> float_of_string
                  in
                  let draw_odd =
                    List.find
                      (fun v -> v |> member "value" |> to_string = "Draw")
                      values
                    |> member "odd" |> to_string |> float_of_string
                  in
                  let away_odd =
                    List.find
                      (fun v -> v |> member "value" |> to_string = "Away")
                      values
                    |> member "odd" |> to_string |> float_of_string
                  in
                  Some (home_odd, draw_odd, away_odd)
          with
          | Yojson.Json_error _ -> None
          | Failure _ -> None)
      | _ -> Lwt.return None )
