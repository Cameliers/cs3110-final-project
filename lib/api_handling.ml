open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Unix

let api_key = "cb5a2c14976a9dd0786cb9d59957702e"

(* Helper function to format Unix timestamp to YYYY-MM-DD *)
let format_date timestamp =
  let tm = Unix.gmtime timestamp in
  Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

(* Fetches a list of upcoming matches for today *)
let get_upcoming_matches () : (int * string * string) list =
  let today = Unix.time () in
  let date = format_date today in

  (* Use the `date` parameter to fetch today's matches *)
  let uri =
    Uri.of_string
      (Printf.sprintf "https://v3.football.api-sports.io/fixtures?date=%s" date)
  in
  let headers =
    Header.init () |> fun h ->
    Header.add h "x-rapidapi-key" api_key |> fun h ->
    Header.add h "x-rapidapi-host" "v3.football.api-sports.io"
  in
  Lwt_main.run
    ( Client.call ~headers `GET uri >>= fun (resp, body) ->
      match resp.status with
      | `OK -> (
          body |> Cohttp_lwt.Body.to_string >|= fun body_str ->
          try
            let json = Yojson.Basic.from_string body_str in
            let matches = json |> member "response" |> to_list in
            (* Limit to 10 matches *)
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
let get_match_result (fixture_id : int) : string =
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
    ( Client.call ~headers `GET uri >>= fun (resp, body) ->
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
