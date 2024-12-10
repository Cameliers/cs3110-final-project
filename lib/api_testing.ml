open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Unix

let api_key = "eb26be46ece5ac441a9aa8ae04daa2ff"

(* Helper function to format Unix timestamp to YYYY-MM-DD *)
let format_date timestamp =
  let tm = Unix.gmtime timestamp in
  Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

let get_upcoming_matches () =
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
                   let home_team =
                     m |> member "teams" |> member "home" |> member "name"
                     |> to_string
                   in
                   let away_team =
                     m |> member "teams" |> member "away" |> member "name"
                     |> to_string
                   in
                   (home_team, away_team))
                 matches)
          with
          | Yojson.Json_error _ -> []
          | _ -> [])
      | _ -> Lwt.return [] )
(* Ensure this branch returns a (string * string) list *)
