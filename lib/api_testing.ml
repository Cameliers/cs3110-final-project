open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util

let api_key = "eb26be46ece5ac441a9aa8ae04daa2ff"

let get_upcoming_matches () =
  let uri =
    Uri.of_string "https://v3.football.api-sports.io/fixtures?next=10"
  in
  let headers =
    Header.init () |> fun h ->
    Header.add h "x-rapidapi-key" api_key |> fun h ->
    Header.add h "x-rapidapi-host" "v3.football.api-sports.io"
  in
  Lwt_main.run
    ( Client.call ~headers `GET uri >>= fun (resp, body) ->
      match resp.status with
      | `OK ->
          body |> Cohttp_lwt.Body.to_string >|= fun body_str ->
          let json = Yojson.Basic.from_string body_str in
          let matches = json |> member "response" |> to_list in
          List.map
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
            matches
      | _ -> Lwt.return [] )
