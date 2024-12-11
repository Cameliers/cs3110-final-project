(* Helper to convert a list of bets to a string in the format: [ Bet1; Bet2; ... ] *)
let bets_to_string (bets : Bet.t list) : string =
  let bet_strings = List.map Bet.to_string bets in
  "[" ^ String.concat "; " bet_strings ^ "]"  (* Adding the square brackets *)

(* Helper to convert a string in the format: [ Bet1; Bet2; ... ] back to a list of bets *)
let string_to_bets (str : string) : Bet.t list =
  if str = "[]" then []  (* Handle the case where there are no bets *)
  else
    (* Remove the surrounding brackets and split by "; " to get individual bets *)
    let stripped_str = String.sub str 1 (String.length str - 2) in
    let bet_strings = String.split_on_char ';' stripped_str in
    List.map Bet.of_string bet_strings

(* Save the user's profile to a file *)
let save_to_file (filename : string) (user : User.t) : unit =
  let balance = string_of_float (User.balance user) in
  let active_bets = bets_to_string (User.bets_active user) in
  let bet_history = bets_to_string (User.bets_history user) in
  let content = Printf.sprintf "%s\n%s\n%s" balance active_bets bet_history in
  let oc = open_out filename in
  output_string oc content;
  close_out oc

(* Takes in a string [filename] that is the file where the user profile is stored and returns a User *)
let load_from_file (filename : string) : User.t =
  let ic = open_in filename in
  let balance_line = input_line ic in
  let active_bets_line = input_line ic in
  let bet_history_line = input_line ic in
  close_in ic;

  let balance = float_of_string balance_line in
  let active_bets = string_to_bets active_bets_line in
  let bet_history = string_to_bets bet_history_line in

  User.create balance active_bets bet_history
