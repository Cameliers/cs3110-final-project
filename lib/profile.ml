(* Helper to convert a list of bets to a string in the format: [ Bet1; Bet2; ...
   ] *)
let bets_to_string (bets : Bet.t list) : string =
  let bet_strings = List.map Bet.to_string bets in
  "[" ^ String.concat ";" bet_strings ^ "]" (* Adding the square brackets *)

(* Helper to convert a string in the format: [ Bet1; Bet2; ... ] back to a list
   of bets *)
let string_to_bets (str : string) : Bet.t list =
  if str = "[]" then [] (* Handle the case where there are no bets *)
  else
    (* Remove the surrounding brackets and split by "; " to get individual
       bets *)
    let stripped_str = String.sub str 1 (String.length str - 2) in
    let bet_strings = String.split_on_char ';' stripped_str in
    List.map Bet.of_string bet_strings

let balance_history_to_string (floats, f) =
  let float_list_str =
    "[" ^ String.concat "; " (List.map string_of_float floats) ^ "]"
  in
  let float_str = string_of_float f in
  "(" ^ float_list_str ^ ", " ^ float_str ^ ")"

let string_to_balance_history s =
  let s = String.trim s in
  if String.length s < 2 || s.[0] <> '(' || s.[String.length s - 1] <> ')' then
    failwith "Invalid format: no outer parentheses";
  let inner = String.sub s 1 (String.length s - 2) in
  let inner = String.trim inner in
  let comma_index =
    try String.index inner ','
    with Not_found -> failwith "Invalid format: no comma found"
  in
  let list_part = String.sub inner 0 comma_index in
  let float_part =
    String.sub inner (comma_index + 1) (String.length inner - comma_index - 1)
  in
  let list_part = String.trim list_part in
  let float_part = String.trim float_part in
  if
    String.length list_part < 2
    || list_part.[0] <> '['
    || list_part.[String.length list_part - 1] <> ']'
  then failwith "Invalid format: list not enclosed in brackets";
  let list_inner = String.sub list_part 1 (String.length list_part - 2) in
  let list_inner = String.trim list_inner in
  let float_strings =
    if list_inner = "" then [] else String.split_on_char ';' list_inner
  in
  let float_strings = List.map String.trim float_strings in
  let float_list =
    try List.map float_of_string float_strings
    with Failure _ -> failwith "Invalid float in list"
  in
  let f =
    try float_of_string float_part
    with Failure _ -> failwith "Invalid float after comma"
  in
  (float_list, f)

(* Save the user's profile to a file *)
let save_to_file (filename : string) (user : User.t) : unit =
  let balance = string_of_float (User.balance user) in
  let active_bets = bets_to_string (User.bets_active user) in
  let bet_history = bets_to_string (User.bets_history user) in
  let balance_history = balance_history_to_string (User.balance_history user) in
  let content =
    Printf.sprintf "%s\n%s\n%s\n%s" balance active_bets bet_history
      balance_history
  in
  let oc = open_out filename in
  output_string oc content;
  close_out oc

(* Takes in a string [filename] that is the file where the user profile is
   stored and returns a User *)
let load_from_file (filename : string) : User.t =
  let ic = open_in filename in
  let balance_line = input_line ic in
  let active_bets_line = input_line ic in
  let bet_history_line = input_line ic in
  let balance_history_line = input_line ic in
  close_in ic;

  let balance = float_of_string balance_line in
  let active_bets = string_to_bets active_bets_line in
  let bet_history = string_to_bets bet_history_line in
  let balance_history = string_to_balance_history balance_history_line in

  User.create balance active_bets bet_history balance_history
