(* Helper to convert a list of bets to a string *)
let bets_to_string (bets : Bet.t list) : string =
  bets
  |> List.map Bet.to_string
     (* Assumes there is a [to_string] function in the Bet module *)
  |> String.concat ";"

(* Helper to convert a string to a list of bets *)
let string_to_bets (str : string) : Bet.t list =
  if str = "" then []
  else str 
    |> String.split_on_char ';'
    |> List.map Bet.of_string

(* Save the user's profile to a file *)
let save_to_file (filename : string) (user : User.t) : unit =
  let balance = string_of_float (User.balance user) in
  let active_bets = bets_to_string (User.bets_active user) in
  let bet_history = bets_to_string (User.bets_history user) in
  let content = Printf.sprintf "%s\n%s\n%s" balance active_bets bet_history in
  let oc = open_out filename in
  output_string oc content;
  close_out oc

(* Load the user's profile from a file *)
let load_from_file (filename : string) : User.t =
  let ic = open_in filename in

  (* Read each line of the file *)
  let balance = float_of_string (input_line ic) in
  let active_bets = string_to_bets (input_line ic) in
  let bet_history = string_to_bets (input_line ic) in
  close_in ic;

  (* Create a new user and populate its fields *)
  let user = User.make_user () in
  User.change_balance user balance;

  (* Add each active bet back to the user's profile *)
  List.iter
    (fun bet ->
      User.add_bet user (Bet.bet_game bet) (Bet.bet_team bet)
        (Bet.bet_amount bet))
    active_bets;

  (* Assuming the User module has a function to add a bet directly to the
     history, you may need a different approach if this is not allowed *)
  List.iter
    (fun bet ->
      User.add_bet user (Bet.bet_game bet) (Bet.bet_team bet)
        (Bet.bet_amount bet))
    bet_history;
  user
