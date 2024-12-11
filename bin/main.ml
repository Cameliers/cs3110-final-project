open Final_project.Api_handling
open Final_project.Match
open Final_project.User
open Final_project.Bet
open Final_project.Profile
open Final_project.Lottery

(* [print_segementation] a helper function that prints the symbols [char] a
   given width [length], with the purpose of seperating menu option *)
let print_segmentation char length =
  for _ = 1 to length do
    print_string char
  done;
  print_newline ()

(* [ask_user] A helper function that prompts the user with the given message
   [msg] on what to do next. *)
let ask_user (msg : string) =
  let width = String.length msg + 20 in
  print_segmentation "=" width;
  print_string msg;
  read_line ()

(* [display_title] A helper function that displays the current menu's title
   [title] *)
let display_title title =
  let width = String.length title + 20 in
  print_segmentation "=" width;
  Printf.printf "|/| %s |/| \n" title;
  print_segmentation "=" width

(* [display_menu] a helper function that prints a series of menu options to pick
   from in the program loop. *)
let display_menu () =
  print_endline "(1) Show Balance";
  print_endline "(2) Show Upcoming Matches";
  print_endline "(3) Enter Active bets menu";
  print_endline "(4) Show Completed Bets History";
  print_endline "(5) Place A New Bet";
  print_endline "(6) Spin for BONUS cash!";
  print_endline "(7) Exit!"

let matches_string =
  get_upcoming_matches ()
  |> List.mapi (fun i (_, a, b) ->
         "(" ^ string_of_int (i + 1) ^ ") " ^ a ^ " vs " ^ b)
  |> String.concat "\n"

let matches_list =
  List.map
    (fun ((id : int), a, b) -> make_match id a b "null")
    (get_upcoming_matches ())

(* Prompt the user for a number with a cancel option *)
let rec prompt_number_with_cancel () =
  print_endline "Enter the number of the match (C to cancel):";
  match read_line () with
  | "C" | "c" -> `Cancel
  | input -> (
      try
        let number = int_of_string input in
        if number >= 1 && number <= 10 then `Value (number - 1)
        else (
          print_endline "Please choose a valid number from the list.";
          prompt_number_with_cancel ())
      with Failure _ ->
        print_endline "Invalid input. Please enter a number.";
        prompt_number_with_cancel ())

(* Prompt the user for a team with a cancel option *)
let rec prompt_team_with_cancel index =
  let match_data = List.nth matches_list index in
  print_endline "Enter the team name (C to cancel):";
  match read_line () with
  | "C" | "c" -> `Cancel
  | team ->
      if team = a_side match_data || team = b_side match_data then `Value team
      else (
        print_endline "Invalid team. Please choose a valid team.";
        prompt_team_with_cancel index)

(* Prompt the user for a betting amount with a cancel option *)
let rec prompt_amount_with_cancel user () =
  print_endline "Enter the betting amount (C to cancel):";
  match read_line () with
  | "C" | "c" -> `Cancel
  | input -> (
      try
        let amount = float_of_string input in
        `Value amount
      with Failure _ ->
        print_endline "Invalid input. Please enter a valid amount.";
        prompt_amount_with_cancel user ())

(* Prompt the user for an active bet index with a cancel option *)
let rec prompt_active_bet_index_with_cancel active_bets =
  print_endline "Enter the number of the bet you want to modify (C to cancel):";
  match read_line () with
  | "C" | "c" -> `Cancel
  | input -> (
      try
        let number = int_of_string input in
        if number >= 1 && number <= List.length active_bets then
          `Value (number - 1)
        else (
          print_endline "Please choose a valid bet number from the list.";
          prompt_active_bet_index_with_cancel active_bets)
      with Failure _ ->
        print_endline "Invalid input. Please enter a number.";
        prompt_active_bet_index_with_cancel active_bets)

(* Prompt the user for additional amount to increase a bet with cancel *)
let rec prompt_additional_amount_with_cancel user () =
  print_endline
    "Enter the additional amount you want to add to this bet (C to cancel):";
  match read_line () with
  | "C" | "c" -> `Cancel
  | input -> (
      try
        let amount = float_of_string input in
        if amount > 0. && amount <= balance user then `Value amount
        else (
          Printf.printf
            "Your balance is %.2f. Please enter an amount within your balance.\n"
            (balance user);
          prompt_additional_amount_with_cancel user ())
      with Failure _ ->
        print_endline "Invalid input. Please enter a valid amount.";
        prompt_additional_amount_with_cancel user ())

(* Function to handle the active bets menu *)
let rec active_bets_menu user active_bet_list =
  (* Display active bets *)
  let active_bet_string_list =
    List.mapi
      (fun i bet ->
        Printf.sprintf "(%d) %s vs %s: Bet $%.2f on %s" (i + 1)
          (a_side (bet_game bet))
          (b_side (bet_game bet))
          (bet_amount bet) (bet_team bet))
      active_bet_list
  in
  let final_string =
    if active_bet_string_list = [] then "\nCurrently no active bets placed."
    else String.concat "\n" active_bet_string_list
  in
  print_endline ("Current Active Bets:\n" ^ final_string);

  if active_bet_list = [] then (
    print_newline ();
    print_endline "Press ENTER to return to the main menu...";
    ignore (read_line ());
    `Return_to_main)
  else (
    print_newline ();
    print_endline "Active Bets Menu:";
    print_endline "(1) Cancel a bet";
    print_endline "(2) Increase bet amount";
    print_endline "(C) Return to main menu";
    match read_line () with
    | "C" | "c" ->
        print_endline "Returning to main menu.";
        `Return_to_main
    | "1" -> (
        match prompt_active_bet_index_with_cancel active_bet_list with
        | `Cancel ->
            print_endline "Action canceled. Returning to active bets menu.";
            active_bets_menu user active_bet_list
        | `Value bet_index ->
            let bet_to_cancel = List.nth active_bet_list bet_index in
            remove_bet user bet_to_cancel;
            print_endline "Bet successfully canceled!";
            `Refresh)
    | "2" -> (
        match prompt_active_bet_index_with_cancel active_bet_list with
        | `Cancel ->
            print_endline "Action canceled. Returning to active bets menu.";
            active_bets_menu user active_bet_list
        | `Value bet_index -> (
            let bet_to_increase = List.nth active_bet_list bet_index in
            print_endline
              (Printf.sprintf
                 "Current bet amount: $%.2f. How much do you want to add?"
                 (bet_amount bet_to_increase));
            match prompt_additional_amount_with_cancel user () with
            | `Cancel ->
                print_endline "Action canceled. Returning to active bets menu.";
                active_bets_menu user active_bet_list
            | `Value additional_amount ->
                (* Increase the bet amount. We assume we have a function like
                   `increase_bet` or we can re-implement it. If not, we can
                   remove the old bet and add a new one with the updated
                   amount. *)
                let new_amount =
                  bet_amount bet_to_increase +. additional_amount
                in
                remove_bet user bet_to_increase;
                add_bet user (bet_game bet_to_increase)
                  (bet_team bet_to_increase) new_amount;
                print_endline "Bet amount successfully increased!";
                `Refresh))
    | _ ->
        print_endline "Invalid choice. Returning to active bets menu.";
        active_bets_menu user active_bet_list)

(* [program_cycle] a Function that acts as the front/landing page of the
   program. *)
let rec program_cycle user () =
  display_title "Main Page";
  display_menu ();
  match ask_user "\nEnter Number: " with
  | "1" ->
      print_newline ();
      print_endline ("Current Balance: " ^ string_of_float (balance user));
      print_newline ();
      program_cycle user ()
  | "2" ->
      print_endline ("Current Matches:\n" ^ matches_string);
      program_cycle user ()
  | "3" -> (
      let active_bet_list = bets_active user in
      match active_bets_menu user active_bet_list with
      | `Return_to_main -> program_cycle user ()
      | `Refresh -> (
          (* After modifying bets, call active_bets_menu again to reflect
             changes *)
          match active_bets_menu user (bets_active user) with
          | `Return_to_main -> program_cycle user ()
          | `Refresh -> program_cycle user ()))
  | "4" ->
      let completed_bet_list = bets_history user in
      let completed_bet_string_list =
        List.map
          (fun bet ->
            a_side (bet_game bet)
            ^ " vs "
            ^ b_side (bet_game bet)
            ^ ": Bet $"
            ^ string_of_float (bet_amount bet)
            ^ " on " ^ bet_team bet)
          completed_bet_list
      in
      let new_string = String.concat "\n" completed_bet_string_list in
      let final_string =
        if completed_bet_string_list = [] then "No completed bets."
        else new_string
      in
      print_endline ("Completed Bet History:\n" ^ final_string);
      print_newline ();
      program_cycle user ()
  | "5" -> (
      print_endline "Choose a match from the matches.";
      match prompt_number_with_cancel () with
      | `Cancel ->
          print_endline "Action canceled. Returning to the main menu.";
          program_cycle user ()
      | `Value index -> (
          match prompt_team_with_cancel index with
          | `Cancel ->
              print_endline "Action canceled. Returning to the main menu.";
              program_cycle user ()
          | `Value team -> (
              let amount = prompt_amount_with_cancel user () in
              match amount with
              | `Cancel ->
                  print_endline "Action canceled. Returning to the main menu.";
                  program_cycle user ()
              | `Value amount -> (
                  try
                    add_bet user (List.nth matches_list index) team amount;
                    print_endline "Bet successfully placed!"
                  with Insufficient_Balance ->
                    Printf.printf
                      "Your balance is %.2f. Please enter an amount within \
                       your balance.\n"
                      (balance user);
                    program_cycle user ()))))
  | "6" ->
    let bonus = spin_lottery () in
    let new_balance = balance user +. bonus in

    (* Print the spinner and bonus value dynamically *)
    for i = 1 to 3 do
      let str = String.make i '.' in
      Printf.printf "\rSpinning%s" str; (* Print spinner *)
      flush stdout;                    (* Flush to force immediate printing *)
      Unix.sleep 1;                    (* Sleep for 1 second *)
    done;
    (* After each animation frame, print the bonus and new balance *)
    Printf.printf "\rBonus: $%.2f" bonus; 
    flush stdout;                    (* Flush to force immediate printing *)
    Unix.sleep 1;                    (* Sleep for 1 second *)
    
    Printf.printf "\rNew Balance: $%.2f" new_balance;
    flush stdout;                    (* Flush to force immediate printing *)
    Unix.sleep 1;                    (* Sleep for 1 second *)

    (* Print the final result after the spinner loop ends *)
    Printf.printf "\rLottery spin result: $%.2f\nYour new balance: $%.2f\n"
      bonus new_balance;

    (* Update the balance and continue the program cycle *)
    change_balance user bonus;
    program_cycle user ()
  | "7" ->
      display_title "Goodbye & Good Luck!";
      save_to_file "./data/user_profile.txt" user;
      exit 0
  | _ ->
      display_title
        "Invalid Choice, please enter number *no parenthesis required*.";
      program_cycle user ()

(* Entry point *)
let () =
  print_segmentation "*" 30;
  print_endline "* Welcome to Cameliers Sports Betting Center *";
  print_segmentation "*" 30;
  let user =
    let filename = "./data/user_profile.txt" in
    if Sys.file_exists filename then
      load_from_file filename (* Load user profile from file *)
    else make_user () (* Create a new user if the file does not exist *)
  in
  program_cycle user ()
