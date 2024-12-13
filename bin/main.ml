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
  print_endline "(1) Show Current Balance & Balance History";
  print_endline "(2) Show Upcoming Matches";
  print_endline "(3) Enter Active bets menu";
  print_endline "(4) Show Completed Bets History";
  print_endline "(5) Place A New Bet";
  print_endline "(6) Spin for BONUS cash!";
  print_endline "(7) Exit!"

(* [display_betting_menu] A helper function meant to display a betting menu for
   a user to choose the amount the want to spend on their bet. *)
let display_betting_menu () =
  print_newline ();
  print_endline "(1) Place 5.00$ Bet.";
  print_endline "(2) Place 10.00$ Bet.";
  print_endline "(3) Place 15.00$ Bet.";
  print_endline "(4) Place 20.00$ Bet.";
  print_endline "(5) Up the stakes!";
  print_endline "(6) Custom amount"

(* [display_higher_stakes_menu] A helper function meant to display a higher
   stakes betting menu for a user to choose the amount the want to spend on
   their bet. *)
let display_higher_stakes_menu () =
  print_newline ();
  print_endline "(1) Place 50.00$ Bet.";
  print_endline "(2) Place 100.00$ Bet.";
  print_endline "(3) Place 150.00$ Bet.";
  print_endline "(4) Place 200.00$ Bet.";
  print_endline "(5) Custom amount"

(* Helper function to choose a bet amount using the new menus *)
let rec choose_bet_amount user () =
  (* Display the main betting menu *)
  display_betting_menu ();
  print_endline "\nEnter your choice (C to cancel): ";
  match read_line () with
  | "C" | "c" -> `Cancel
  | "1" -> `Value 5.00
  | "2" -> `Value 10.00
  | "3" -> `Value 15.00
  | "4" -> `Value 20.00
  | "5" -> (
      (* Higher stakes menu *)
      display_higher_stakes_menu ();
      print_endline "\nEnter your choice (C to cancel): ";
      match read_line () with
      | "C" | "c" -> `Cancel
      | "1" -> `Value 50.00
      | "2" -> `Value 100.00
      | "3" -> `Value 150.00
      | "4" -> `Value 200.00
      | "5" -> (
          (* Prompt custom amount *)
          print_endline "\nEnter custom amount (C to cancel): ";
          match read_line () with
          | "C" | "c" -> `Cancel
          | input -> (
              try
                let amount = float_of_string input in
                if amount > 0. && amount <= balance user then `Value amount
                else (
                  print_newline ();
                  Printf.printf
                    "Your balance is %.2f. Please enter an amount within your \
                     balance.\n"
                    (balance user);
                  print_newline ();
                  choose_bet_amount user ())
              with Failure _ ->
                print_newline ();
                display_title "Invalid input. Please enter a valid amount.";
                print_newline ();
                choose_bet_amount user ()))
      | _ ->
          print_newline ();
          display_title "Invalid choice.";
          print_newline ();
          choose_bet_amount user ())
  | "6" -> (
      (* Prompt custom amount *)
      print_endline "\nEnter custom amount (C to cancel): ";
      match read_line () with
      | "C" | "c" -> `Cancel
      | input -> (
          try
            let amount = float_of_string input in
            if amount > 0. && amount <= balance user then `Value amount
            else (
              print_newline ();
              Printf.printf
                "Your balance is %.2f. Please enter an amount within your \
                 balance.\n"
                (balance user);
              print_newline ();
              choose_bet_amount user ())
          with Failure _ ->
            print_newline ();
            display_title "Invalid input. Please enter a valid amount.";
            print_newline ();
            choose_bet_amount user ()))
  | _ ->
      print_newline ();
      display_title "Invalid choice.";
      print_newline ();
      choose_bet_amount user ()

(* [matches_list] A helper val that gets the matches from the API, and
   represents them in List form.*)
let matches_list =
  List.map
    (fun ((id : int), a, b) -> make_match id a b)
    (get_upcoming_matches ())

(* [matches_string] A helper val that displays the upcoming matches list into
   string form.*)
let matches_string =
  matches_list
  |> List.mapi (fun i m ->
         "("
         ^ string_of_int (i + 1)
         ^ ") " ^ a_side m ^ " vs " ^ b_side m ^ " Odds: " ^ match_odds m)
  |> String.concat "\n"

(* [balance_history_to_string] A helper function to convert balance history into
   a nicely formatted string. *)
let balance_history_to_string (balances, net) =
  let balances_str =
    match balances with
    | [] -> "No historical balances recorded."
    | _ ->
        let balances_list_str =
          balances |> List.map (fun b -> Printf.sprintf "$%.2f" b)
        in
        "Balances Over Time: " ^ String.concat " -> " balances_list_str
  in
  let net_str =
    if net > 0. then Printf.sprintf "Net Gain: +$%.2f" net
    else if net < 0. then Printf.sprintf "Net Loss: -$%.2f" (abs_float net)
    else "No net gain or loss."
  in
  balances_str ^ "\n" ^ net_str

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
          print_newline ();
          display_title "Please choose a valid number from the list.";
          print_newline ();
          prompt_number_with_cancel ())
      with Failure _ ->
        print_newline ();
        display_title "Invalid input. Please enter a number.";
        print_newline ();
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
        print_newline ();
        display_title "Invalid team. Please choose a valid team.";
        print_newline ();
        prompt_team_with_cancel index)

(* Prompt the user for an active bet index with a cancel option *)
let rec prompt_active_bet_index_with_cancel active_bets =
  print_endline
    "\nEnter the number of the bet you want to modify (C to cancel): ";
  match read_line () with
  | "C" | "c" -> `Cancel
  | input -> (
      try
        let number = int_of_string input in
        if number >= 1 && number <= List.length active_bets then
          `Value (number - 1)
        else (
          print_newline ();
          display_title "Please choose a valid bet number from the list.";
          print_newline ();
          prompt_active_bet_index_with_cancel active_bets)
      with Failure _ ->
        print_newline ();
        display_title "Invalid input. Please enter a number.";
        print_newline ();
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
          print_newline ();
          Printf.printf
            "Your balance is %.2f. Please enter an amount within your balance.\n"
            (balance user);
          print_newline ();
          prompt_additional_amount_with_cancel user ())
      with Failure _ ->
        print_newline ();
        display_title "Invalid input. Please enter a valid amount.";
        print_newline ();
        prompt_additional_amount_with_cancel user ())

(* [active_bets_menu] A helper function that displays the 'active bets' menu.
   [user] the user data. [active_bet_list] list of all ongoing/active bets. *)
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
    if active_bet_string_list = [] then "Currently no active bets placed."
    else String.concat "\n" active_bet_string_list
  in
  print_endline ("Current Active Bets: " ^ final_string);

  if active_bet_list = [] then (
    print_newline ();
    print_endline "Press ENTER to return to the main menu...";
    ignore (read_line ());
    `Return_to_main)
  else (
    print_newline ();
    display_title " Active bets menu; ";
    print_endline "(1) Cancel a bet";
    print_endline "(2) Increase bet amount";
    print_endline "(3) Return to main menu";

    match ask_user "\nEnter number: " with
    | "3" ->
        print_newline ();
        display_title "Returning to main menu.";
        print_newline ();
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
            print_newline ();
            display_title
              (Printf.sprintf
                 "Current bet amount: $%.2f. How much do you want to add?"
                 (bet_amount bet_to_increase));
            print_newline ();
            match prompt_additional_amount_with_cancel user () with
            | `Cancel ->
                print_endline "Action canceled. Returning to active bets menu.";
                active_bets_menu user active_bet_list
            | `Value additional_amount ->
                modify_bet user bet_to_increase additional_amount;
                print_endline "Bet amount successfully increased!";
                `Refresh))
    | _ ->
        print_newline ();
        display_title "Invalid choice. Returning to active bets menu.";
        print_newline ();
        active_bets_menu user active_bet_list)

(* [program_cycle] a Function that acts as the front/landing page of the
   program. *)
let rec program_cycle user () =
  display_title "Main Page";
  display_menu ();
  match ask_user "\nEnter Number: " with
  | "1" ->
      print_newline ();
      (* Retrieve the balance history and print it *)
      let bh = balance_history user in
      display_title "Current Balance History:";
      print_newline ();
      print_endline (balance_history_to_string bh);
      print_newline ();
      (* Print the current balance as well *)
      display_title ("Current Balance: " ^ string_of_float (balance user) ^ " $");
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
      print_newline ();
      print_endline ("Completed Bet History:\n\n" ^ final_string);
      print_newline ();
      program_cycle user ()
  | "5" -> (
      print_endline "\nChoose a match from the matches.";
      match prompt_number_with_cancel () with
      | `Cancel ->
          print_newline ();
          display_title "Action canceled. Returning to the main menu.";
          print_newline ();
          program_cycle user ()
      | `Value index -> (
          match prompt_team_with_cancel index with
          | `Cancel ->
              print_newline ();
              display_title "Action canceled. Returning to the main menu.";
              print_newline ();
              program_cycle user ()
          | `Value team -> (
              (* Use the new choose_bet_amount function *)
              match choose_bet_amount user () with
              | `Cancel ->
                  print_newline ();
                  display_title "Action canceled. Returning to the main menu.";
                  print_newline ();
                  program_cycle user ()
              | `Value amount -> (
                  try
                    add_bet user (List.nth matches_list index) team amount;
                    print_newline ();
                    display_title "Bet successfully placed!";
                    print_newline ();
                    program_cycle user ()
                  with Insufficient_Balance ->
                    Printf.printf
                      "\n\
                       Your balance is %.2f. Please enter an amount within \
                       your balance.\n"
                      (balance user);
                    program_cycle user ()))))
  | "6" ->
      let bonus = spin_lottery () in
      let new_balance = balance user +. bonus in
      print_newline ();
      display_title "Commence Spinning";
      print_newline ();
      for i = 1 to 3 do
        let str = String.make i '.' in
        Printf.printf "\rSpinning%s" str;
        flush stdout;
        Unix.sleep 1
      done;

      Printf.printf "\rBonus: $%.2f\n" bonus;
      flush stdout;
      Unix.sleep 1;

      Printf.printf "\r\nNew Balance: $%.2f\n" new_balance;
      flush stdout;
      Unix.sleep 1;

      print_newline ();
      Printf.printf "\rLottery spin result: $%.2f\n\nYour new balance: $%.2f\n"
        bonus new_balance;
      print_newline ();

      change_balance user bonus;
      program_cycle user ()
  | "7" ->
      print_newline ();
      display_title "Goodbye & Good Luck!";
      print_newline ();
      save_to_file "./data/user_profile.txt" user;
      exit 0
  | _ ->
      print_newline ();
      display_title
        "Invalid Choice, please enter number *no parenthesis required*.";
      print_newline ();
      program_cycle user ()

let pause_and_continue () =
  print_endline "\nPress ENTER to continue...";
  let rec wait_for_enter () =
    match read_line () with
    | "" -> () (* Empty string means Enter key was pressed *)
    | _ ->
        print_endline "Please press ENTER to continue...";
        wait_for_enter ()
  in
  wait_for_enter ()

let display_tutorial () =
  (* Tutorial sections *)
  print_endline "Tutorial: Getting Started with Sands of Chance";
  pause_and_continue ();

  (* App Overview *)
  display_title "App Overview";
  print_endline
    "Sands of Chance is a soccer match betting application where you \
     can:";
  print_endline "- View upcoming soccer matches";
  print_endline "- Place bets on match outcomes";
  print_endline "- Manage your active and completed bets";
  print_endline "- Spin for bonus cash";
  pause_and_continue ();

  (* Initial Balance *)
  display_title "Starting Balance";
  print_endline
    "When you start, you'll receive an initial balance of $1,000.00.";
  print_endline "Use this balance wisely to place bets on soccer matches!";
  pause_and_continue ();

  (* How to Place a Bet *)
  display_title "How to Place a Bet";
  print_endline "To place a bet:";
  print_endline
    "1. Select 'Show Upcoming Matches' to see available soccer matches";
  print_endline "2. Choose a match by its number";
  print_endline "3. Select which team you want to bet on";
  print_endline "4. Enter the amount you want to bet";
  print_endline "Note: You can only bet an amount within your current balance";
  pause_and_continue ();

  (* Managing Bets *)
  display_title "Managing Your Bets";
  print_endline "In the 'Active bets menu', you can:";
  print_endline "- Cancel an active bet before the match starts";
  print_endline "- Increase the amount of an existing bet";
  print_endline "- View your bet history";
  pause_and_continue ();

  (* Lottery Bonus *)
  display_title "Bonus Cash Spinner";
  print_endline "Feeling lucky? Use the 'Spin for BONUS cash!' option.";
  print_endline
    "This feature lets you potentially win extra money to add to your balance.";
  pause_and_continue ();

  (* Betting Tips *)
  display_title "Betting Tips";
  print_endline "- Only bet what you can afford to lose";
  print_endline "- Check match details carefully before placing a bet";
  print_endline "- Keep track of your bet history";
  print_endline "- Remember, betting should be fun, not a financial strategy";
  pause_and_continue ();

  (* Final Welcome *)
  print_segmentation "*" 30;
  print_endline "* Ready to start your betting adventure? *";
  print_endline "* Let's create your profile! *";
  print_segmentation "*" 30;
  print_newline ()

let startup user () =
  complete_bets user;
  program_cycle user ()

(* Main entry *)
let () =
  print_segmentation "*" 30;
  print_endline "* Welcome to Sands of Chance *";
  print_segmentation "*" 30;
  let filename = "./data/user_profile.txt" in
  if not (Sys.file_exists filename) then display_tutorial ();
  let user =
    let filename = "./data/user_profile.txt" in
    if Sys.file_exists filename then
      load_from_file filename (* Load user profile from file *)
    else make_user () (* Create a new user if the file does not exist *)
  in
  startup user ()
