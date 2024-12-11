open Final_project.Api_testing
open Final_project.Match
open Final_project.User
open Final_project.Bet
open Final_project.Profile

(* [print_segementation] a helper function that prints the symbols [char] a
   given width [length], with the purpose of seperating menu option*)
let print_segmentation char length =
  for _ = 1 to length do
    print_string char
  done;
  print_newline ()

(* [ask_user] A helper function that prompts the user with the given message
   [msg] on what to do next.*)
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
   from in the program loop.*)
let display_menu () =
  print_endline "(1) Show Balance";
  print_endline "(2) Show Upcoming Matches";
  print_endline "(3) Show Betting History";
  print_endline "(4) Place A New Bet";
  print_endline "(5) Exit!"

let matches_string =
  get_upcoming_matches ()
  |> List.mapi (fun i (_, a, b) ->
         "(" ^ string_of_int (i + 1) ^ ") " ^ a ^ " vs " ^ b)
  |> String.concat "\n"

let matches_list =
  List.map
    (fun ((id : int), a, b) -> make_match id a b "null")
    (get_upcoming_matches ())

let rec prompt_number () =
  let number = read_line () in
  match number with
  | "1" -> 0
  | "2" -> 1
  | "3" -> 2
  | "4" -> 3
  | "5" -> 4
  | "6" -> 5
  | "7" -> 6
  | "8" -> 7
  | "9" -> 8
  | "10" -> 9
  | _ ->
      print_endline "Please choose a match from the list of 10.";
      prompt_number ()

let rec prompt_team (index : int) () =
  let team = read_line () in
  if
    team = a_side (List.nth matches_list index)
    || team = b_side (List.nth matches_list index)
  then team
  else (
    print_endline "Please choose a team.";
    prompt_team index ())

let rec prompt_amount user () =
  let amount = read_line () in
  if float_of_string amount <= balance user then float_of_string amount
  else (
    print_endline
      ("Your balance is "
      ^ string_of_float (balance user)
      ^ ", please choose an amount less than or equal to your balance.");
    prompt_amount user ())

(* [program_cycle] a Function that acts as the front/landing page of the
   program.*)
let rec program_cycle user () =
  display_title "Main Page";
  display_menu ();
  match ask_user "\nEnter Number: " with
  | "1" ->
      print_endline ("Current Balance: " ^ string_of_float (balance user));
      program_cycle user ()
  | "2" ->
      print_endline ("Current Matches:\n" ^ matches_string);
      program_cycle user ()
  | "3" ->
      let bet_list = bets_active user in
      let bet_string_list =
        List.map
          (fun bet ->
            a_side (bet_game bet)
            ^ " vs "
            ^ b_side (bet_game bet)
            ^ ": Bet $"
            ^ string_of_float (bet_amount bet)
            ^ " on " ^ bet_team bet)
          bet_list
      in
      let new_string = String.concat "\n" bet_string_list in
      let final_string =
        if bet_string_list = [] then "No bet history." else new_string
      in
      print_endline ("Current Bet History:\n" ^ final_string);
      program_cycle user ()
  | "4" ->
      print_endline "Choose a match from the matches.";
      let index = prompt_number () in
      print_endline "Choose a team.";
      let team = prompt_team index () in
      print_endline "Choose an amount.";
      let amount = prompt_amount user () in
      add_bet user (List.nth matches_list index) team amount;
      print_endline "Added bet!";
      program_cycle user ()
  | "5" ->
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
  print_endline "* Welcome to Cameliers Sports Betting Center*";
  print_segmentation "*" 30;
  let user =
    let filename = "./data/user_profile.txt" in
    if Sys.file_exists filename then
      load_from_file filename (* Load user profile from file *)
    else make_user () (* Create a new user if the file does not exist *)
  in
  program_cycle user ()
