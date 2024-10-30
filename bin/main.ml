open Final_project.Api_testing
open Final_project.Match
open Final_project.User
open Final_project.Bet

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
  |> List.mapi (fun i (a, b) -> "(" ^ string_of_int i ^ ") " ^ a ^ " vs " ^ b)
  |> String.concat "\n"

let matches_list =
  List.map (fun (a, b) -> make_match a b "null") (get_upcoming_matches ())

let user = make_user ()

(* [program_cycle] a Function that acts as the front/landing page of the
   program.*)
let rec program_cycle () =
  display_title "Main Page";
  display_menu ();
  match ask_user "\nEnter Number: " with
  | "1" ->
      print_endline ("Current Balance: " ^ string_of_float (balance user));
      program_cycle ()
  | "2" ->
      print_endline ("Current Matches:\n" ^ matches_string);
      program_cycle ()
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
      let final_string = String.concat "\n" bet_string_list in
      print_endline ("Current Bet History:\n" ^ final_string);
      program_cycle ()
  | "4" ->
      print_endline "Choose a match from the matches.";
      let user_input = read_line () in
      let index = int_of_string user_input in
      print_endline "Choose a team";
      let team = read_line () in
      print_endline "Choose an amount";
      let amount = float_of_string (read_line ()) in
      add_bet user (List.nth matches_list index) team amount;
      print_endline "added bet!";
      program_cycle ()
  | "5" ->
      display_title "Goodbye & Goodluck";
      exit 0
  | _ ->
      display_title
        "Invalid Choice, please enter number *no paranthesis required*.";
      program_cycle ()

(* Entry point *)
let () =
  print_segmentation "*" 30;
  print_endline "* Welcome to Cameliers Sports Betting Center*";
  print_segmentation "*" 30;
  program_cycle ()
