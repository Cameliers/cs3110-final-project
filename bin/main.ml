(* [print_segementation] a helper function that prints the symbols [char] a
   given width [length], with the purpose of seperating menu option*)
let print_segementation char length =
  for _ = 1 to length do
    print_string char
  done;
  print_newline ()

(* [ask_user] A helper function that prompts the user with the given message
   [msg] on what to do next.*)
let ask_user (msg : string) =
  let width = String.length msg + 20 in
  print_segementation "=" width;
  print_string msg;
  read_line ()

(* [display_title] A helper function that displays the current menu's title
   [title] *)
let display_title title =
  let width = String.length title + 20 in
  print_segementation "=" width;
  Printf.printf "|/| %s |/| \n" title;
  print_segementation "=" width

let display_menu () =
  print_endline "(1) Show Balance";
  print_endline "(2) Show AutoBets";
  print_endline "(3) Show Betting History";
  print_endline "(4) Place A New Bet";
  print_endline "(5) Exit!"

let rec program_cycle () =
  display_title "Main Page";
  display_menu ();
  match ask_user "\nEnter Number: " with
  | "1" ->
      print_endline "Current Balance: 99999999999999999999999 \n";
      program_cycle ()
  | "2" ->
      print_endline "Current AutoBets placed: None. \n";
      program_cycle ()
  | "3" ->
      print_endline "Current Bet History: No Bets placed yet. \n";
      program_cycle ()
  | "4" -> failwith "todo"
  | "5" ->
      display_title "Goodbye & Goodluck";
      exit 0
  | _ ->
      display_title
        "Invalid Choice, please enter number *no paranthesis required*.";
      program_cycle ()

(* Entry point *)
let () =
  print_segementation "*" 30;
  print_endline "* Welcome to Cameliers Sports Betting Center*";
  print_segementation "*" 30;
  program_cycle ()
