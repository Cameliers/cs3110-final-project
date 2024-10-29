open Final_project.Demo

(* [ask_user] A helper function that prompts the user with the given message
   [msg] on what to do next.*)
let ask_user (msg : string) =
  print_string msg;
  read_line ()
