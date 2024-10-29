open Graphics

let demo () = 
  (* Initialize the graphics system *)
  let _ = open_graph " 400x200" in

  (* Set the background color *)
  set_window_title "Simple Menu";
  set_color white;
  fill_rect 0 0 400 200;

  (* Set text properties *)
  set_color black;
  set_text_size 20;

  (* Draw the menu options *)
  moveto 50 150; 
  draw_string "1. Portfolio";
  
  moveto 50 100; 
  draw_string "2. Active Bets";
  
  moveto 50 50; 
  draw_string "3. Search";

  (* Wait for a key press before closing *)
  let _ = read_key () in
  close_graph ()
