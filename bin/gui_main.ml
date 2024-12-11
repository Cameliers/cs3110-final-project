(* bin/gui_main.ml *)

(* Open only necessary modules once at the top *)
open GMain (* Main.init, Main.main *)
open GWindow (* window *)
open GPack (* vbox, hbox, notebook *)
open GMisc (* label *)
open GButton (* button *)
open GText (* view *)
open GEdit (* combo_box_text, entry *)
open Final_project.Api_handling
open Final_project.Match
open Final_project.User
open Final_project.Bet
open Final_project.Profile

let () =
  (* Initialize GTK *)
  let _ = Main.init () in

  (* Load or create user profile *)
  let filename = "./data/user_profile.txt" in
  let user =
    if Sys.file_exists filename then load_from_file filename else make_user ()
  in

  (* Create main window *)
  let window =
    window ~title:"Cameliers Sports Betting GUI" ~border_width:10 ()
  in

  (* Connect the destroy signal to save data and quit GTK *)
  let _ =
    window#connect#destroy ~callback:(fun () ->
        save_to_file filename user;
        Main.quit ())
  in

  (* Create a notebook for tabs *)
  let notebook =
    notebook ~tab_pos:`TOP ~show_tabs:true ~show_border:true ~packing:window#add
      ()
  in

  (* ========== BALANCE TAB ========== *)
  let balance_vbox =
    vbox ~spacing:10
      ~packing:(fun w ->
        let lbl = label ~text:"Balance" () in
        ignore (notebook#append_page ~tab_label:lbl#coerce w#coerce))
      ()
  in

  let balance_label = label ~text:"" ~packing:balance_vbox#add () in

  let update_balance () =
    balance_label#set_text
      (Printf.sprintf "Current Balance: %.2f" (balance user))
  in

  (* Initial balance update *)
  update_balance ();

  (* Refresh Balance Button *)
  let refresh_balance_button =
    button ~label:"Refresh Balance" ~packing:balance_vbox#add ()
  in
  let _ = refresh_balance_button#connect#clicked ~callback:update_balance in

  (* ========== MATCHES TAB ========== *)
  let matches_vbox =
    vbox ~spacing:10
      ~packing:(fun w ->
        let lbl = label ~text:"Matches" () in
        ignore (notebook#append_page ~tab_label:lbl#coerce w#coerce))
      ()
  in

  let matches_view = view ~packing:matches_vbox#add () in

  let update_matches () =
    let matches = get_upcoming_matches () in
    let str =
      matches
      |> List.mapi (fun i (_, a, b) ->
             Printf.sprintf "(%d) %s vs %s" (i + 1) a b)
      |> String.concat "\n"
    in
    matches_view#buffer#set_text str
  in

  (* Initial matches update *)
  update_matches ();

  (* Refresh Matches Button *)
  let refresh_matches_button =
    button ~label:"Refresh Matches" ~packing:matches_vbox#add ()
  in
  let _ = refresh_matches_button#connect#clicked ~callback:update_matches in

  (* ========== BET HISTORY TAB ========== *)
  let history_vbox =
    vbox ~spacing:10
      ~packing:(fun w ->
        let lbl = label ~text:"Bet History" () in
        ignore (notebook#append_page ~tab_label:lbl#coerce w#coerce))
      ()
  in

  let history_view = view ~packing:history_vbox#add () in

  let update_history () =
    let bets = bets_active user in
    let str =
      if bets = [] then "No bet history."
      else
        bets
        |> List.map (fun b ->
               Printf.sprintf "%s vs %s: Bet $%.2f on %s"
                 (a_side (bet_game b))
                 (b_side (bet_game b))
                 (bet_amount b) (bet_team b))
        |> String.concat "\n"
    in
    history_view#buffer#set_text str
  in

  (* Initial history update *)
  update_history ();

  (* Refresh History Button *)
  let refresh_history_button =
    button ~label:"Refresh History" ~packing:history_vbox#add ()
  in
  let _ = refresh_history_button#connect#clicked ~callback:update_history in

  (* ========== PLACE BET TAB ========== *)
  let placebet_vbox =
    vbox ~spacing:10
      ~packing:(fun w ->
        let lbl = label ~text:"Place Bet" () in
        ignore (notebook#append_page ~tab_label:lbl#coerce w#coerce))
      ()
  in

  (* Retrieve and prepare matches for betting *)
  let matches_for_bet =
    ref
      (get_upcoming_matches ()
      |> List.map (fun (id, a, b) -> make_match id a b "null"))
  in

  (* Match Selection HBox *)
  let match_hbox = hbox ~spacing:5 ~packing:placebet_vbox#add () in

  let _ = label ~text:"Match:" ~packing:match_hbox#add () in

  (* Create combo_box_text, which returns (combo_box, (store, column)) *)
  let match_combo, (store, column) = combo_box_text () in
  let _ = match_hbox#add match_combo#coerce in

  (* Populate the combo box with matches *)
  let populate_matches_combo () =
    store#clear ();
    matches_for_bet :=
      get_upcoming_matches ()
      |> List.map (fun (id, a, b) -> make_match id a b "null");
    List.iteri
      (fun i m ->
        let row = store#append () in
        store#set ~row ~column
          (Printf.sprintf "%d: %s vs %s" (i + 1) (a_side m) (b_side m)))
      !matches_for_bet
  in

  (* Initial population of combo box *)
  populate_matches_combo ();

  (* Team Entry *)
  let _ = label ~text:"Team:" ~packing:placebet_vbox#add () in
  let team_entry = entry ~packing:placebet_vbox#add () in

  (* Amount Entry *)
  let _ = label ~text:"Amount:" ~packing:placebet_vbox#add () in
  let amount_entry = entry ~packing:placebet_vbox#add () in

  (* Place Bet Callback Function *)
  let place_bet_callback () =
    let sel_index = match_combo#active in
    (* Get selected index *)
    let team = team_entry#text in
    let amt_str = amount_entry#text in
    let index = sel_index in

    if index < 0 || index >= List.length !matches_for_bet then
      let md =
        message_dialog ~message:"Please select a valid match."
          ~message_type:`ERROR ~buttons:GWindow.Buttons.ok ~modal:true
          ~destroy_with_parent:true ~title:"Error" ()
      in
      let _ = md#run () in
      md#destroy ()
    else
      let chosen_match = List.nth !matches_for_bet index in
      if team <> a_side chosen_match && team <> b_side chosen_match then
        let md =
          message_dialog
            ~message:"Invalid team name. Please choose exactly as shown."
            ~message_type:`ERROR ~buttons:GWindow.Buttons.ok ~modal:true
            ~destroy_with_parent:true ~title:"Error" ()
        in
        let _ = md#run () in
        md#destroy ()
      else
        try
          let amount = float_of_string amt_str in
          if amount > 0. && amount <= balance user then (
            add_bet user chosen_match team amount;
            update_balance ();
            update_history ();
            amount_entry#set_text "";
            team_entry#set_text "";
            let md =
              message_dialog ~message:"Bet placed successfully!"
                ~message_type:`INFO ~buttons:GWindow.Buttons.ok ~modal:true
                ~destroy_with_parent:true ~title:"Success" ()
            in
            let _ = md#run () in
            md#destroy ())
          else
            let msg =
              Printf.sprintf "Amount must be > 0 and <= %.2f" (balance user)
            in
            let md =
              message_dialog ~message:msg ~message_type:`ERROR
                ~buttons:GWindow.Buttons.ok ~modal:true
                ~destroy_with_parent:true ~title:"Error" ()
            in
            let _ = md#run () in
            md#destroy ()
        with Failure _ ->
          let md =
            message_dialog ~message:"Invalid amount. Please enter a number."
              ~message_type:`ERROR ~buttons:GWindow.Buttons.ok ~modal:true
              ~destroy_with_parent:true ~title:"Error" ()
          in
          let _ = md#run () in
          md#destroy ()
  in

  (* Refresh Matches Button in Place Bet Tab *)
  let refresh_matches_button =
    button ~label:"Refresh Matches" ~packing:placebet_vbox#add ()
  in
  let _ =
    refresh_matches_button#connect#clicked ~callback:populate_matches_combo
  in

  (* Place Bet Button *)
  let place_bet_button =
    button ~label:"Place Bet" ~packing:placebet_vbox#add ()
  in
  let _ = place_bet_button#connect#clicked ~callback:place_bet_callback in

  (* ========== EXIT TAB ========== *)
  let exit_vbox =
    vbox ~spacing:10
      ~packing:(fun w ->
        let lbl = label ~text:"Exit" () in
        ignore (notebook#append_page ~tab_label:lbl#coerce w#coerce))
      ()
  in

  (* Save & Exit Button *)
  let save_exit_button =
    button ~label:"Save & Exit" ~packing:exit_vbox#add ()
  in
  let _ =
    save_exit_button#connect#clicked ~callback:(fun () ->
        save_to_file filename user;
        Main.quit ())
  in

  (* Show the window and run the main loop *)
  window#show ();
  Main.main ()
