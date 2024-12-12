open OUnit2

let test_make_user _ =
  let user = Final_project.User.make_user () in
  assert_equal 1000.0 (Final_project.User.balance user);
  assert_equal [] (Final_project.User.bets_active user);
  assert_equal [] (Final_project.User.bets_history user)

let test_make_user_alternative1 _ =
  let user = Final_project.User.make_user () in
  assert_bool "Initial balance is positive"
    (Final_project.User.balance user > 0.0);
  assert_equal [] (Final_project.User.bets_active user);
  assert_equal [] (Final_project.User.bets_history user)

let test_make_user_alternative2 _ =
  let user = Final_project.User.make_user () in
  assert_equal 0 (List.length (Final_project.User.bets_active user));
  assert_equal 0 (List.length (Final_project.User.bets_history user))

let test_make_user_initial_balance _ =
  let user = Final_project.User.make_user () in
  assert_equal 1000.0 (Final_project.User.balance user);
  assert_bool "Initial balance is equal to 1000.0"
    (Final_project.User.balance user = 1000.0)

let test_make_user_no_active_bets _ =
  let user = Final_project.User.make_user () in
  assert_equal [] (Final_project.User.bets_active user);
  assert_equal 0 (List.length (Final_project.User.bets_active user));
  assert_bool "Active bets should be empty on creation"
    (List.length (Final_project.User.bets_active user) = 0)

let test_make_user_no_bet_history _ =
  let user = Final_project.User.make_user () in
  assert_equal [] (Final_project.User.bets_history user);
  assert_bool "Bet history should be empty on creation"
    (List.length (Final_project.User.bets_history user) = 0)

let test_make_user_modify_bet_list _ =
  let user = Final_project.User.make_user () in
  let match1 = Final_project.Match.make_match 1 "TeamA" "TeamB" in
  let bet = Final_project.Bet.make_bet match1 "TeamA" 100.0 in
  Final_project.User.add_bet user match1 "TeamA" 100.0;
  assert_equal [ bet ] (Final_project.User.bets_active user);
  Final_project.User.remove_bet user bet;
  assert_equal [] (Final_project.User.bets_active user)

let test_change_balance_positive _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 500.0;
  assert_equal 1500.0 (Final_project.User.balance user)

let test_change_balance_positive_alternative1 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 1000.0;
  assert_equal 2000.0 (Final_project.User.balance user)

let test_change_balance_positive_alternative2 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 250.0;
  assert_equal 1250.0 (Final_project.User.balance user)

let test_change_balance_negative _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user (-200.0);
  assert_equal 800.0 (Final_project.User.balance user)

let test_change_balance_negative_alternative1 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user (-500.0);
  assert_equal 500.0 (Final_project.User.balance user)

let test_change_balance_negative_alternative2 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user (-750.0);
  assert_equal 250.0 (Final_project.User.balance user)

let test_change_balance_zero _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 0.0;
  assert_equal 1000.0 (Final_project.User.balance user)

let test_change_balance_zero_alternative1 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 0.0;
  assert_bool "Balance remains unchanged"
    (Final_project.User.balance user = 1000.0)

let test_change_balance_zero_alternative2 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 0.0;
  assert_equal (Final_project.User.balance user) 1000.0

let test_change_balance_multiple_operations _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 300.0;
  assert_equal 1300.0 (Final_project.User.balance user);
  Final_project.User.change_balance user (-100.0);
  assert_equal 1200.0 (Final_project.User.balance user);
  Final_project.User.change_balance user 200.0;
  assert_equal 1400.0 (Final_project.User.balance user);
  Final_project.User.change_balance user (-400.0);
  assert_equal 1000.0 (Final_project.User.balance user)

let test_change_balance_multiple_operations_alternative1 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 100.0;
  Final_project.User.change_balance user (-50.0);
  Final_project.User.change_balance user 25.0;
  assert_equal 1075.0 (Final_project.User.balance user)

let test_change_balance_multiple_operations_alternative2 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 500.0;
  Final_project.User.change_balance user (-500.0);
  assert_equal 1000.0 (Final_project.User.balance user)

let test_change_balance_boundary_values _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 1e6;
  assert_equal 1_001_000.0 (Final_project.User.balance user);
  Final_project.User.change_balance user (-1e6);
  assert_equal 1000.0 (Final_project.User.balance user)

let test_change_balance_large_positive _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 1000000.0;
  assert_equal 1_001_000.0 (Final_project.User.balance user)

let test_change_balance_large_negative _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user (-999000.0);
  assert_equal (-998000.0) (Final_project.User.balance user)

let test_change_balance_exact_balance_deduction _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user (-1000.0);
  assert_equal 0.0 (Final_project.User.balance user)

let test_change_balance_addition_and_zero_check _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 1000.0;
  Final_project.User.change_balance user (-1000.0);
  assert_equal 1000.0 (Final_project.User.balance user)

let test_change_balance_multiple_zero_changes _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 0.0;
  Final_project.User.change_balance user 0.0;
  assert_equal 1000.0 (Final_project.User.balance user)

let test_change_balance_positive_then_negative _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 500.0;
  assert_equal 1500.0 (Final_project.User.balance user);
  Final_project.User.change_balance user (-500.0);
  assert_equal 1000.0 (Final_project.User.balance user)

let test_change_balance_negative_then_positive _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user (-500.0);
  assert_equal 500.0 (Final_project.User.balance user);
  Final_project.User.change_balance user 500.0;
  assert_equal 1000.0 (Final_project.User.balance user)

let test_change_balance_small_increments _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 0.1;
  Final_project.User.change_balance user 0.1;
  assert_equal 1000.2 (Final_project.User.balance user)

let test_change_balance_small_decrements _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user (-0.1);
  Final_project.User.change_balance user (-0.1);
  assert_equal 999.8 (Final_project.User.balance user)

let test_modify_bet_valid _ =
  let user = Final_project.User.make_user () in
  let game = Final_project.Match.make_match 1 "TeamA" "TeamB" in
  let bet = Final_project.Bet.make_bet game "TeamA" 100.0 in
  Final_project.User.add_bet user game "TeamA" 100.0;
  (* Add the initial bet *)
  Final_project.User.modify_bet user bet 50.0;
  (* Modify the bet *)
  assert_equal 850.0 (Final_project.User.balance user);
  let updated_bet = List.hd (Final_project.User.bets_active user) in
  assert_equal 150.0 (Final_project.Bet.bet_amount updated_bet)

let test_modify_bet_insufficient_balance _ =
  let user = Final_project.User.make_user () in
  let game = Final_project.Match.make_match 1 "TeamA" "TeamB" in
  let bet = Final_project.Bet.make_bet game "TeamA" 100.0 in
  Final_project.User.add_bet user game "TeamA" 100.0;
  assert_raises Final_project.User.Insufficient_Balance (fun () ->
      Final_project.User.modify_bet user bet 950.0);
  assert_equal 900.0 (Final_project.User.balance user);
  assert_equal 1 (List.length (Final_project.User.bets_active user))

let test_bets_to_string_multiple _ =
  let match1 = Final_project.Match.make_match 1 "TeamA" "TeamB" in
  let match2 = Final_project.Match.make_match 2 "TeamC" "TeamD" in
  let bet1 = Final_project.Bet.make_bet match1 "TeamA" 100.0 in
  let bet2 = Final_project.Bet.make_bet match2 "TeamC" 200.0 in
  let bets = [ bet1; bet2 ] in
  let result = Final_project.Profile.bets_to_string bets in
  assert_equal
    ("["
    ^ Final_project.Bet.to_string bet1
    ^ "; "
    ^ Final_project.Bet.to_string bet2
    ^ "]")
    result

let test_string_to_bets_empty _ =
  let str = "[]" in
  let result = Final_project.Profile.string_to_bets str in
  assert_equal [] result

let test_save_and_load_user _ =
  let filename = "test_user_profile.txt" in
  let user = Final_project.User.make_user () in
  Final_project.Profile.save_to_file filename user;
  let loaded_user = Final_project.Profile.load_from_file filename in
  assert_equal
    (Final_project.User.balance user)
    (Final_project.User.balance loaded_user);
  assert_equal
    (Final_project.User.bets_active user)
    (Final_project.User.bets_active loaded_user);
  assert_equal
    (Final_project.User.bets_history user)
    (Final_project.User.bets_history loaded_user)

(** let test_save_and_load_user_with_bets _ = let filename =
    "test_user_with_bets.txt" in let user = Final_project.User.make_user () in
    let match1 = Final_project.Match.make_match 1 "TeamA" "TeamB" in let match2
    = Final_project.Match.make_match 2 "TeamC" "TeamD" in
    Final_project.User.add_bet user match1 "TeamA" 100.0;
    Final_project.User.add_bet user match2 "TeamC" 200.0;
    Final_project.Profile.save_to_file filename user; let loaded_user =
    Final_project.Profile.load_from_file filename in assert_equal
    (Final_project.User.balance user) (Final_project.User.balance loaded_user);
    assert_equal (Final_project.User.bets_active user)
    (Final_project.User.bets_active loaded_user); assert_equal
    (Final_project.User.bets_history user) (Final_project.User.bets_history
    loaded_user) *)

let test_bets_to_string_empty _ =
  let bets = [] in
  let result = Final_project.Profile.bets_to_string bets in
  assert_equal "[]" result

let test_bets_to_string_complex _ =
  let match1 = Final_project.Match.make_match 1 "TeamA" "TeamB" in
  let match2 = Final_project.Match.make_match 2 "TeamC" "TeamD" in
  let match3 = Final_project.Match.make_match 3 "TeamE" "TeamF" in
  let bet1 = Final_project.Bet.make_bet match1 "TeamA" 100.0 in
  let bet2 = Final_project.Bet.make_bet match2 "TeamD" 50.0 in
  let bet3 = Final_project.Bet.make_bet match3 "TeamE" 75.0 in
  let bets = [ bet1; bet2; bet3 ] in
  let result = Final_project.Profile.bets_to_string bets in
  assert_equal
    ("["
    ^ Final_project.Bet.to_string bet1
    ^ "; "
    ^ Final_project.Bet.to_string bet2
    ^ "; "
    ^ Final_project.Bet.to_string bet3
    ^ "]")
    result

let test_string_to_bets_malformed _ =
  let str = "[INVALID STRING]" in
  assert_raises (Failure "Invalid bet string format") (fun () ->
      Final_project.Profile.string_to_bets str);
  assert_raises (Failure("Invalid bet string format")) (fun () -> 
    Final_project.Profile.string_to_bets "([vamo], 0.0)")

let test_string_to_bets_partial _ =
  let match1 = Final_project.Match.make_match 1 "TeamA" "TeamB" in
  let bet1 = Final_project.Bet.make_bet match1 "TeamA" 100.0 in
  let str = "[" ^ Final_project.Bet.to_string bet1 ^ "; INVALID]" in
  assert_raises (Failure "Invalid bet string format") (fun () ->
      Final_project.Profile.string_to_bets str)

let test_user_initial_balance _ =
  let user = Final_project.User.make_user () in
  assert_equal 1000.0 (Final_project.User.balance user)

let test_user_change_balance _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 500.0;
  assert_equal 1500.0 (Final_project.User.balance user)

let test_user_active_bets _ =
  let user = Final_project.User.make_user () in
  let match1 = Final_project.Match.make_match 1 "TeamA" "TeamB" in
  let match2 = Final_project.Match.make_match 2 "TeamC" "TeamD" in
  Final_project.User.add_bet user match1 "TeamA" 100.0;
  Final_project.User.add_bet user match2 "TeamD" 200.0;
  let active_bets = Final_project.User.bets_active user in
  assert_equal 2 (List.length active_bets)

let test_average_nonempty _ =
  let lst = [ 1.0; 2.0; 3.0; 4.0; 5.0 ] in
  assert_equal 3.0 (Final_project.Bet_odds.average lst)

let test_average_empty _ =
  assert_raises Final_project.Bet_odds.InvalidData (fun () ->
      Final_project.Bet_odds.average [])

let test_average _ =
  let lst = [ 42.0 ] in
  assert_equal 42.0 (Final_project.Bet_odds.average lst)

let test_avg_for_against_nonempty _ =
  let results = [ (2.0, 1.0); (3.0, 2.0); (1.0, 1.0) ] in
  assert_equal (2.0, 1.3333333333333333)
    (Final_project.Bet_odds.avg_for_against results)

let test_avg_for_against_empty _ =
  assert_raises Final_project.Bet_odds.InvalidData (fun () ->
      Final_project.Bet_odds.avg_for_against [])

let test_avg_for_against_one _ =
  let results = [ (4.0, 3.0) ] in
  assert_equal (4.0, 3.0) (Final_project.Bet_odds.avg_for_against results)

let test_poisson_pmf_zero_lambda _ =
  assert_equal 0.0 (Final_project.Bet_odds.poisson_pmf 0.0 1)

let test_poisson_pmf_zero_k _ =
  assert_equal 0.36787944117144233 (Final_project.Bet_odds.poisson_pmf 1.0 0)

(* Define test cases *)
let test_match_id _ =
  let match_ = Final_project.Match.make_match 1 "TeamA" "TeamB" in
  assert_equal 1 (Final_project.Match.match_id match_)

(* Test for odds_to_string function *)
let test_odds_to_string _ =
  let test_cases = [
    (None, "None");
    (Some (2.0, 3.0, 4.0), "(2.00, 3.00, 4.00)");
    (Some (1.23, 4.56, 7.89), "(1.23, 4.56, 7.89)");
    (Some (0.0, 0.0, 0.0), "(0.00, 0.00, 0.00)");
    (Some (10.12345, 20.6789, 30.98765), "(10.12, 20.68, 30.99)")
  ] in
  List.iter (fun (input, expected) ->
    let result = Final_project.Match.odds_to_string input in
    assert_equal ~msg:("odds_to_string " ^ (match input with 
      | None -> "None"
      | Some (x, y, z) -> Printf.sprintf "(%.2f, %.2f, %.2f)" x y z
    )) expected result
  ) test_cases

(* Test for match_odds function *)
let test_match_odds _ =
  (* Test 1: Match with no odds *)
  let match1 = Final_project.Match.make_match 1 "TeamA" "TeamB" in
  assert_equal "None" (Final_project.Match.match_odds match1);

  (* Test 2: Match with some odds set manually *)
  let match2 = Final_project.Match.make_match 2 "TeamX" "TeamY" in
  let match2 = Final_project.Match.set_match_odds match2 (Some (0.0, 0.0, 0.0)) in
  assert_equal "(0.00, 0.00, 0.00)" (Final_project.Match.match_odds match2)


let test_a_side _ =
  let match_ = Final_project.Match.make_match 1 "TeamA" "TeamB" in
  assert_equal "TeamA" (Final_project.Match.a_side match_)

let test_b_side _ =
  let match_ = Final_project.Match.make_match 1 "TeamA" "TeamB" in
  assert_equal "TeamB" (Final_project.Match.b_side match_)

let test_to_string _ =
  let match_ = Final_project.Match.make_match 1 "TeamA" "TeamB" in
  let expected = "1/TeamA/TeamB" in
  assert_equal expected (Final_project.Match.to_string match_)

let test_of_string _ =
  let match_str = "1/TeamA/TeamB" in
  let match_ = Final_project.Match.of_string match_str in
  assert_equal 1 (Final_project.Match.match_id match_);
  assert_equal "TeamA" (Final_project.Match.a_side match_);
  assert_equal "TeamB" (Final_project.Match.b_side match_);
  assert_raises 
    (Failure("Error occured with making match: 1/TeamA due to exception: Failure(\"Invalid match string format\")"))
    (fun () -> Final_project.Match.of_string "1/TeamA")

let test_match_odds_skewed _ =
  let a_results = [ (10.0, 0.0); (10.0, 0.0) ] in
  let b_results = [ (0.0, 10.0); (0.0, 10.0) ] in
  assert_equal "100 to 1"
    (Final_project.Bet_odds.match_odds a_results b_results);

  let a_results_2 = [(0., 3.); (0., 2.); (0., 1.)] (* Team A never scores *) in
  let b_results_2 = [(3., 0.); (2., 0.); (1., 0.)] (* Team B always scores more *) in
  let odds_2 = Final_project.Bet_odds.match_odds a_results_2 b_results_2 in
  assert_equal odds_2 "1 to 100" ~msg:"Expected '1 to 100' when Team A never wins"

let test_match_odds_equal _ =
  let a_results = [ (1.0, 1.0) ] in
  let b_results = [ (1.0, 1.0) ] in
  assert_equal "1 to 1" (Final_project.Bet_odds.match_odds a_results b_results);

  let a_results_2 = [(0., 0.); (0., 0.)] in
  let b_results_2 = [(0., 0.); (0., 0.)] in
  assert_equal "1 to 1" (Final_project.Bet_odds.match_odds a_results_2 b_results_2)

let test_match_odds_no_data _ =
  let a_results = [] in
  let b_results = [] in
  assert_raises Final_project.Bet_odds.InvalidData (fun () ->
      Final_project.Bet_odds.match_odds a_results b_results)

let test_match_odds_partial_data _ =
  let a_results = [ (2.0, 1.0) ] in
  let b_results = [] in
  assert_raises Final_project.Bet_odds.InvalidData (fun () ->
      Final_project.Bet_odds.match_odds a_results b_results)

let test_poisson_pmf_zero_lambda_and_k _ =
  assert_equal 1.0 (Final_project.Bet_odds.poisson_pmf 0.0 0)

let test_average_negative_numbers _ =
  let lst = [ -3.0; -6.0; -9.0 ] in
  assert_equal (-6.0) (Final_project.Bet_odds.average lst)

let test_average_large_list _ =
  let lst = List.init 1000 (fun i -> float_of_int (i + 1)) in
  assert_equal 500.5 (Final_project.Bet_odds.average lst)

(*API_HANDLING tests*)

let test_format_date _ =
  (* Set a fixed timestamp for consistent testing *)
  let timestamp = 1609459200.0 (* This corresponds to 2021-01-01 *) in
  let result = Final_project.Api_handling.format_date timestamp in
  assert_equal "2021-01-01" result

(* Mock HTTP GET function *)
let mock_http_get uri _headers =
  let uri_string = Uri.to_string uri in
  if
    String.starts_with ~prefix:"https://v3.football.api-sports.io/fixtures?date"
      uri_string
  then
    Lwt.return
      ( Cohttp.Response.make ~status:`OK (),
        Cohttp_lwt.Body.of_string
          "{\"response\": [{\"fixture\": {\"id\": 1}, \"teams\": {\"home\": \
           {\"name\": \"Team A\"}, \"away\": {\"name\": \"Team B\"}}}]}" )
  else if
    String.starts_with ~prefix:"https://v3.football.api-sports.io/fixtures?id"
      uri_string
  then
    Lwt.return
      ( Cohttp.Response.make ~status:`OK (),
        Cohttp_lwt.Body.of_string
          "{\"response\": [{\"fixture\": {\"status\": {\"short\": \"FT\"}}, \
           \"teams\": {\"home\": {\"name\": \"Team A\"}, \"away\": {\"name\": \
           \"Team B\"}}, \"goals\": {\"home\": 2, \"away\": 1}}]}" )
  else if
    String.starts_with ~prefix:"https://v3.football.api-sports.io/odds?fixture"
      uri_string
  then
    Lwt.return
      ( Cohttp.Response.make ~status:`OK (),
        Cohttp_lwt.Body.of_string
          "{\"response\": [{\"bookmakers\": [{\"bets\": [{\"name\": \"Match \
           Winner\", \"values\": [{\"value\": \"Home\", \"odd\": \"1.5\"}, \
           {\"value\": \"Draw\", \"odd\": \"3.0\"}, {\"value\": \"Away\", \
           \"odd\": \"2.5\"}]}]}]}]}" )
  else Lwt.fail_with "Unmocked URI"

(* Test get_upcoming_matches function with mocked API response *)
let test_get_upcoming_matches _ =
  let expected = [ (1, "Team A", "Team B") ] in
  let actual =
    Final_project.Api_handling.get_upcoming_matches ~http_get:mock_http_get ()
  in
  assert_equal
    ~printer:(fun x ->
      String.concat "; "
        (List.map
           (fun (id, home, away) -> Printf.sprintf "(%d, %s, %s)" id home away)
           x))
    expected actual

(* Test get_match_result function with mocked API response *)
let test_get_match_result _ =
  let expected = "Team A" in
  let actual =
    Final_project.Api_handling.get_match_result ~http_get:mock_http_get 1
  in
  assert_equal ~printer:(fun x -> x) expected actual

(* Test get_match_winner_odds function with mocked API response *)
let test_get_match_winner_odds _ =
  let expected = Some (1.5, 3.0, 2.5) in
  let actual =
    Final_project.Api_handling.get_match_winner_odds ~http_get:mock_http_get 1
  in
  assert_equal
    ~printer:(fun x ->
      match x with
      | Some (h, d, a) -> Printf.sprintf "Some (%f, %f, %f)" h d a
      | None -> "None")
    expected actual

let tests =
  "test_user_module"
  >::: [
         "test_make_user" >:: test_make_user;
         "test_make_user_alternative1" >:: test_make_user_alternative1;
         "test_make_user_alternative2" >:: test_make_user_alternative2;
         "test_make_user_initial_balance" >:: test_make_user_initial_balance;
         "test_make_user_no_active_bets" >:: test_make_user_no_active_bets;
         "test_make_user_no_bet_history" >:: test_make_user_no_bet_history;
         "test_make_user_modify_bet_list" >:: test_make_user_modify_bet_list;
         "test_change_balance_positive" >:: test_change_balance_positive;
         "test_change_balance_positive_alternative1"
         >:: test_change_balance_positive_alternative1;
         "test_change_balance_positive_alternative2"
         >:: test_change_balance_positive_alternative2;
         "test_change_balance_negative" >:: test_change_balance_negative;
         "test_change_balance_negative_alternative1"
         >:: test_change_balance_negative_alternative1;
         "test_change_balance_negative_alternative2"
         >:: test_change_balance_negative_alternative2;
         "test_change_balance_zero" >:: test_change_balance_zero;
         "test_change_balance_zero_alternative1"
         >:: test_change_balance_zero_alternative1;
         "test_change_balance_zero_alternative2"
         >:: test_change_balance_zero_alternative2;
         "test_change_balance_multiple_operations"
         >:: test_change_balance_multiple_operations;
         "test_change_balance_multiple_operations_alternative1"
         >:: test_change_balance_multiple_operations_alternative1;
         "test_change_balance_multiple_operations_alternative2"
         >:: test_change_balance_multiple_operations_alternative2;
         "test_change_balance_boundary_values"
         >:: test_change_balance_boundary_values;
         "test_change_balance_large_positive"
         >:: test_change_balance_large_positive;
         "test_change_balance_large_negative"
         >:: test_change_balance_large_negative;
         "test_change_balance_exact_balance_deduction"
         >:: test_change_balance_exact_balance_deduction;
         "test_change_balance_addition_and_zero_check"
         >:: test_change_balance_addition_and_zero_check;
         "test_change_balance_multiple_zero_changes"
         >:: test_change_balance_multiple_zero_changes;
         "test_change_balance_positive_then_negative"
         >:: test_change_balance_positive_then_negative;
         "test_change_balance_negative_then_positive"
         >:: test_change_balance_negative_then_positive;
         "test_change_balance_small_increments"
         >:: test_change_balance_small_increments;
         "test_change_balance_small_decrements"
         >:: test_change_balance_small_decrements;
         "test_bets_to_string_multiple" >:: test_bets_to_string_multiple;
         "test_string_to_bets_empty" >:: test_string_to_bets_empty;
         "test_save_and_load_user" >:: test_save_and_load_user;
         (* "test_save_and_load_user_with_bets" >::
            test_save_and_load_user_with_bets; *)
         "test_bets_to_string_empty" >:: test_bets_to_string_empty;
         "test_bets_to_string_complex" >:: test_bets_to_string_complex;
         "test_string_to_bets_malformed" >:: test_string_to_bets_malformed;
         "test_string_to_bets_partial" >:: test_string_to_bets_partial;
         "test_user_initial_balance" >:: test_user_initial_balance;
         "test_user_change_balance" >:: test_user_change_balance;
         "test_user_active_bets" >:: test_user_active_bets;
         "test_average_nonempty" >:: test_average_nonempty;
         "test_average_empty" >:: test_average_empty;
         "test_average" >:: test_average;
         "test_avg_for_against_nonempty" >:: test_avg_for_against_nonempty;
         "test_avg_for_against_empty" >:: test_avg_for_against_empty;
         "test_avg_for_against_one" >:: test_avg_for_against_one;
         "test_poisson_pmf_zero_lambda" >:: test_poisson_pmf_zero_lambda;
         "test_poisson_pmf_zero_k" >:: test_poisson_pmf_zero_k;
         "test_match_id" >:: test_match_id;
         "test_odds_to_string" >:: test_odds_to_string;
         "test_match_odds" >:: test_match_odds;
         "test_a_side" >:: test_a_side;
         "test_b_side" >:: test_b_side;
         "test_to_string" >:: test_to_string;
         "test_of_string" >:: test_of_string;
         "test_match_odds_skewed" >:: test_match_odds_skewed;
         "test_match_odds_equal" >:: test_match_odds_equal;
         "test_match_odds_no_data" >:: test_match_odds_no_data;
         "test_match_odds_partial_data" >:: test_match_odds_partial_data;
         "test_poisson_pmf_zero_lambda_and_k"
         >:: test_poisson_pmf_zero_lambda_and_k;
         "test_average_negative_numbers" >:: test_average_negative_numbers;
         "test_average_large_list" >:: test_average_large_list;
         "test_modify_bet_valid" >:: test_modify_bet_valid;
         "test_modify_bet_insufficient_balance"
         >:: test_modify_bet_insufficient_balance;
         "test_format_date" >:: test_format_date;
         "test_get_upcoming_matches" >:: test_get_upcoming_matches;
         "test_get_match_result" >:: test_get_match_result;
         "test_get_match_winner_odds" >:: test_get_match_winner_odds;
       ]

let () = run_test_tt_main tests
