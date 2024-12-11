open OUnit2

let test_make_user _ =
  let user = Final_project.User.make_user () in
  assert_equal 1000.0 (Final_project.User.balance user);
  assert_equal [] (Final_project.User.bets_active user);
  assert_equal [] (Final_project.User.bets_history user)

let test_make_user_alternative1 _ =
  let user = Final_project.User.make_user () in
  assert_bool "Initial balance is positive" (Final_project.User.balance user > 0.0);
  assert_equal [] (Final_project.User.bets_active user);
  assert_equal [] (Final_project.User.bets_history user)

let test_make_user_alternative2 _ =
  let user = Final_project.User.make_user () in
  assert_equal 0 (List.length (Final_project.User.bets_active user));
  assert_equal 0 (List.length (Final_project.User.bets_history user))

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
  assert_bool "Balance remains unchanged" (Final_project.User.balance user = 1000.0)

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

let test_change_balance_boundary_values_alternative1 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user max_float;
  Final_project.User.change_balance user (-1.0 *. max_float);
  assert_equal 1000.0 (Final_project.User.balance user)

let test_change_balance_boundary_values_alternative2 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 1.0;
  Final_project.User.change_balance user (-1.0);
  assert_equal 1000.0 (Final_project.User.balance user)

let test_add_bet_single _ =
  let match1 = Final_project.Match.make_match 10 "TeamA" "TeamB" "2:1" in
  let user = Final_project.User.make_user () in
  Final_project.User.add_bet user match1 "TeamA" 100.0;
  assert_equal 900.0 (Final_project.User.balance user);
  assert_equal 1 (List.length (Final_project.User.bets_active user));
  assert_equal 1 (List.length (Final_project.User.bets_history user))

let test_add_bet_single_alternative1 _ =
  let match1 = Final_project.Match.make_match 15 "TeamX" "TeamY" "1:1" in
  let user = Final_project.User.make_user () in
  Final_project.User.add_bet user match1 "TeamX" 150.0;
  assert_equal 850.0 (Final_project.User.balance user);
  assert_equal 1 (List.length (Final_project.User.bets_active user))

let test_add_bet_single_alternative2 _ =
  let match1 = Final_project.Match.make_match 20 "TeamM" "TeamN" "3:1" in
  let user = Final_project.User.make_user () in
  Final_project.User.add_bet user match1 "TeamM" 200.0;
  assert_equal 800.0 (Final_project.User.balance user);
  assert_equal 1 (List.length (Final_project.User.bets_active user))

let test_add_bet_multiple _ =
  let match1 = Final_project.Match.make_match 10 "TeamA" "TeamB" "2:1" in
  let match2 = Final_project.Match.make_match 20 "TeamC" "TeamD" "3:2" in
  let user = Final_project.User.make_user () in
  Final_project.User.add_bet user match1 "TeamA" 100.0;
  Final_project.User.add_bet user match2 "TeamC" 200.0;
  assert_equal 700.0 (Final_project.User.balance user);
  assert_equal 2 (List.length (Final_project.User.bets_active user));
  assert_equal 2 (List.length (Final_project.User.bets_history user))

let test_add_bet_multiple_alternative1 _ =
  let match1 = Final_project.Match.make_match 30 "TeamE" "TeamF" "4:1" in
  let match2 = Final_project.Match.make_match 40 "TeamG" "TeamH" "5:2" in
  let user = Final_project.User.make_user () in
  Final_project.User.add_bet user match1 "TeamE" 300.0;
  Final_project.User.add_bet user match2 "TeamG" 400.0;
  assert_equal 300.0 (Final_project.User.balance user);
  assert_equal 2 (List.length (Final_project.User.bets_active user))

let test_add_bet_multiple_alternative2 _ =
  let match1 = Final_project.Match.make_match 50 "TeamI" "TeamJ" "2:3" in
  let match2 = Final_project.Match.make_match 60 "TeamK" "TeamL" "1:4" in
  let user = Final_project.User.make_user () in
  Final_project.User.add_bet user match1 "TeamI" 50.0;
  Final_project.User.add_bet user match2 "TeamK" 75.0;
  assert_equal 875.0 (Final_project.User.balance user);
  assert_equal 2 (List.length (Final_project.User.bets_active user))

  open OUnit2

  let test_bets_to_string_empty _ =
    let bets = [] in
    let result = Final_project.Profile.bets_to_string bets in
    assert_equal "[]" result
  
  let test_bets_to_string_single _ =
    let bet = Final_project.Bet.make_bet 1 "TeamA" 100.0 in
    let bets = [bet] in
    let result = Final_project.Profile.bets_to_string bets in
    assert_equal ("[" ^ Final_project.Bet.to_string bet ^ "]") result
  
  let test_bets_to_string_multiple _ =
    let bet1 = Final_project.Bet.make_bet 1 "TeamA" 100.0 in
    let bet2 = Final_project.Bet.make_bet 2 "TeamB" 200.0 in
    let bets = [bet1; bet2] in
    let result = Final_project.Profile.bets_to_string bets in
    assert_equal ("[" ^ Final_project.Bet.to_string bet1 ^ "; " ^ Final_project.Bet.to_string bet2 ^ "]") result
  
  let test_string_to_bets_empty _ =
    let str = "[]" in
    let result = Final_project.Profile.string_to_bets str in
    assert_equal [] result
  
  let test_string_to_bets_single _ =
    let bet = Final_project.Bet.make_bet 1 "TeamA" 100.0 in
    let str = "[" ^ Final_project.Bet.to_string bet ^ "]" in
    let result = Final_project.Profile.string_to_bets str in
    assert_equal [bet] result
  
  let test_string_to_bets_multiple _ =
    let bet1 = Final_project.Bet.make_bet 1 "TeamA" 100.0 in
    let bet2 = Final_project.Bet.make_bet 2 "TeamB" 200.0 in
    let str = "[" ^ Final_project.Bet.to_string bet1 ^ "; " ^ Final_project.Bet.to_string bet2 ^ "]" in
    let result = Final_project.Profile.string_to_bets str in
    assert_equal [bet1; bet2] result
  
  let test_save_and_load_user _ =
    let filename = "test_user_profile.txt" in
    let user = Final_project.User.make_user () in
    Final_project.Profile.save_to_file filename user;
    let loaded_user = Final_project.Profile.load_from_file filename in
    assert_equal (Final_project.User.balance user) (Final_project.User.balance loaded_user);
    assert_equal (Final_project.User.bets_active user) (Final_project.User.bets_active loaded_user);
    assert_equal (Final_project.User.bets_history user) (Final_project.User.bets_history loaded_user)
  
  let test_save_and_load_user_with_bets _ =
    let filename = "test_user_with_bets.txt" in
    let user = Final_project.User.make_user () in
    let bet1 = Final_project.Bet.make_bet 1 "TeamA" 100.0 in
    let bet2 = Final_project.Bet.make_bet 2 "TeamB" 200.0 in
    Final_project.User.add_bet user bet1;
    Final_project.User.add_bet user bet2;
    Final_project.Profile.save_to_file filename user;
    let loaded_user = Final_project.Profile.load_from_file filename in
    assert_equal (Final_project.User.balance user) (Final_project.User.balance loaded_user);
    assert_equal (Final_project.User.bets_active user) (Final_project.User.bets_active loaded_user);
    assert_equal (Final_project.User.bets_history user) (Final_project.User.bets_history loaded_user)

let tests =
  "test_user_module"
  >::: [
         "test_make_user" >:: test_make_user;
         "test_make_user_alternative1" >:: test_make_user_alternative1;
         "test_make_user_alternative2" >:: test_make_user_alternative2;
         "test_change_balance_positive" >:: test_change_balance_positive;
         "test_change_balance_positive_alternative1" >:: test_change_balance_positive_alternative1;
         "test_change_balance_positive_alternative2" >:: test_change_balance_positive_alternative2;
         "test_change_balance_negative" >:: test_change_balance_negative;
         "test_change_balance_negative_alternative1" >:: test_change_balance_negative_alternative1;
         "test_change_balance_negative_alternative2" >:: test_change_balance_negative_alternative2;
         "test_change_balance_zero" >:: test_change_balance_zero;
         "test_change_balance_zero_alternative1" >:: test_change_balance_zero_alternative1;
         "test_change_balance_zero_alternative2" >:: test_change_balance_zero_alternative2;
         "test_change_balance_multiple_operations" >:: test_change_balance_multiple_operations;
         "test_change_balance_multiple_operations_alternative1" >:: test_change_balance_multiple_operations_alternative1;
         "test_change_balance_multiple_operations_alternative2" >:: test_change_balance_multiple_operations_alternative2;
         "test_bets_to_string_empty" >:: test_bets_to_string_empty;
         "test_bets_to_string_single" >:: test_bets_to_string_single;
         "test_bets_to_string_multiple" >:: test_bets_to_string_multiple;
         "test_string_to_bets_empty" >:: test_string_to_bets_empty;
         "test_string_to_bets_single" >:: test_string_to_bets_single;
         "test_string_to_bets_multiple" >:: test_string_to_bets_multiple;
         "test_save_and_load_user" >:: test_save_and_load_user;
         "test_save_and_load_user_with_bets" >:: test_save_and_load_user_with_bets
       ]

let () =
  run_test_tt_main tests
