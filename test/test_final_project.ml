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
  let match1 = Final_project.Match.make_match 1 "TeamA" "TeamB" "2:1" in
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

let test_bets_to_string_multiple _ =
  let match1 = Final_project.Match.make_match 1 "TeamA" "TeamB" "2:1" in
  let match2 = Final_project.Match.make_match 2 "TeamC" "TeamD" "3:2" in
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
    let match1 = Final_project.Match.make_match 1 "TeamA" "TeamB" "2:1" in let
    match2 = Final_project.Match.make_match 2 "TeamC" "TeamD" "3:2" in
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
  let match1 = Final_project.Match.make_match 1 "TeamA" "TeamB" "2:1" in
  let match2 = Final_project.Match.make_match 2 "TeamC" "TeamD" "3:2" in
  let match3 = Final_project.Match.make_match 3 "TeamE" "TeamF" "1:1" in
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
      Final_project.Profile.string_to_bets str)

let test_string_to_bets_partial _ =
  let match1 = Final_project.Match.make_match 1 "TeamA" "TeamB" "2:1" in
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
  let match1 = Final_project.Match.make_match 1 "TeamA" "TeamB" "2:1" in
  let match2 = Final_project.Match.make_match 2 "TeamC" "TeamD" "3:2" in
  Final_project.User.add_bet user match1 "TeamA" 100.0;
  Final_project.User.add_bet user match2 "TeamD" 200.0;
  let active_bets = Final_project.User.bets_active user in
  assert_equal 2 (List.length active_bets)

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
       ]

let () = run_test_tt_main tests
