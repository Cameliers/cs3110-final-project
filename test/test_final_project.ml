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

let test_change_balance_fractional_precision _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 0.000001;
  assert_bool "Balance handles fractional precision"
    (abs_float (Final_project.User.balance user -. 1000.000001) < 1e-9)

let test_change_balance_round_trip _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 500.0;
  Final_project.User.change_balance user (-500.0);
  Final_project.User.change_balance user (-500.0);
  Final_project.User.change_balance user 500.0;
  assert_equal 1000.0 (Final_project.User.balance user)

let test_change_balance_zero_check_after_multiple_ops _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 200.0;
  Final_project.User.change_balance user (-200.0);
  Final_project.User.change_balance user 300.0;
  Final_project.User.change_balance user (-300.0);
  assert_equal 1000.0 (Final_project.User.balance user)

let test_change_balance_multiple_fractions _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 0.333333;
  Final_project.User.change_balance user (-0.333333);
  Final_project.User.change_balance user 0.111111;
  Final_project.User.change_balance user (-0.111111);
  assert_equal 1000.0 (Final_project.User.balance user)

let test_change_balance_substantial_overdraw _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user (-1e9);
  assert_equal (-999_999_000.0) (Final_project.User.balance user)

let test_change_balance_positive_variant1 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 750.0;
  assert_equal 1750.0 (Final_project.User.balance user)

let test_change_balance_positive_variant2 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 300.0;
  assert_bool "Balance is 1300.0 after addition"
    (Final_project.User.balance user = 1300.0)

let test_change_balance_negative_variant1 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user (-300.0);
  assert_equal 700.0 (Final_project.User.balance user)

let test_change_balance_negative_variant2 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user (-100.0);
  assert_bool "Balance is 900.0 after deduction"
    (Final_project.User.balance user = 900.0)

let test_change_balance_zero_variant1 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 0.0;
  Final_project.User.change_balance user 0.0;
  assert_equal 1000.0 (Final_project.User.balance user)

let test_change_balance_zero_variant2 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 0.0;
  assert_bool "Balance remains the same with zero change"
    (Final_project.User.balance user = 1000.0)

let test_change_balance_multiple_operations_variant1 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 200.0;
  Final_project.User.change_balance user (-150.0);
  assert_equal 1050.0 (Final_project.User.balance user);
  Final_project.User.change_balance user 500.0;
  assert_equal 1550.0 (Final_project.User.balance user)

let test_change_balance_multiple_operations_variant2 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 50.0;
  Final_project.User.change_balance user 50.0;
  assert_equal 1100.0 (Final_project.User.balance user)

let test_change_balance_large_positive_variant1 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 500000.0;
  assert_equal 501000.0 (Final_project.User.balance user)

let test_change_balance_large_positive_variant2 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 1e5;
  assert_equal 101000.0 (Final_project.User.balance user)

let test_change_balance_large_negative_variant1 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user (-999500.0);
  assert_equal (-998500.0) (Final_project.User.balance user)

let test_change_balance_large_negative_variant2 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user (-500000.0);
  assert_equal (-499000.0) (Final_project.User.balance user)

let test_change_balance_exact_balance_deduction_variant1 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user (-1000.0);
  Final_project.User.change_balance user 1000.0;
  assert_equal 1000.0 (Final_project.User.balance user)

let test_change_balance_exact_balance_deduction_variant2 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user (-1000.0);
  assert_bool "Balance is exactly zero after full deduction"
    (Final_project.User.balance user = 0.0)

let test_change_balance_small_increments_variant1 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 0.01;
  Final_project.User.change_balance user 0.02;
  assert_equal 1000.03 (Final_project.User.balance user)

let test_change_balance_small_increments_variant2 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 0.001;
  Final_project.User.change_balance user 0.001;
  assert_equal 1000.002 (Final_project.User.balance user)

let test_change_balance_small_decrements_variant2 _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user (-0.01);
  Final_project.User.change_balance user (-0.01);
  assert_equal 999.98 (Final_project.User.balance user)

let test_change_balance_negative_overdraw _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user (-2000.0);
  assert_equal (-1000.0) (Final_project.User.balance user)

let test_change_balance_small_fractional_increments _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 0.01;
  Final_project.User.change_balance user 0.01;
  assert_equal 1000.02 (Final_project.User.balance user)

let test_change_balance_small_fractional_decrements _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user (-0.01);
  Final_project.User.change_balance user (-0.01);
  assert_equal 999.98 (Final_project.User.balance user)

let test_change_balance_large_sequence _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 1e6;
  Final_project.User.change_balance user (-500000.0);
  Final_project.User.change_balance user 500000.0;
  Final_project.User.change_balance user (-1e6);
  assert_equal 1000.0 (Final_project.User.balance user)

let test_change_balance_multiple_small_increments_and_decrements _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 0.1;
  Final_project.User.change_balance user (-0.1);
  Final_project.User.change_balance user 0.2;
  Final_project.User.change_balance user (-0.2);
  assert_equal 1000.0 (Final_project.User.balance user)

let test_change_balance_no_change_after_zero_operations _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 0.0;
  Final_project.User.change_balance user 0.0;
  Final_project.User.change_balance user 0.0;
  assert_equal 1000.0 (Final_project.User.balance user)

let test_change_balance_rounding _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 0.333333333;
  Final_project.User.change_balance user (-0.333333333);
  assert_bool "Balance remains consistent after rounding"
    (abs_float (Final_project.User.balance user -. 1000.0) < 1e-9)

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
    ^ ";"
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
    ^ ";"
    ^ Final_project.Bet.to_string bet2
    ^ ";"
    ^ Final_project.Bet.to_string bet3
    ^ "]")
    result

let test_string_to_bets_malformed _ =
  let str = "[INVALID STRING]" in
  assert_raises (Failure "Invalid bet string format") (fun () ->
      Final_project.Profile.string_to_bets str);
  assert_raises (Failure "Invalid bet string format") (fun () ->
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
  assert_equal 3.0 (Final_project.Bet_odds.average lst);

  let lst2 = [ 0.0; 0.0; 0.0; 0.0 ] in
  assert_equal 0.0 (Final_project.Bet_odds.average lst2);

  (* Test with all positive numbers *)
  let lst3 = [ 1.5; 2.5; 3.5; 4.5 ] in
  assert_equal 3.0 (Final_project.Bet_odds.average lst3);

  let lst5 = [ 2.0; 1.0; 3.0; 2.0 ] in
  assert_equal 2.0 (Final_project.Bet_odds.average lst5);

  (* Test with a list of size 1 *)
  let lst6 = [ 10.0 ] in
  assert_equal 10.0 (Final_project.Bet_odds.average lst6);

  (* Test with very large numbers *)
  let lst8 = [ 1000000.0; 2000000.0; 3000000.0 ] in
  assert_equal 2000000.0 (Final_project.Bet_odds.average lst8);

  (* Test with a list containing zero *)
  let lst9 = [ 0.0; 1.0; 2.0; 3.0 ] in
  assert_equal 1.5 (Final_project.Bet_odds.average lst9)

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
  assert_equal 1 (Final_project.Match.match_id match_);

  (* Test with a match with a different ID *)
  let match_2 = Final_project.Match.make_match 42 "TeamC" "TeamD" in
  assert_equal 42 (Final_project.Match.match_id match_2);

  (* Test with a match with a negative ID *)
  let match_3 = Final_project.Match.make_match (-1) "TeamE" "TeamF" in
  assert_equal (-1) (Final_project.Match.match_id match_3);

  (* Test with a match with a zero ID *)
  let match_4 = Final_project.Match.make_match 0 "TeamG" "TeamH" in
  assert_equal 0 (Final_project.Match.match_id match_4);

  (* Test with a match having a very large ID *)
  let match_5 = Final_project.Match.make_match 9999999 "TeamI" "TeamJ" in
  assert_equal 9999999 (Final_project.Match.match_id match_5);

  (* Test with a match having a very small (negative) ID *)
  let match_6 = Final_project.Match.make_match (-9999999) "TeamK" "TeamL" in
  assert_equal (-9999999) (Final_project.Match.match_id match_6);

  (* Test with a match having a non-standard ID of 1001 *)
  let match_7 = Final_project.Match.make_match 1001 "TeamM" "TeamN" in
  assert_equal 1001 (Final_project.Match.match_id match_7);

  (* Test with a match having an unusually large ID (max int) *)
  let match_8 = Final_project.Match.make_match max_int "TeamO" "TeamP" in
  assert_equal max_int (Final_project.Match.match_id match_8);

  (* Test with a match having an unusually small ID (min int) *)
  let match_9 = Final_project.Match.make_match min_int "TeamQ" "TeamR" in
  assert_equal min_int (Final_project.Match.match_id match_9)

(* Test for odds_to_string function *)
let test_odds_to_string _ =
  let test_cases =
    [
      (None, "None");
      (Some (2.0, 3.0, 4.0), "(2.00, 4.00)");
      (Some (1.23, 4.56, 7.89), "(1.23, 7.89)");
      (Some (0.0, 0.0, 0.0), "(0.00, 0.00)");
      (Some (10.12345, 20.6789, 30.98765), "(10.12, 30.99)");
      (Some (0.001, 0.002, 0.003), "(0.00, 0.00)");
      (Some (1.0, 2.0, 3.0), "(1.00, 3.00)");
      (Some (5.12, 7.56, 9.99), "(5.12, 9.99)");
      (Some (0.1, 100.0, 1000.0), "(0.10, 1000.00)");
      (Some (0.03, 0.05, 0.07), "(0.03, 0.07)");
      (Some (1234.56, 7890.12, 3456.78), "(1234.56, 3456.78)");
    ]
  in
  List.iter
    (fun (input, expected) ->
      let result = Final_project.Match.odds_to_string input in
      assert_equal
        ~msg:
          ("odds_to_string "
          ^
          match input with
          | None -> "None"
          | Some (x, y, z) -> Printf.sprintf "(%.2f, %.2f, %.2f)" x y z)
        expected result)
    test_cases

(* Test for match_odds function *)
let test_match_odds _ =
  (* Test 1: Match with no odds *)
  let match1 = Final_project.Match.make_match 1 "TeamA" "TeamB" in
  assert_equal "None" (Final_project.Match.match_odds match1);

  (* Test Type 2: Match with some odds set manually *)
  let match2 = Final_project.Match.make_match 2 "TeamX" "TeamY" in
  let match2 =
    Final_project.Match.set_match_odds match2 (Some (0.0, 0.0, 0.0))
  in
  assert_equal "(0.00, 0.00)" (Final_project.Match.match_odds match2);

  let match3 = Final_project.Match.make_match 3 "TeamW" "TeamZ" in
  let match3 =
    Final_project.Match.set_match_odds match3 (Some (1.0, 1.0, 1.0))
  in
  assert_equal "(1.00, 1.00)" (Final_project.Match.match_odds match3);

  let match4 = Final_project.Match.make_match 4 "TeamC" "TeamD" in
  let match4 =
    Final_project.Match.set_match_odds match4 (Some (1.0, 2.0, 3.0))
  in
  assert_equal "(1.00, 3.00)" (Final_project.Match.match_odds match4);

  let match5 = Final_project.Match.make_match 5 "TeamC" "TeamD" in
  let match5 =
    Final_project.Match.set_match_odds match5 (Some (0.1, 0.2, 0.3))
  in
  assert_equal "(0.10, 0.30)" (Final_project.Match.match_odds match5);

  let match6 = Final_project.Match.make_match 6 "TeamE" "TeamF" in
  let match6 =
    Final_project.Match.set_match_odds match6 (Some (3.14, 2.71, 1.62))
  in
  assert_equal "(3.14, 1.62)" (Final_project.Match.match_odds match6);

  (* Test 7: Match with odds set to (100.0, 200.0, 300.0) *)
  let match7 = Final_project.Match.make_match 7 "TeamG" "TeamH" in
  let match7 =
    Final_project.Match.set_match_odds match7 (Some (100.0, 200.0, 300.0))
  in
  assert_equal "(100.00, 300.00)" (Final_project.Match.match_odds match7);

  let match8 = Final_project.Match.make_match 8 "TeamI" "TeamJ" in
  let match8 =
    Final_project.Match.set_match_odds match8 (Some (10.0, 20.0, 30.0))
  in
  assert_equal "(10.00, 30.00)" (Final_project.Match.match_odds match8);

  let match9 = Final_project.Match.make_match 9 "TeamK" "TeamL" in
  let match9 =
    Final_project.Match.set_match_odds match9 (Some (0.9, 1.8, 2.7))
  in
  assert_equal "(0.90, 2.70)" (Final_project.Match.match_odds match9);

  let match10 = Final_project.Match.make_match 10 "TeamM" "TeamN" in
  let match10 =
    Final_project.Match.set_match_odds match10 (Some (5.0, 6.0, 7.0))
  in
  assert_equal "(5.00, 7.00)" (Final_project.Match.match_odds match10);

  let match11 = Final_project.Match.make_match 11 "TeamO" "TeamP" in
  let match11 =
    Final_project.Match.set_match_odds match11 (Some (8.8, 7.7, 6.6))
  in
  assert_equal "(8.80, 6.60)" (Final_project.Match.match_odds match11);

  let match12 = Final_project.Match.make_match 12 "TeamQ" "TeamR" in
  let match12 =
    Final_project.Match.set_match_odds match12 (Some (0.05, 0.25, 0.75))
  in
  assert_equal "(0.05, 0.75)" (Final_project.Match.match_odds match12)

let test_a_side _ =
  (* Test 1: Match with "TeamA" vs "TeamB" *)
  let match1 = Final_project.Match.make_match 1 "TeamA" "TeamB" in
  assert_equal "TeamA" (Final_project.Match.a_side match1);

  (* Test 2: Match with "TeamX" vs "TeamY" *)
  let match2 = Final_project.Match.make_match 2 "TeamX" "TeamY" in
  assert_equal "TeamX" (Final_project.Match.a_side match2);

  (* Test 3: Match with "TeamM" vs "TeamN" *)
  let match3 = Final_project.Match.make_match 3 "TeamM" "TeamN" in
  assert_equal "TeamM" (Final_project.Match.a_side match3);

  (* Test 4: Match with "TeamW" vs "TeamZ" *)
  let match4 = Final_project.Match.make_match 4 "TeamW" "TeamZ" in
  assert_equal "TeamW" (Final_project.Match.a_side match4);

  (* Test 5: Match with "Alpha" vs "Beta" *)
  let match5 = Final_project.Match.make_match 5 "Alpha" "Beta" in
  assert_equal "Alpha" (Final_project.Match.a_side match5);

  (* Test 6: Match with "Red" vs "Blue" *)
  let match6 = Final_project.Match.make_match 6 "Red" "Blue" in
  assert_equal "Red" (Final_project.Match.a_side match6);

  (* Test 7: Match with "Green" vs "Yellow" *)
  let match7 = Final_project.Match.make_match 7 "Green" "Yellow" in
  assert_equal "Green" (Final_project.Match.a_side match7);

  (* Test 8: Match with "Team1" vs "Team2" *)
  let match8 = Final_project.Match.make_match 8 "Team1" "Team2" in
  assert_equal "Team1" (Final_project.Match.a_side match8);

  (* Test 9: Match with "SquadA" vs "SquadB" *)
  let match9 = Final_project.Match.make_match 9 "SquadA" "SquadB" in
  assert_equal "SquadA" (Final_project.Match.a_side match9);

  (* Test 10: Match with "TeamBlue" vs "TeamRed" *)
  let match10 = Final_project.Match.make_match 10 "TeamBlue" "TeamRed" in
  assert_equal "TeamBlue" (Final_project.Match.a_side match10);

  (* Test 11: Match with "East" vs "West" *)
  let match11 = Final_project.Match.make_match 11 "East" "West" in
  assert_equal "East" (Final_project.Match.a_side match11)

let test_b_side _ =
  (* Test 1: Match with "TeamA" vs "TeamB" *)
  let match1 = Final_project.Match.make_match 1 "TeamA" "TeamB" in
  assert_equal "TeamB" (Final_project.Match.b_side match1);

  (* Test 2: Match with "TeamX" vs "TeamY" *)
  let match2 = Final_project.Match.make_match 2 "TeamX" "TeamY" in
  assert_equal "TeamY" (Final_project.Match.b_side match2);

  (* Test 3: Match with "TeamM" vs "TeamN" *)
  let match3 = Final_project.Match.make_match 3 "TeamM" "TeamN" in
  assert_equal "TeamN" (Final_project.Match.b_side match3);

  (* Test 4: Match with "TeamW" vs "TeamZ" *)
  let match4 = Final_project.Match.make_match 4 "TeamW" "TeamZ" in
  assert_equal "TeamZ" (Final_project.Match.b_side match4);

  (* Test 5: Match with "Alpha" vs "Beta" *)
  let match5 = Final_project.Match.make_match 5 "Alpha" "Beta" in
  assert_equal "Beta" (Final_project.Match.b_side match5);

  (* Test 6: Match with "Red" vs "Blue" *)
  let match6 = Final_project.Match.make_match 6 "Red" "Blue" in
  assert_equal "Blue" (Final_project.Match.b_side match6);

  (* Test 7: Match with "Green" vs "Yellow" *)
  let match7 = Final_project.Match.make_match 7 "Green" "Yellow" in
  assert_equal "Yellow" (Final_project.Match.b_side match7);

  (* Test 8: Match with "Team1" vs "Team2" *)
  let match8 = Final_project.Match.make_match 8 "Team1" "Team2" in
  assert_equal "Team2" (Final_project.Match.b_side match8);

  (* Test 9: Match with "SquadA" vs "SquadB" *)
  let match9 = Final_project.Match.make_match 9 "SquadA" "SquadB" in
  assert_equal "SquadB" (Final_project.Match.b_side match9);

  (* Test 10: Match with "TeamBlue" vs "TeamRed" *)
  let match10 = Final_project.Match.make_match 10 "TeamBlue" "TeamRed" in
  assert_equal "TeamRed" (Final_project.Match.b_side match10);

  (* Test 11: Match with "East" vs "West" *)
  let match11 = Final_project.Match.make_match 11 "East" "West" in
  assert_equal "West" (Final_project.Match.b_side match11)

let test_to_string _ =
  (* Test 1: Match with "TeamA" vs "TeamB" *)
  let match1 = Final_project.Match.make_match 1 "TeamA" "TeamB" in
  let expected1 = "1/TeamA/TeamB" in
  assert_equal expected1 (Final_project.Match.to_string match1);

  (* Test 2: Match with "TeamX" vs "TeamY" *)
  let match2 = Final_project.Match.make_match 2 "TeamX" "TeamY" in
  let expected2 = "2/TeamX/TeamY" in
  assert_equal expected2 (Final_project.Match.to_string match2);

  (* Test 3: Match with "TeamM" vs "TeamN" *)
  let match3 = Final_project.Match.make_match 3 "TeamM" "TeamN" in
  let expected3 = "3/TeamM/TeamN" in
  assert_equal expected3 (Final_project.Match.to_string match3);

  (* Test 4: Match with "TeamW" vs "TeamZ" *)
  let match4 = Final_project.Match.make_match 4 "TeamW" "TeamZ" in
  let expected4 = "4/TeamW/TeamZ" in
  assert_equal expected4 (Final_project.Match.to_string match4);

  (* Test 5: Match with "Alpha" vs "Beta" *)
  let match5 = Final_project.Match.make_match 5 "Alpha" "Beta" in
  let expected5 = "5/Alpha/Beta" in
  assert_equal expected5 (Final_project.Match.to_string match5);

  (* Test 6: Match with "Red" vs "Blue" *)
  let match6 = Final_project.Match.make_match 6 "Red" "Blue" in
  let expected6 = "6/Red/Blue" in
  assert_equal expected6 (Final_project.Match.to_string match6);

  (* Test 7: Match with "Green" vs "Yellow" *)
  let match7 = Final_project.Match.make_match 7 "Green" "Yellow" in
  let expected7 = "7/Green/Yellow" in
  assert_equal expected7 (Final_project.Match.to_string match7);

  (* Test 8: Match with "Team1" vs "Team2" *)
  let match8 = Final_project.Match.make_match 8 "Team1" "Team2" in
  let expected8 = "8/Team1/Team2" in
  assert_equal expected8 (Final_project.Match.to_string match8);

  (* Test 9: Match with "SquadA" vs "SquadB" *)
  let match9 = Final_project.Match.make_match 9 "SquadA" "SquadB" in
  let expected9 = "9/SquadA/SquadB" in
  assert_equal expected9 (Final_project.Match.to_string match9);

  (* Test 10: Match with "TeamBlue" vs "TeamRed" *)
  let match10 = Final_project.Match.make_match 10 "TeamBlue" "TeamRed" in
  let expected10 = "10/TeamBlue/TeamRed" in
  assert_equal expected10 (Final_project.Match.to_string match10);

  (* Test 11: Match with "East" vs "West" *)
  let match11 = Final_project.Match.make_match 11 "East" "West" in
  let expected11 = "11/East/West" in
  assert_equal expected11 (Final_project.Match.to_string match11)

let test_of_string _ =
  let match_str = "1/TeamA/TeamB" in
  let match_ = Final_project.Match.of_string match_str in
  assert_equal 1 (Final_project.Match.match_id match_);
  assert_equal "TeamA" (Final_project.Match.a_side match_);
  assert_equal "TeamB" (Final_project.Match.b_side match_);
  assert_raises
    (Failure
       "Error occured with making match: 1/TeamA due to exception: \
        Failure(\"Invalid match string format\")") (fun () ->
      Final_project.Match.of_string "1/TeamA");
  assert_raises
    (Failure
       "Error occured with making match: 1/TeamA due to exception: \
        Failure(\"Invalid match string format\")") (fun () ->
      Final_project.Match.of_string "1/TeamA")

let test_match_odds_skewed _ =
  let a_results = [ (10.0, 0.0); (10.0, 0.0) ] in
  let b_results = [ (0.0, 10.0); (0.0, 10.0) ] in
  assert_equal "100 to 1"
    (Final_project.Bet_odds.match_odds a_results b_results);

  let a_results_2 =
    [ (0., 3.); (0., 2.); (0., 1.) ]
    (* Team A never scores *)
  in
  let b_results_2 =
    [ (3., 0.); (2., 0.); (1., 0.) ]
    (* Team B always scores more *)
  in
  let odds_2 = Final_project.Bet_odds.match_odds a_results_2 b_results_2 in
  assert_equal odds_2 "1 to 100"
    ~msg:"Expected '1 to 100' when Team A never wins";
  assert_equal odds_2 "1 to 100"
    ~msg:"Expected '1 to 100' when Team A never wins"

let test_match_odds_equal _ =
  let a_results = [ (1.0, 1.0) ] in
  let b_results = [ (1.0, 1.0) ] in
  assert_equal "1 to 1" (Final_project.Bet_odds.match_odds a_results b_results);

  let a_results_2 = [ (0., 0.); (0., 0.) ] in
  let b_results_2 = [ (0., 0.); (0., 0.) ] in
  assert_equal "1 to 1"
    (Final_project.Bet_odds.match_odds a_results_2 b_results_2);
  let a_results_2 = [ (0., 0.); (0., 0.) ] in
  let b_results_2 = [ (0., 0.); (0., 0.) ] in
  assert_equal "1 to 1"
    (Final_project.Bet_odds.match_odds a_results_2 b_results_2)

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
  (* This corresponds to 2021-01-01 *)
  let timestamp1 = 1609459200.0 in
  (* This corresponds to 2021-01-01 *)
  let result1 = Final_project.Api_handling.format_date timestamp1 in
  assert_equal "2021-01-01" result1

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

let test_calculate_win _ =
  let odds1 = (1., 1.) in
  let odds2 = (1., 2.) in
  let game = Final_project.Match.make_match 007 "usa" "romania" in
  let bet1 = Final_project.Bet.make_bet game "romania " 5. in
  let bet2 = Final_project.Bet.make_bet game "usa" 5. in
  let win_amount1 = Final_project.User.calculate_win odds1 bet1 in
  let win_amount2 = Final_project.User.calculate_win odds2 bet1 in
  let win_amount3 = Final_project.User.calculate_win odds1 bet2 in
  let win_amount4 = Final_project.User.calculate_win odds2 bet2 in
  assert_equal 5. win_amount1;
  assert_equal 10. win_amount2;
  assert_equal 5. win_amount3;
  assert_equal 5. win_amount4

let test_match_odds_tuple_helper _ =
  let odds1 = Some (2., 1., 3.) in
  let odds2 = None in
  let result1 = Final_project.Match.match_odds_tuple_helper odds1 in
  let result2 = Final_project.Match.match_odds_tuple_helper odds2 in
  assert_equal (2., 3.) result1;
  assert_equal (1., 1.) result2

let test_match_odds_tuple _ =
  let game = Final_project.Match.make_match 007 "usa" "romania" in
  assert_equal (1., 1.) (Final_project.Match.match_odds_tuple game)

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
         "test_change_balance_fractional_precision"
         >:: test_change_balance_fractional_precision;
         "test_change_balance_round_trip" >:: test_change_balance_round_trip;
         "test_change_balance_zero_check_after_multiple_ops"
         >:: test_change_balance_zero_check_after_multiple_ops;
         "test_change_balance_multiple_fractions"
         >:: test_change_balance_multiple_fractions;
         "test_change_balance_substantial_overdraw"
         >:: test_change_balance_substantial_overdraw;
         "test_change_balance_positive_alternative1"
         >:: test_change_balance_positive_alternative1;
         "test_change_balance_positive_alternative2"
         >:: test_change_balance_positive_alternative2;
         "test_change_balance_positive_variant1"
         >:: test_change_balance_positive_variant1;
         "test_change_balance_positive_variant2"
         >:: test_change_balance_positive_variant2;
         "test_change_balance_negative_alternative1"
         >:: test_change_balance_negative_alternative1;
         "test_change_balance_negative_alternative2"
         >:: test_change_balance_negative_alternative2;
         "test_change_balance_negative_variant1"
         >:: test_change_balance_negative_variant1;
         "test_change_balance_negative_variant2"
         >:: test_change_balance_negative_variant2;
         "test_change_balance_zero_alternative1"
         >:: test_change_balance_zero_alternative1;
         "test_change_balance_zero_alternative2"
         >:: test_change_balance_zero_alternative2;
         "test_change_balance_zero_variant1"
         >:: test_change_balance_zero_variant1;
         "test_change_balance_zero_variant2"
         >:: test_change_balance_zero_variant2;
         "test_change_balance_multiple_operations_alternative1"
         >:: test_change_balance_multiple_operations_alternative1;
         "test_change_balance_multiple_operations_alternative2"
         >:: test_change_balance_multiple_operations_alternative2;
         "test_change_balance_multiple_operations_variant1"
         >:: test_change_balance_multiple_operations_variant1;
         "test_change_balance_multiple_operations_variant2"
         >:: test_change_balance_multiple_operations_variant2;
         "test_change_balance_large_positive_variant1"
         >:: test_change_balance_large_positive_variant1;
         "test_change_balance_large_positive_variant2"
         >:: test_change_balance_large_positive_variant2;
         "test_change_balance_large_negative_variant1"
         >:: test_change_balance_large_negative_variant1;
         "test_change_balance_large_negative_variant2"
         >:: test_change_balance_large_negative_variant2;
         "test_change_balance_exact_balance_deduction_variant1"
         >:: test_change_balance_exact_balance_deduction_variant1;
         "test_change_balance_exact_balance_deduction_variant2"
         >:: test_change_balance_exact_balance_deduction_variant2;
         "test_change_balance_small_increments_variant1"
         >:: test_change_balance_small_increments_variant1;
         "test_change_balance_small_increments_variant2"
         >:: test_change_balance_small_increments_variant2;
         "test_change_balance_small_decrements_variant2"
         >:: test_change_balance_small_decrements_variant2;
         "test_change_balance_negative_overdraw"
         >:: test_change_balance_negative_overdraw;
         "test_change_balance_small_fractional_increments"
         >:: test_change_balance_small_fractional_increments;
         "test_change_balance_small_fractional_decrements"
         >:: test_change_balance_small_fractional_decrements;
         "test_change_balance_large_sequence"
         >:: test_change_balance_large_sequence;
         "test_change_balance_multiple_small_increments_and_decrements"
         >:: test_change_balance_multiple_small_increments_and_decrements;
         "test_change_balance_no_change_after_zero_operations"
         >:: test_change_balance_no_change_after_zero_operations;
         "test_change_balance_rounding" >:: test_change_balance_rounding;
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
         "test_calculate_win" >:: test_calculate_win;
         "test_match_odds_tuple_helper" >:: test_match_odds_tuple_helper;
         "test_match_odds_tuple" >:: test_match_odds_tuple;
       ]

let () = run_test_tt_main tests
