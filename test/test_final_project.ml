open OUnit2

let test_make_user _ =
  let user = Final_project.User.make_user () in
  assert_equal 1000.0 (Final_project.User.balance user);
  assert_equal [] (Final_project.User.bets_active user);
  assert_equal [] (Final_project.User.bets_history user)

let test_change_balance _ =
  let user = Final_project.User.make_user () in
  Final_project.User.change_balance user 500.0;
  assert_equal 1500.0 (Final_project.User.balance user);
  Final_project.User.change_balance user (-200.0);
  assert_equal 1300.0 (Final_project.User.balance user)

(**match Final_project.User.bets_active user with | [ { game; team; amount } ]
   -> assert_equal "TeamA" team; assert_equal 100.0 amount; assert_equal "TeamA"
   (Match.a_side game); assert_equal "TeamB" (Match.b_side game); assert_equal
   "2:1" (Match.match_odds game) | _ -> assert_failure "Expected one active bet"*)
let test_add_bet _ =
  let match1 = Final_project.Match.make_match 10 "TeamA" "TeamB" "2:1" in
  let user = Final_project.User.make_user () in
  Final_project.User.add_bet user match1 "TeamA" 100.0;
  assert_equal 900.0 (Final_project.User.balance user);
  assert_equal 1 (List.length (Final_project.User.bets_active user));
  assert_equal 1 (List.length (Final_project.User.bets_history user))

let test_bets_active_and_history _ =
  let match1 = Final_project.Match.make_match 10 "TeamA" "TeamB" "2:1" in
  let match2 = Final_project.Match.make_match 10 "TeamC" "TeamD" "3:2" in
  let user = Final_project.User.make_user () in
  Final_project.User.add_bet user match1 "TeamA" 100.0;
  Final_project.User.add_bet user match2 "TeamC" 200.0;
  assert_equal 2 (List.length (Final_project.User.bets_active user));
  assert_equal 2 (List.length (Final_project.User.bets_history user));
  assert_equal 700.0 (Final_project.User.balance user)

let test_match_creation _ =
  let match1 = Final_project.Match.make_match 10 "TeamA" "TeamB" "2:1" in
  assert_equal "TeamA" (Final_project.Match.a_side match1);
  assert_equal "TeamB" (Final_project.Match.b_side match1);
  assert_equal "2:1" (Final_project.Match.match_odds match1)

let test_bet_creation _ =
  let match1 = Final_project.Match.make_match 10 "TeamA" "TeamB" "2:1" in
  let bet = Final_project.Bet.make_bet match1 "TeamA" 50.0 in
  assert_equal 50.0 (Final_project.Bet.bet_amount bet);
  assert_equal "TeamA" (Final_project.Bet.bet_team bet);
  assert_equal "TeamA"
    (Final_project.Match.a_side (Final_project.Bet.bet_game bet));
  assert_equal "TeamB"
    (Final_project.Match.b_side (Final_project.Bet.bet_game bet));
  assert_equal "2:1"
    (Final_project.Match.match_odds (Final_project.Bet.bet_game bet))

let tests =
  "test_user_module"
  >::: [
         "test_make_user" >:: test_make_user;
         "test_change_balance" >:: test_change_balance;
         "test_add_bet" >:: test_add_bet;
         "test_bets_active_and_history" >:: test_bets_active_and_history;
         "test_match_creation" >:: test_match_creation;
         "test_bet_creation" >:: test_bet_creation;
       ]

let _ = run_test_tt_main tests
