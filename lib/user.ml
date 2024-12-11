open Api_handling
open Bet
open Match

type t = {
  mutable balance : float;
  mutable bets_active : Bet.t list;
  mutable bets_history : Bet.t list;
  mutable balance_history : float list * float;
}

let make_user () =
  {
    balance = 1000.0;
    bets_active = [];
    bets_history = [];
    balance_history = ([ 1000.0 ], 0.);
  }

let create balance active_bets bet_history =
  {
    balance;
    bets_active = active_bets;
    bets_history = bet_history;
    balance_history = ([ balance ], 0.);
  }

let balance t = t.balance
let bets_active t = t.bets_active
let bets_history t = t.bets_history

let change_balance t change =
  let new_balance_list = fst t.balance_history @ [ t.balance +. change ] in
  let overall_gains =
    match new_balance_list with
    | h :: _ -> t.balance +. change -. h
    | _ -> failwith "bruh"
  in
  t.balance_history <- (new_balance_list, overall_gains);
  t.balance <- t.balance +. change

let balance_history t = t.balance_history

let add_bet t game team amount =
  let bet = Bet.make_bet game team amount in
  t.bets_active <- bet :: t.bets_active;
  change_balance t (-1. *. amount)

let complete_bets t =
  List.iter
    (fun bet ->
      let bet_match = bet_game bet in
      let id = match_id bet_match in
      let result = get_match_result id in
      match result with
      | "Draw" ->
          t.bets_active <-
            List.filter
              (fun bet ->
                let bet_match = bet_game bet in
                let filter_id = match_id bet_match in
                if id = filter_id then false else true)
              t.bets_active;
          t.bets_history <- bet :: t.bets_history
      | "Not Finished" -> ()
      | "Cancelled" ->
          change_balance t (bet_amount bet);
          t.bets_active <-
            List.filter
              (fun bet ->
                let bet_match = bet_game bet in
                let filter_id = match_id bet_match in
                if id = filter_id then false else true)
              t.bets_active;
          t.bets_history <- bet :: t.bets_history
      | "Unknown Status" -> ()
      | team ->
          if team = bet_team bet then change_balance t (2. *. bet_amount bet)
          else ();
          t.bets_active <-
            List.filter
              (fun bet ->
                let bet_match = bet_game bet in
                let filter_id = match_id bet_match in
                if id = filter_id then false else true)
              t.bets_active;
          t.bets_history <- bet :: t.bets_history)
    t.bets_active
