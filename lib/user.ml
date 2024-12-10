type t = {
  mutable balance : float;
  mutable bets_active : Bet.t list;
  mutable bets_history : Bet.t list;
}

let make_user () = { balance = 1000.0; bets_active = []; bets_history = [] }
let create balance active_bets bet_history = { balance=balance; bets_active = active_bets; bets_history=bet_history}
let balance t = t.balance
let bets_active t = t.bets_active
let bets_history t = t.bets_history
let change_balance t change = t.balance <- t.balance +. change

let add_bet t game team amount =
  let bet = Bet.make_bet game team amount in
  t.bets_active <- bet :: t.bets_active;
  t.bets_history <- bet :: t.bets_history;
  t.balance <- t.balance -. amount

let complete_bets t =
  t.balance <- t.balance;
  t.bets_active <- t.bets_active;
  t.bets_history <- t.bets_history;
  failwith "TODO"
