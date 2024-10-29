type t = {
  balance : float;
  bets_active : int list;
  bets_history : int list;
  auto_bet : bool;
}

let make () =
  { balance = 0.0; bets_active = []; bets_history = []; auto_bet = false }

let balance t = t.balance
let bets_active t = t.bets_active
let bets_history t = t.bets_history
let change_balance t change = t.balance = t.balance +. change

let auto_bet t =
  match t.auto_bet with
  | true -> false
  | false -> true

let bet t match_id amount team = failwith "Bet.make_bet match_id team amount"
