type t = {
  bet_id : int;
  match_id : int;
  amount : int;
}

let bet_id bet = bet.bet_id
let match_id bet = bet.match_id
let bet_amount bet = bet.amount
let make_bet bet_id match_id amount = { bet_id; match_id; amount }
