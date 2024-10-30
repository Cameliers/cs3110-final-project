type t = {
  bet_id : int;
  match_id : int;
  team : string;
  amount : float;
}

let bet_id bet = bet.bet_id
let match_id bet = bet.match_id
let bet_amount bet = bet.amount
let bet_team bet = bet.team
let calculate_bet_id match_id = match_id

let make_bet match_id team amount =
  { bet_id = calculate_bet_id match_id; match_id; team; amount }
