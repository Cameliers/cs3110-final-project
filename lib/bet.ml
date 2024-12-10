type t = {
  game : Match.t;
  team : string;
  amount : float;
}

let bet_amount bet = bet.amount
let bet_team bet = bet.team
let bet_game bet = bet.game
let make_bet game team amount = { game; team; amount }
let to_string bet = 
  let game_str = Match.to_string bet.game in
  "Game:" ^ game_str ^ ", Team:" ^ bet.team ^ ", Amount: " ^ (string_of_float bet.amount)
