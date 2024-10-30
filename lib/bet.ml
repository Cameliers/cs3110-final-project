type t = {
  game : Match.t;
  team : string;
  amount : float;
}

let bet_amount bet = bet.amount
let bet_team bet = bet.team
let bet_game bet = bet.game
let make_bet game team amount = { game; team; amount }
