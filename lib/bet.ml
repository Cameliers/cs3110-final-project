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
  game_str ^ "," ^ bet.team ^ "," ^ (string_of_float bet.amount)

let of_string str =
  match String.split_on_char ',' str with
  | [game_str; team; amount] -> 
    let game = Match.of_string game_str in
    let amount = float_of_string amount in
    make_bet game team amount
  | _ -> failwith "Invalid bet string format"
