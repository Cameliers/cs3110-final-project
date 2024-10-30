type t = {
  a_side : string;
  b_side : string;
  match_odds : string;
}

let a_side x = x.a_side
let b_side x = x.b_side
let match_odds x = x.match_odds
let make_match a b odds = { a_side = a; b_side = b; match_odds = odds }
