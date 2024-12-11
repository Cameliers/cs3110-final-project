type t = {
  id : int;
  a_side : string;
  b_side : string;
  match_odds : string;
}

let match_id x = x.id
let a_side x = x.a_side
let b_side x = x.b_side
let match_odds x = x.match_odds
let make_match id a b odds = { id; a_side = a; b_side = b; match_odds = odds }

let to_string match_ =
  match_.a_side ^ "|vs|" ^ match_.b_side ^ "|(Odds: " ^ match_.match_odds ^ ")"

(* Function to convert a string like "TeamA vs TeamB (Odds: 2.5:1)" back into a
   Match.t *)
let of_string str =
  try
    (* Split on " vs " to separate the two teams *)
    let teams_and_odds = String.split_on_char '-' str in
    match teams_and_odds with
    | [ a_side; "vs"; b_side; "(Odds:"; odds_with_paren ] ->
        (* Remove the closing parenthesis from the odds string *)
        let clean_odds =
          String.sub odds_with_paren 0 (String.length odds_with_paren - 1)
        in
        make_match 10 a_side b_side clean_odds
    | _ -> failwith "Invalid match string format"
  with _ -> failwith ("Error occured with making match: " ^ str)
