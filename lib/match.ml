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
  (string_of_int match_.id) ^ "/" ^ match_.a_side ^ "/" ^ match_.b_side ^ "/(Odds:" ^ match_.match_odds ^ ")"

(* Function to convert a string like "TeamA vs TeamB (Odds: 2.5:1)" back into a
   Match.t *)
let of_string str =
  try
    (* Split on " vs " to separate the two teams *)
    let teams_and_odds = String.split_on_char '/' str in
    match teams_and_odds with
    | [id; a_side; b_side; odds_with_paren] -> make_match (int_of_string id) a_side b_side odds_with_paren
    | _ -> failwith "Invalid match string format"
  with exn -> 
    failwith ("Error occured with making match: " ^ str ^ " due to exception: " ^ Printexc.to_string exn)
