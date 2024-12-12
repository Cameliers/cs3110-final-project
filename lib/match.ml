open Api_handling

type t = {
  id : int;
  a_side : string;
  b_side : string;
  match_odds : (float * float * float) option;
}

let match_id x = x.id
let a_side x = x.a_side
let b_side x = x.b_side

let odds_to_string = function
  | None -> "None"
  | Some (x, y, z) -> Printf.sprintf "(%.2f, %.2f, %.2f)" x y z

let calculate_odds id = get_match_winner_odds id
let match_odds x = odds_to_string x.match_odds

let set_match_odds t odds =
  { t with match_odds = odds }

let make_match match_id a b =
  {
    id = match_id;
    a_side = a;
    b_side = b;
    match_odds = calculate_odds match_id;
  }

let to_string match_ =
  string_of_int match_.id ^ "/" ^ match_.a_side ^ "/" ^ match_.b_side

(* Function to convert a string like "TeamA vs TeamB (Odds: 2.5:1)" back into a
   Match.t *)
let of_string str =
  try
    (* Split on " vs " to separate the two teams *)
    let teams_and_odds = String.split_on_char '/' str in
    match teams_and_odds with
    | [ id; a_side; b_side ] -> make_match (int_of_string id) a_side b_side
    | _ -> failwith "Invalid match string format"
  with exn ->
    failwith
      ("Error occured with making match: " ^ str ^ " due to exception: "
     ^ Printexc.to_string exn)
