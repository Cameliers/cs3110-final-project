exception InvalidData

(* Compute average from a list of floats *)
let average lst =
  if lst = [] then raise InvalidData;
  let sum = List.fold_left ( +. ) 0.0 lst in
  sum /. float_of_int (List.length lst)

(* Given a list of (goals_for, goals_against), compute average goals_for and
   goals_against *)
let avg_for_against results =
  if results = [] then raise InvalidData;
  let gf = List.map fst results in
  let ga = List.map snd results in
  (average gf, average ga)

(* Poisson PMF *)
let poisson_pmf lambda k =
  let rec fact n = if n <= 1 then 1 else n * fact (n - 1) in
  exp (-.lambda) *. (lambda ** float_of_int k) /. float_of_int (fact k)

(* Return odds as a string like "3 to 1" or "1 to 2" *)
let match_odds (a_results : (float * float) list)
    (b_results : (float * float) list) : string =
  let a_for, a_against = avg_for_against a_results in
  let b_for, b_against = avg_for_against b_results in

  (* Simple estimation for lambdas *)
  let lambda_a = (a_for +. b_against) /. 2.0 in
  let lambda_b = (b_for +. a_against) /. 2.0 in

  let max_goals = 5 in
  let p_a_win = ref 0.0 in
  let p_b_win = ref 0.0 in

  for ga = 0 to max_goals do
    for gb = 0 to max_goals do
      let p_ga = poisson_pmf lambda_a ga in
      let p_gb = poisson_pmf lambda_b gb in
      let joint_p = p_ga *. p_gb in
      if ga > gb then p_a_win := !p_a_win +. joint_p
      else if ga < gb then p_b_win := !p_b_win +. joint_p
    done
  done;

  let a = !p_a_win in
  let b = !p_b_win in

  if b = 0.0 && a > 0.0 then
    (* If B's probability is essentially zero, we can say something like "∞ to
       1". But since we need an integer ratio, we can just say something like
       "100 to 1" to represent a very skewed ratio. *)
    "100 to 1"
  else if a = 0.0 && b > 0.0 then (* If A never wins: "1 to 100" *)
    "1 to 100"
  else if a = 0.0 && b = 0.0 then
    (* Very unlikely scenario with this model, but let's handle it. Means no
       difference. *)
    "1 to 1"
  else begin
    (* Convert ratio a : b to a nice integer ratio *)
    let ratio = a /. b in
    (* We'll scale and simplify *)
    let scale = 100 in
    let ai = int_of_float (ratio *. float_of_int scale) in
    let bi = scale in
    let rec gcd x y = if y = 0 then x else gcd y (x mod y) in
    let g = gcd ai bi in
    let ai = ai / g in
    let bi = bi / g in
    Printf.sprintf "%d to %d" ai bi
  end

(* Example usage: let odds = match_odds_ratio [(2.,1.);(1.,1.);(3.,2.)]
   [(1.,0.);(2.,2.);(0.,1.)] (* odds might be something like "3 to 2" *) *)
