let spin_lottery () : float =
  Random.self_init (); (* Initialize the random number generator *)

  (* Generate a normally distributed number with mean 0 and standard deviation 10 *)
  let mean = 0.0 in
  let stddev = 10.0 in
  let u1 = Random.float 1.0 in
  let u2 = Random.float 1.0 in
  let z0 = sqrt (-2.0 *. log u1) *. cos (2.0 *. Float.pi *. u2) in
  let bonus = mean +. stddev *. z0 in

  (* Ensure the bonus is within the range of -20 to 20 *)
  let clamped_bonus = max (-20.0) (min 20.0 bonus) in

  clamped_bonus