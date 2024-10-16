(* Problem 1 Solution *)

(*Non-tail recursive*)
let rec replicate_non_tail x n =
  if n < 0 then
    invalid_arg "Negative" 
  else if n = 0 then
    []
  else
    x :: replicate_non_tail x (n-1);;


(*Tail-recursive*)
let rec replicate_tail x n =
  let rec helper n acc =
    if n < 0 then
      invalid_arg "Negative count"
    else if n = 0 then
      acc
    else
      helper (n-1) (x :: acc)
  in
  helper n [];;


(* Problem 2 Solution *)

(*Non-tail recursive*)
let rec count_non_tail n  =
  if n < 0 then
    invalid_arg "Negative"
  else if n = 0 then
    []
  else
    count_non_tail (n-1) @ [n];;
  

(*Tail-recursive*)
let rec count_tail n = 
  let rec helper n acc =
    if n < 0 then
      invalid_arg "Negative"
    else if n = 0 then
      acc
    else
      helper (n-1) (n :: acc)

    in 
    helper n [];;


(* Problem 3 Solution *)

(*Non-tail recursive*)

