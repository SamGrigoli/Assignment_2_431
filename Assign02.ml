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
let rec reverse_non_tail lst =
  match lst with
  | [] -> []
  | [a'] -> [a']
  | hd :: tl -> reverse_non_tail tl @ [hd];;


(*Tail-recursive*)
let reverse_tail lst =
  let rec helper lst acc =
    match lst with
    | [] -> acc
    | hd :: tl -> helper tl (hd :: acc)
  in
  helper lst [];;



(* Problem 4 Solution *)

type 'a btree = 
| Empty
| Leaf of 'a
| Node of 'a btree * 'a * 'a btree;;

let rec count_leaves tree =
  match tree with
  | Empty -> 0
  | Leaf _ -> 1
  | Node (left, _, right) -> count_leaves left + count_leaves right;;



(* Problem 5 Solution *)

let rec in_order tree =
  match tree with
  | Empty -> []
  | Leaf a -> [a]
  | Node (left, a, right) -> in_order left @ [a] @ in_order right;;



(* Problem 6 Solution *)

let rec pre_order tree =
  match tree with
  | Empty -> []
  | Leaf a -> [a]
  | Node (left, a, right) -> [a] @ pre_order left @ pre_order right;;



