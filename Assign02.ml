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


(* Problem 7 Solution *)

(λt.λu.u t t)(λx.x)t
  (* Step 1: Substitute (λx.x) for t *)
  λu.u(λx.x)(λx.x)
  (* Step 2: Apply t to the expression *)
  λu.u(λx.x)(λx.x)t
  (* Step 3: Replace u with t *)
  t(λx.x)(λx.x)
  (* Conclusion *)
    (* The normal form exists if we interpret t as a variable *)


(* Problem 8 Solution *)

(λz. z z z)(λy. y y)
  (* Step 1: Apply the outer λ expression *)
  (λy.y y)(λy.y y)(λy.y y)
  (* Step 2: Evaluate the expression *)
    (* Apply (λy.y y) to itself *)
    (λy.y y)(λy.y y)
  (* Step 3: Evaluate the second application *)
    (* Apply (λy.y y) to itself *)
    (λy.y y)(λy.y y)
  (* Conclusion *)
    (* Each application of (λy.y y) to itself continues to produce the same structure, which means the expression does not reach a normal form.*)


(* Problem 9 Solution *)

(λx.x x)(λx.y)
  (* Step 1: Apply the outer λ expression *)
  (λx.y)(λx.y)
  (* Step 2: Evaluate the expression *)
  y
  (* Conclusion *)
    (* Since y is a simple variable and not a function, we have reached normal *)


(* Problem 10 Solution *)

(λz.z t)(λz.(λt.t z)(λx.z x))
  (* Step 1: Apply the outer λ expression *)
  (λz.(λt.t z)(λx.z x))t
  (* Step 2: Substitute the inner expression *)
  t(λz.(λt.t z)(λx.z x))
  (* Step 3: Expand the inner expression *)
  (λz.(λt.t z)(λx.z x))
  (* Step 4: Analyze the inner expression *)
  t(λz.(λt.t z)(λx.z x))
  (* Conclusion *)
    (* The normal form cannot be determined without further context or application *)


(* Problem 11 Solution *)

int_to_list : int -> int list


(* Problem 12 Solution *)

divisors :: Int -> [Int]
divisors n = [d | d <- [1..n], n `mod` d == 0]


(* Problem 13 Solution *)

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = divisors n == [1, n]

divisors :: Int -> [Int]
divisors n = [d | d <- [1..n], n `mod` d == 0]


(* Problem 14 Solution *)

primes :: [Int]
primes = filter isPrime [2..]

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = divisors n == [1, n]

divisors :: Int -> [Int]
divisors n = [d | d <- [1..n], n `mod` d == 0]


(* Problem 15 Solution *)
primes :: [Int]
primes = filter isPrime [2..]

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = divisors n == [1, n]

divisors :: Int -> [Int]
divisors n = [d | d <- [1..n], n `mod` d == 0]

main :: IO ()
main = print (primes !! 1999)  -- Output: 17389


(* Problem 16 Solution *) 

perfs :: Int -> [Int]
perfs n = [x | x <- [1..n], isPerfect x]

isPerfect :: Int -> Bool
isPerfect x = sum (divisors x) == x

divisors :: Int -> [Int]
divisors n = [d | d <- [1..(n-1)], n `mod` d == 0]

main :: IO ()
main = print (perfs 10000)  -- Example usage




