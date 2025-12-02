let parseInput rawLines = 
  List.map (fun x -> 
    match String.split_on_char ':' x with
    | [res; values] -> 
      let vv = String.split_on_char ' ' values |> List.filter (fun x -> x <> "") |> List.map int_of_string in
      (int_of_string res, vv)
    | _ -> failwith "invalid input"
  ) rawLines;;

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

(* operators is a list of booleansn that represent bool int *)
(* increment it by getting  next boolean combination*)
let nextBooleanOperator operators = 
  let rec inner acc = function
    | [] -> acc
    | h :: t -> if h = true then inner (false :: acc) t else (List.rev t) @ [true] @ (List.rev acc)
  in inner [] (List.rev operators);;

nextBooleanOperator [false; true; false; true; true];;

let isDoable (res, values) = 
  let max = pow 2 (List.length values - 1) in
  let rec helper count operators = 
    if count > max then false else
    let sum = List.fold_left2 (fun acc a b -> if b = true then acc * a else acc + a) (List.hd values) (List.tl values) operators in
    if sum = res then true else helper (count + 1) (nextBooleanOperator operators)
  in helper 0 (List.init (List.length values - 1) (fun _ -> false));;

  isDoable (48356500, [63; 602; 3; 2; 85; 5]);;
  pow 2 5;;


let nextTripleOperator operators = 
  let rec inner acc = function
    | [] -> acc
    | h :: t -> if h = 2 then inner (0 :: acc) t else (List.rev t) @ [h + 1] @ (List.rev acc)
  in inner [] (List.rev operators);;

let isDoable2 (res, values) = 
  let max = pow 3 (List.length values - 1) in
  let rec helper count operators = 
    if count > max then false else
    let sum = List.fold_left2 (fun acc a b -> 
      match b with
      | 0 -> acc + a
      | 1 -> acc * a
      | _ -> int_of_string ((string_of_int acc) ^ (string_of_int a))
      ) (List.hd values) (List.tl values) operators in
    if sum = res then true else helper (count + 1) (nextTripleOperator operators)
  in helper 0 (List.init (List.length values - 1) (fun _ -> 0));;


let solve rawLines = 
  let lines = parseInput rawLines in
  (
    List.fold_left (fun acc (res, values) -> 
      if isDoable (res, values) then res + acc else acc
    ) 0 lines, 
    List.fold_left (fun acc (res, values) -> 
      if isDoable2 (res, values) then res + acc else acc
    ) 0 lines
  );;

(*   is       136762151777 *)
(* should be 4122618559853 *)
