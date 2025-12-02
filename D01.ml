let file = "one.txt"

let read_lines name =
	let ic = open_in name in
	let try_read () =
	  try Some (input_line ic) with End_of_file -> None in
	let rec loop acc = match try_read () with
	  | Some s -> loop (s :: acc)
	  | None -> close_in ic; List.rev acc in
	loop [];;


let split_two lines =
	let rec helper l1 l2 list =
		match list with
		[] -> (l1, l2)
		| head :: tail -> let [h1; h2] = head |> String.split_on_char ' ' |> List.filter (fun s -> s <> "") in 
							helper (int_of_string h1 :: l1) (int_of_string h2 :: l2) tail;
in helper [] [] lines;;

	
let raw_list = read_lines file;;
let (list1, list2) = split_two raw_list;;

let compare a b = if a = b then 0 else if a > b then 1 else -1;;

let sorted1 = List.sort compare list1;;
let sorted2 = List.sort compare list2;;

let rec solveFirst acc l1 l2 =
	match l1 with
	| [] -> acc
	| h1 :: t1 -> let h2 :: t2 = l2 in 
					solveFirst (acc + abs (h1 - h2)) t1 t2;;

solveFirst 0 sorted1 sorted2;;

let rec parseTo list value count =
	match list with
	[] -> (list, count)
	| h :: t -> if h > value then (list, count) else parseTo t value (count + (if h = value then 1 else 0));;

let rec solveSec acc l1 l2 = 
	match l1 with
	[] -> acc
	| h :: t -> let (newL2, count) = parseTo l2 h 0 in 
				let (_, multiplier) = parseTo l1 h 0 in
				solveSec (acc + multiplier * h * count) t newL2;;

solveSec 0 sorted1 sorted2;;