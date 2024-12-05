let read_lines name =
	let ic = open_in name in
	let try_read () =
	  try Some (input_line ic) with End_of_file -> None in
	let rec loop acc = match try_read () with
	  | Some s -> loop (s :: acc)
	  | None -> close_in ic; List.rev acc in
	loop [];;

let rawLines = read_lines "5.txt";;
let (rulesRaw, _ :: updatesRaw) = List.partition (fun x -> String.contains x '|') rawLines;;

let addRule rls id value =
  let rec helper count = function
    | [] -> []
    | head :: tail -> if count = id then (value :: head) :: tail else head :: helper (count + 1) tail
in helper 0 rls;;

let bp = List.init 100 (fun x -> []);;

let rules = List.fold_left (fun acc x -> 
  let [first; second] = String.split_on_char '|' x in
  addRule acc (int_of_string first) (int_of_string second)
) bp rulesRaw;;

let updates = List.map (fun x -> 
  String.split_on_char ',' x |> List.map int_of_string
) updatesRaw;;

let getMiddle update = 
  let rec helper l1 l2 =
    let h1 :: t1 = l1 in
    let _ :: _ :: t2 = l2 in
    
  in helper update update;;


let isOk = function
  | [] -> true
  | head :: tail -> 
    