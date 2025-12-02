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

let getMiddle lst = 
  let rec helper l1 l2 =
    match l2 with
    | [] -> List.hd l1
    | [_] -> List.hd l1
    | _ :: _ :: t -> helper (List.tl l1) t
  in helper lst lst;;

getMiddle [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10];;

let rec isOk prev update =
  match update with
  | [] -> true
  | head :: tail ->
    let rule = List.nth rules head in
    if List.fold_left (fun acc x -> 
      if acc then true else List.exists (fun y -> x = y) prev
    ) false rule
    then false else isOk (head :: prev) tail;;

let putIn rule res x =
  let rec helper re =
    match re with
    | [] -> [x]
    | head :: tail -> if List.exists (fun y -> head = y) rule then x :: re else head :: helper tail
  in helper res;;

let rec fixUpdate res update =
  match update with
  | [] -> res
  | head :: tail ->
    let rule = List.nth rules head in
    fixUpdate (putIn rule res head) tail;
  ;;


List.fold_left (fun acc ll -> 
  if isOk [] ll then acc + getMiddle ll else acc
) 0 updates;;

List.fold_left (fun acc ll -> 
  if not (isOk [] ll) then acc + getMiddle (fixUpdate [] ll) else acc
) 0 updates;;
