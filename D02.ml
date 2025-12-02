let file = "two.txt"

let read_lines name =
	let ic = open_in name in
	let try_read () =
	  try Some (input_line ic) with End_of_file -> None in
	let rec loop acc = match try_read () with
	  | Some s -> loop (s :: acc)
	  | None -> close_in ic; List.rev acc in
	loop [];;

let lines = read_lines file;;

let isSameSignOrZero x y =
  (x >= 0 && y >= 0) || (x <= 0 && y <= 0);;

let isStable = function
| [] -> false
| head :: tail -> let rec helper dir prev = function
    | [] -> true
    | h :: t -> if 
        (not (isSameSignOrZero (h - prev) dir)) || 
        abs(h - prev) > 3 ||
        abs(h - prev) < 1 
        then false
        else helper (h - prev) h t
  in helper 0 head tail;;

let safe = List.fold_left (
  fun acc line ->
    let list = String.split_on_char ' ' line |> List.filter (fun s -> s <> "") |> List.map int_of_string in
    if isStable list then acc + 1 else acc
) 0 lines;;

isStable [32; 32; 34; 38; 39];;
isStable [32; 32; 34; 38; 39];;

let cutOneAtID list id = 
  let rec helper count acc = function
    | [] -> acc
    | h :: t -> if count = id then helper (count + 1) acc t else helper (count + 1) (h :: acc) t
  in List.rev (helper 0 [] list);;

let findStable orgList =
  let rec helper count =
    if count > List.length orgList then false else
    if isStable (cutOneAtID orgList count) then true else helper (count + 1)
  in helper 0;;

  cutOneAtID [32; 32; 35; 38; 39] 1;;
findStable (cutOneAtID [32; 32; 35; 38; 39] 1);;

let safer = List.fold_left (
  fun acc line ->
    let list = String.split_on_char ' ' line |> List.filter (fun s -> s <> "") |> List.map int_of_string in
    if findStable list then acc + 1 else acc
) 0 lines;;



(* Larger than 348, less than 450 not 332 *)