let read_lines name =
	let ic = open_in name in
	let try_read () =
	  try Some (input_line ic) with End_of_file -> None in
	let rec loop acc = match try_read () with
	  | Some s -> loop (s :: acc)
	  | None -> close_in ic; List.rev acc in
	loop [];;

let string_to_char_list s =
    s |> String.to_seq |> List.of_seq
let stringLines = read_lines "4.txt";;
let listLines = List.map string_to_char_list stringLines;;

let word = ['X'; 'M'; 'A'; 'S'];;
let directions = [(-1, -1); (0, -1); (1, -1); (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0)];;


let countXMAS lines =
  let maxY = List.length lines in
  let maxX = List.length (List.hd lines) in
  let getChar x y = 
    if x < 0 || x >= maxX || y < 0 || y >= maxY then None else Some(List.nth (List.nth lines y) x) in
  let search x y =
      let rec beam a b count (dx, dy) =
        if count = List.length word then 1 else
          let nextX = a + dx in
          let nextY = b + dy in
          if getChar nextX nextY = List.nth_opt word count then beam nextX nextY (count + 1) (dx, dy) else 0
   in List.fold_left (fun acc (dx, dy) -> acc + beam x y 1 (dx, dy)) 0 directions 

  in
  let rec helper acc x y = function
    | [] -> acc
    | head :: tail -> match head with
        | [] -> helper acc 0 (y + 1) tail
        | h :: t ->
          let nextX = (x + 1) mod maxX in
          let nextY = y mod maxY in
            if h = List.hd word then helper (acc + (search x y)) nextX nextY (t :: tail) else helper acc nextX nextY (t :: tail)
  in helper 0 0 0 lines;;

countXMAS listLines;;


let phraze = ['M'; 'A'; 'S'];;

let ends = ['M'; 'S'];;
let dirs = [((-1, -1), (1, 1)); ((1, -1), (-1, 1))];;

let countX_MAS lines =
  let maxY = List.length lines in
  let maxX = List.length (List.hd lines) in
  let getChar x y = 
    if x < 0 || x >= maxX || y < 0 || y >= maxY then None else Some(List.nth (List.nth lines y) x) in
  let search x y =
    let res = List.map (fun ((dx1, dy1), (dx2, dy2)) ->
      let first = Some(List.hd ends) in
      let second = Some(List.nth ends 1) in
      if getChar (x + dx1) (y + dy1) = first then
        if getChar (x + dx2) (y + dy2) = second then true
        else false
      else 
        if getChar (x + dx1) (y + dy1) = second then
          if getChar (x + dx2) (y + dy2) = first then true
          else false
        else false
    ) dirs
    in if List.hd res && List.nth res 1 then 1 else 0

  in
  let rec helper acc x y = function
    | [] -> acc
    | head :: tail -> match head with
        | [] -> helper acc 0 (y + 1) tail
        | h :: t ->
          let nextX = (x + 1) mod maxX in
          let nextY = y mod maxY in
            if h = 'A' then helper (acc + (search x y)) nextX nextY (t :: tail) else helper acc nextX nextY (t :: tail)
  in helper 0 0 0 lines;;

countX_MAS listLines;;

