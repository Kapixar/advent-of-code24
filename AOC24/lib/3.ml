let read_lines name =
	let ic = open_in name in
	let try_read () =
	  try Some (input_line ic) with End_of_file -> None in
	let rec loop acc = match try_read () with
	  | Some s -> loop (s :: acc)
	  | None -> close_in ic; List.rev acc in
	loop [];;

open Re;;

let line = List.hd (read_lines "3.txt");;
let regex = Re.compile Re.(seq [str "mul("; repn digit 1 (Some(3)); str ","; repn digit 1 (Some(3)); str ")"]);;

let matches = Re.matches regex line;;

let mul strr =
  let reg1 = Re.compile Re.(seq [str "("; repn digit 1 None]) in
  let reg2 = Re.compile Re.(seq [str ","; repn digit 1 None]) in
  let d1 = List.hd (Re.matches reg1 strr) in
  let d2 = List.hd (Re.matches reg2 strr) in
  (int_of_string (String.sub d1 1 (String.length d1 - 1)), int_of_string (String.sub d2 1 (String.length d2 - 1)));;

List.length matches;;

let rec solve acc = function
  | [] -> acc
  | h :: t -> let (d1, d2) = mul h in solve (acc + d1 * d2) t;;


solve 0 matches;;

(* 30916125 too low *)

let is_digit = function '0' .. '9' -> true | _ -> false

(* let parse strin =
  let rec helper acc cs d1 d2 = 
    match cs with
    | [] -> acc
    | h :: t -> match h with
            | '(' -> helper acc t None None
            | ',' -> helper acc t d1 None
            | ')' -> match d1 with
                    | None -> helper acc t None None
                    | Some d1v -> match d2 with
                            | None -> helper acc t None None
                            | Some d2v -> helper (acc + d1v * d2v) t None None
            | _ -> if is_digit h then match d2 with
                    | None -> helper acc t (Some (int_of_string (String.make 1 h))) None
                    | Some d1v -> match d2 with
                            | None -> helper acc t d1 (Some (int_of_string (String.make 1 h)))
                            | Some d2v -> helper acc t d1 d2
                    else helper acc t None None
  in helper 0 (String.split_on_char "" strin 0 0) None None;; *)