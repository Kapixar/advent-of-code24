module MapType = struct
  type t = int * int
  let compare (x1, y1) (x2, y2) = compare (x1, y1) (x2, y2)
end

module Maps = Set.Make(MapType);;

let read_lines name =
	let ic = open_in name in
	let try_read () =
	  try Some (input_line ic) with End_of_file -> None in
	let rec loop acc = match try_read () with
	  | Some s -> 
        let seq = String.to_seq s in
        let arr = Array.of_seq seq in
        loop (Array.append acc [|arr|])
	  | None -> close_in ic; acc in
	loop [||];;


let directions = [(0,-1); (1,0); (0,1); (-1,0)];;


let maze = read_lines "6.txt";;

(* let start = (60,60);; (4, 6);; *)
let startY = 70;;
let Some(startX) = Array.find_index (fun c -> c = '^') maze.(startY);;
let start = (startX, startY);;

let goThrough start = 
  let rec inner current direction path =
    let (x, y) = current in
    let (dx, dy) = List.nth directions direction in
    let (nx, ny) = (x + dx, y + dy) in
    if nx < 0 || ny < 0 || nx >= Array.length maze || ny >= Array.length maze.(0) then path
    else
      if (maze.(ny)).(nx) = '#' then inner current ((direction + 1) mod 4) path
      else inner (nx, ny) direction (Maps.add (nx, ny) path)

  in inner start 0 (Maps.of_list [start]);;

let res = goThrough start;;
let visited = Maps.to_list res;;
let visitedCount = List.length visited;;

let isLoop start nMaze =
    let rec inner current direction path bounces prev =
        let (x, y) = current in
        let (dx, dy) = List.nth directions direction in
        let (nx, ny) = (x + dx, y + dy) in
        if nx < 0 || ny < 0 || nx >= Array.length nMaze || ny >= Array.length nMaze.(0) then false
        else
            if (nMaze.(ny)).(nx) = '#' then 
            if Maps.mem current bounces then true
            else
                inner current ((direction + 1) mod 4) path bounces current
        else inner (nx, ny) direction (Maps.add (nx, ny) path) (Maps.add prev bounces) (0,0)
    
    in inner start 0 (Maps.of_list [start]) Maps.empty (0,0);;

isLoop start maze;;

let deepCopy maze =
    let nMaze = Array.copy maze in
    Array.map (fun x -> Array.copy x) nMaze;;


let putObstacle x y = 
    let nnMaze = deepCopy maze in
    nnMaze.(y).(x) <- '#';
    nnMaze;;

(* deepCopy maze;; *)
(* putObstacle 3 6;; *)
isLoop start (putObstacle 1 7);;



List.fold_left (fun acc (x, y) -> 
    let candidate = putObstacle x y in
    if isLoop start candidate then acc + 1 else acc) 0 visited;;