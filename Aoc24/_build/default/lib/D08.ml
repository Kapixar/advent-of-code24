module MapType = struct
  type t = int * int
  let compare (x1, y1) (x2, y2) = compare (x1, y1) (x2, y2)
end

module Maps = Set.Make(MapType);;

let print_list_list list = 
  List.iter (fun l -> 
    List.iter (fun (x, y) -> print_string (string_of_int x ^ "," ^ string_of_int y ^ "\t")) l;
    print_newline ()
  ) list;;

let print_map map = 
  Maps.iter (fun (x, y) -> print_string (string_of_int x ^ "," ^ string_of_int y ^ "\t")) map;
  print_newline ();;

let addArr arr id value =
    let rec helper count = function
      | [] -> []
      | head :: tail -> if count = id then (value :: head) :: tail else head :: helper (count + 1) tail
  in helper 0 arr;;

let parse lines = 
  List.map (fun s -> 
    let seq = String.to_seq s in
    List.of_seq seq
  ) lines;;

let rec getAntennas chars mapIter x y = 
  match mapIter with
  | [] -> chars
  | head :: tail -> 
    match head with
      | [] -> getAntennas chars tail 0 (y + 1)
      | h :: t -> 
        if h = '.' then getAntennas chars (t :: tail) (x + 1) y
        else
        let newChars = addArr chars (Char.code h) (x, y) in
        getAntennas newChars (t :: tail) (x + 1) y;;

let getNodePos (x1, y1) (x2, y2) =
    let dX = abs (x2 - x1) in
    let dY = abs (y2 - y1) in
    let nX1 = if x1 < x2 then x1 - dX else x1 + dX in
    let nY1 = if y1 < y2 then y1 - dY else y1 + dY in
    let nX2 = if x1 < x2 then x2 + dX else x2 - dX in
    let nY2 = if y1 < y2 then y2 + dY else y2 - dY in
    
    [(nX1, nY1); (nX2, nY2)];;

let getCombinations list = 
  let rec inner acc = function
    | [] -> acc
    | head :: tail -> 
      let newAcc = List.fold_left (fun acc x -> (head, x) :: acc) acc tail in
      inner newAcc tail
  in inner [] list;;

getNodePos (1, 1) (2, 2);;
getNodePos (0, 6) (1, 3);;
getNodePos (2, 2) (0, 7);;

getNodePos (9, 9) (8, 8);;
getNodePos (9, 9) (6, 5);;
getNodePos (6, 5) (8, 8);;


let solve map getNodeFun = 
  let mapHeight = List.length map in
  let mapWidth = List.length (List.hd map) in
  let antennas = getAntennas (List.init 123 (fun _ -> [])) map 0 0 in
    (* print_list_list antennas; *)
  let rec getNodes nodeSet antens = 
    match antens with
    | [] -> nodeSet
    | head :: tail -> 
      match head with
      | [] -> getNodes nodeSet tail
      | _ -> 
        let newSet = List.fold_left (fun acc (a1, a2) -> 
            let points = getNodeFun a1 a2 in
            List.fold_left (fun acc2 h ->
                let x1, y1 = h in
                if x1 < 0 || x1 >= mapWidth || y1 < 0 || y1 >= mapHeight then acc2
                else Maps.add h acc2
            ) acc points 
        ) nodeSet (getCombinations head) in
        getNodes newSet tail
    in 
    let nodes = getNodes Maps.empty antennas in
    print_map nodes;
    Maps.cardinal nodes;;

let getNodePosLong (x1, y1) (x2, y2) =
    let dX = (x2 - x1) in
    let dY = (y2 - y1) in
    let rec inner acc x y dir = 
        if x < 0 || x >= 50 || y < 0 || y >= 50 then acc
        else inner ((x, y) :: acc) (x + dX * dir) (y + dY * dir) dir
    in
    (inner [] x1 y1 1) @ (inner [] x2 y2 (-1));;

getNodePosLong (0, 0) (3, 1) ;;
getNodePosLong (0, 0) (1, 2) ;;
getNodePosLong (1, 2) (3, 1) ;;

let solve (lines:string list) =
  let map = parse lines in
  let part1 = solve map getNodePos in
  let part2 = solve map getNodePosLong in
  part1, part2;;

