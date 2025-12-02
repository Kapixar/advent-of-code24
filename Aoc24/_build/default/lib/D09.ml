

let removeOddIDReverse list =
    let rec inner acc list =
        match list with
        | [last] -> last :: acc
        | [] -> acc
        | h1 :: _ :: tail -> inner (h1 :: acc) tail
        
    in inner [] list;;

removeOddIDReverse [1;2;3;4;5;6;7];;


let solve1 discString =
    let maxId = ((List.length discString) + 1) / 2 - 1 in
    let rec inner acc state position list cID cIDMax listRev cRevID =
        print_int cID;
        print_string " ";
        print_int cRevID;
        print_string " ";

        print_int cIDMax;
        print_string " ";
        print_int (List.hd list);
        print_string " ";
        print_int (List.hd listRev);
        print_string "\n";
        if cID = cRevID && cIDMax - (List.hd list) = (List.hd listRev) then acc
        else
        if state then
            match list with
            | [] -> acc     (* warning *)
            | head :: tail ->
                print_int cID;
                print_string "acc: ";
                print_int (cID * position);
                print_string "\n";
                let newAcc = acc + cID * position in
                if head > 1 then
                    inner newAcc true (position + 1) ((head - 1) :: tail) cID cIDMax listRev cRevID
                else
                    inner newAcc false (position + 1) tail (cID + 1) (List.hd tail) listRev cRevID
        else 
            match list with
            | [] -> acc     (* warning *)
            | head :: tail ->
                match listRev with
                | [] -> acc (* warning *)
                | headRev :: tailRev ->
                    print_string "acc rev: ";
                    print_int (cRevID * position);
                    print_string "\n";
                    let newAcc = acc + cRevID * position in
                    let isRevEnd = headRev = 1 in
                    let newRevID = if isRevEnd then cRevID - 1 else cRevID in
                    let newRevList = if isRevEnd then tailRev else (headRev - 1) :: tailRev in
                    if head > 1 then
                        inner newAcc false (position + 1) ((head - 1) :: tail) cID cIDMax newRevList newRevID
                    else
                        inner newAcc true (position + 1) tail cID (List.hd tail) newRevList newRevID
                     
                    
    in inner 0 true 0 discString 0 (List.hd discString) (removeOddIDReverse discString) maxId;;


(* 6220182342255 too low *)

let solve (lines : string list) =
    let discList = String.to_seq (List.hd lines) |> List.of_seq |> List.map(fun c -> int_of_string (Char.escaped c)) in
    let part1 = solve1 discList in
    let part2 = 2 in
    part1, part2;;