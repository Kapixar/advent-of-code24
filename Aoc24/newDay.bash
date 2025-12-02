#!/bin/bash
cd txt
START=$(ls | wc -l)
END=$((START+1))
touch ./$END.txt
touch ../lib/D$END.ml
echo "let solve1 (lines : string list) =
  let part1 = List.length lines in
  part1;;



let solve (lines : string list) =
  let part1 = solve1 lines in
  let part2 = 2 in
  part1, part2;;" >> ../lib/D$END.ml
