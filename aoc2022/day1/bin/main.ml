open Core;;

let rec split_elves lines result = match lines with
    | [] -> result
    | "" :: rest -> split_elves rest (0 :: result)
    | cals :: rest -> split_elves rest (match result with 
        | [] -> [int_of_string cals]
        | hd :: tail -> (hd + int_of_string cals) :: tail)
;;

let read_lines file =
    let lines = In_channel.read_lines file in 
    let elves = split_elves lines [] in
    elves
;;

let max lst = List.fold_left ~f:max ~init:0 lst

let rec _print_list x = 
    match x with
        | [] -> ()
        | hd :: tail -> print_endline (string_of_int hd); _print_list tail;;

let part1 filename = 
    let x = read_lines filename in
    let result = max x in
    (* let _ = print_list x in  *)
    print_endline ("part1 " ^ (string_of_int result))
;;


let rec top_3 (lst: int list) ((m1 : int), (m2 : int), (m3 : int)) = match lst with 
    | [] -> (m1, m2, m3)
    | hd :: tl -> top_3 tl (
        match (m1, m2, m3) with 
        | (m1, m2, _) when hd > m1 -> (hd, m1, m2)
        | (m1, m2, _) when hd > m2 -> (m1, hd, m2)
        | (m1, m2, m3) when hd > m3 -> (m1, m2, hd)
        | _ -> (m1, m2, m3)
    )

let sum_tuple ((m1 : int), (m2 : int), (m3 : int)) = m1 + m2 + m3

let part2 filename = 
    let x = read_lines filename in
    let top_3_result = top_3 x (0, 0, 0) in
    let result = sum_tuple top_3_result in
    print_endline ("part2 " ^ (string_of_int result))

;;

let _ = print_endline ""
let _ = part1 "toy.txt"
let _ = part1 "input.txt"
let _ = part2 "toy.txt"
let _ = part2 "input.txt"
