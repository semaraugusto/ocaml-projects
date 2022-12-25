open Core
open String


let rec _print_list x = 
    match x with
        | [] -> ()
        | hd :: tail -> print_endline hd; _print_list tail;;
    
let rec _print_int_list x = 
    match x with
        | [] -> ()
        | hd :: tail -> print_endline (string_of_int hd); _print_int_list tail;;

let rec _print_list_list x = 
    match x with
        | [] -> ()
        | hd :: tail -> _print_list hd; _print_list_list tail;;

module Action = struct 
    type t = Win | Draw | Lose

    let of_string str = match str with 
        | "X" -> Lose
        | "Y" -> Draw
        | "Z" -> Win
        | _ -> failwith "invalid instruction"
    ;;

    let points act = match act with 
        | Win -> 6
        | Draw -> 3
        | Lose -> 0
end

module Part1 = struct
    open Action
    type moves = Rock | Paper | Scissors
    type _t = moves * moves

    let moves_of_string = function 
        | "A" | "X" -> Rock
        | "B" | "Y" -> Paper
        | "C" | "Z" -> Scissors
        | _ -> failwith "invalid instruction"
    ;;

    let of_string str = 
        let split = String.split str ~on: ' ' in
        match split with 
            | fst :: scnd :: [] -> (
                (moves_of_string fst), (moves_of_string scnd))
            | _ -> failwith "invalid instruction"
    ;;
    let equals a b = match (a, b) with 
        | (Rock, Rock) | (Paper, Paper) | (Scissors, Scissors) -> true
        | _ -> false
    ;;

    let move_points mv = match mv with 
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3
    ;;

    let points t = 
        let (mv1, mv2) = t in
        let p2 = move_points mv2 in
        match (mv1, mv2) with 
            | (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) -> (points Action.Win) + p2
            | ours, theirs when equals ours theirs -> (points Action.Draw) + p2 
            | _ -> (points Action.Lose) + p2
    ;;
end

module Part2 = struct
    open Action
    type moves = Rock | Paper | Scissors
    type _t = moves * Action.t

    let moves_of_string = function 
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissors
        | _ -> failwith "invalid instruction"
    ;;

    let of_string str = 
        let split = String.split str ~on: ' ' in
        match split with 
            | fst :: scnd :: [] -> ((moves_of_string fst), (Action.of_string scnd))
            | _ -> failwith "invalid instruction"
    ;;

    let move_points mv = match mv with 
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3
    ;;

    let get_win mv = match mv with 
        | Rock -> Paper
        | Paper -> Scissors 
        | Scissors -> Rock
    ;;

    let get_lose mv = match mv with 
        | Rock -> Scissors 
        | Paper -> Rock 
        | Scissors -> Paper
    ;;

    let get_draw mv = match mv with 
        | Rock -> Rock 
        | Paper -> Paper 
        | Scissors -> Scissors
    ;;

    let points t = 
        let (mv, action) = t in
        match action with
            | Action.Win -> (points Action.Win) + (move_points (get_win mv))
            | Action.Draw -> (points Action.Draw) + (move_points (get_draw mv))
            | Action.Lose -> (points Action.Lose) + (move_points (get_lose mv))
    ;;
end

let part1 filename =
    let (lines : string list) = In_channel.read_lines filename in 
    let instr = List.map ~f:(fun x -> Part1.of_string x) lines in 
    let points = List.map ~f:(fun x -> Part1.points x) instr in 
    (* let _ = _print_int_list points in *)
    let result = List.fold_left ~f:(+) ~init:0 points in
    let _ = print_endline ("part1 result: " ^ string_of_int result) in
    result
;;

let part2 filename =
    let (lines : string list) = In_channel.read_lines filename in 
    let instr = List.map ~f:(fun x -> Part2.of_string x) lines in 
    let points = List.map ~f:(fun x -> Part2.points x) instr in 
    (* let _ = _print_int_list points in *)
    let result = List.fold_left ~f:(+) ~init:0 points in
    let _ = print_endline ("part1 result: " ^ string_of_int result) in
    result
;;

let _ = print_endline ""
let (result_toy_part1 : int) = part1 "toy.txt"
let _ = assert (phys_equal 15 result_toy_part1)
let result_part1 = part1 "input.txt"
let _ = assert (phys_equal 13675 result_part1)


let _ = print_endline ""
let (result_toy_part2 : int) = part2 "toy.txt"
let _ = assert (phys_equal 12 result_toy_part2)
let result_part2 = part2 "input.txt"
let _ = assert (phys_equal 14184 result_part2)
