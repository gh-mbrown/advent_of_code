open Printf

let print_day_part day part answer = printf "Day %d Part %d: %d\n" day part answer

let run_day day =
    match day with
    | 1 ->
        Days.Day01.part_one () |> print_day_part 1 1;
        Days.Day01.part_two () |> print_day_part 1 2
    | 2 ->
        Days.Day02.part_one () |> print_day_part 2 1;
        Days.Day02.part_two () |> print_day_part 2 2
    | 3 ->
        Days.Day03.part_one () |> print_day_part 3 1;
        Days.Day03.part_two () |> print_day_part 3 2
    | 4 ->
        Days.Day04.part_one () |> print_day_part 4 1;
        Days.Day04.part_two () |> print_day_part 4 2
    | 5 ->
        Days.Day05.part_one () |> print_day_part 5 1;
        Days.Day05.part_two () |> print_day_part 5 2
    | 6 ->
        Days.Day06.part_one () |> print_day_part 6 1;
        Days.Day06.part_two () |> print_day_part 6 2
    | _ -> printf "%d not mapped\n" day
;;

let day = ref 1
let unbound = ref []
let anon_arg arg = unbound := arg :: !unbound
let speclist = [ "-day", Arg.Set_int day, "Day to run" ]

let () =
    Arg.parse speclist anon_arg "Used to select the day";
    run_day !day
;;
