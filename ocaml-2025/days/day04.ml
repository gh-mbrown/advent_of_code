let file_name = "input/day04.txt"

let parse_data =
    lazy
      (Common.File.read_file_list file_name
       |> List.map (fun x -> List.init (String.length x) (String.get x))
       |> List.mapi (fun row x ->
         List.mapi (fun col y -> if y = '@' then Some (row, col) else None) x)
       |> List.map (List.filter_map (fun x -> x))
       |> List.concat)
;;

let make_ad (row, col) =
    [ row - 1, col - 1
    ; row - 1, col
    ; row - 1, col + 1
    ; row, col - 1
    ; row, col + 1
    ; row + 1, col - 1
    ; row + 1, col
    ; row + 1, col + 1
    ]
;;

let filt lst =
    let tbl = Hashtbl.create (List.length lst) in
    List.iter (fun pos -> Hashtbl.add tbl pos ()) lst;
    List.filter
      (fun pos -> make_ad pos |> List.filter (Hashtbl.mem tbl) |> List.length < 4)
      lst
;;

let rec filt_all removed remaining =
    let filtered = filt remaining in
    match filtered with
    | [] -> removed
    | _ ->
        let len = List.length filtered in
        let tbl = Hashtbl.create len in
        List.iter (fun pos -> Hashtbl.add tbl pos ()) filtered;
        List.filter (fun x -> not (Hashtbl.mem tbl x)) remaining
        |> filt_all (len :: removed)
;;

let part_one () = Lazy.force parse_data |> filt |> List.length
let part_two () = Lazy.force parse_data |> filt_all [] |> List.fold_left ( + ) 0
