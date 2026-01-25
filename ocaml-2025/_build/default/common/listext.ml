let remove_at_i idx lst =
    List.mapi (fun i x -> if i <> idx then Some x else None) lst
    |> List.filter_map (fun x -> x)
;;

let print_list to_string lst =
    let rec aux rem acc =
        match rem with
        | [] -> acc
        | [ s ] -> acc ^ to_string s
        | s :: ss -> aux ss (acc ^ to_string s ^ "; ")
    in
    print_string "[";
    print_string (aux lst "");
    print_endline "]"
;;

let print_char_list = print_list (fun x -> "'" ^ String.make 1 x ^ "'")
let print_string_list = print_list (fun x -> "\"" ^ x ^ "\"")
