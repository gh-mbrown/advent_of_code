open In_channel

let read_file_list file_name =
    with_open_text file_name input_lines |> List.filter (fun x -> x <> "")
;;

let read_file_seq file_name =
    with_open_text file_name input_lines |> List.to_seq |> Seq.filter (fun x -> x <> "")
;;
