let remove_index_from_seq index seq =
    Seq.mapi (fun i x -> if i <> index then Some x else None) seq
    |> Seq.filter_map (fun x -> x)
;;

let max_value_seq seq =
    match seq () with
    | Seq.Nil -> None
    | Seq.Cons (first, rest) -> Some (Seq.fold_left max first rest)
;;

let split_on_char_seq ch str =
    let len = String.length str in
    let rec aux start () =
        if start >= len
        then Seq.Nil
        else (
          let idx =
              try String.index_from str start ch with
              | Not_found -> len
          in
          Seq.Cons (String.sub str start (idx - start), aux (idx + 1)))
    in
    aux 0
;;

let pattern_match_seq num seq =
    let rec aux acc i seq =
        if i = 0
        then (
          match Seq.uncons seq with
          | None -> Some (List.rev acc)
          | Some _ -> None)
        else (
          match Seq.uncons seq with
          | None -> None
          | Some (x, xs) -> aux (x :: acc) (i - 1) xs)
    in
    aux [] num seq
;;
