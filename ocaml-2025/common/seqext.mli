val remove_index_from_seq : int -> 'a Seq.t -> 'a Seq.t
val max_value_seq : (unit -> 'a Seq.node) -> 'a option
val split_on_char_seq : char -> string -> string Seq.t
val pattern_match_seq : int -> 'a Seq.t -> 'a list option
