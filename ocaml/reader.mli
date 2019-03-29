
type token = string

type reader = token list

(* Reader methods*)
val next : reader -> (token * reader)
val peek : reader -> token

val tokenize : string -> token list

val read_str : string -> Types.mal_type

val read_form : reader -> (Types.mal_type * reader)

val read_list : reader -> (Types.mal_type * reader)

val read_atom : reader -> (Types.mal_type * reader)
