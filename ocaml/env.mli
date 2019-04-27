
type env

val create : outer:(env option) -> env

val set : env -> (string * Types.mal_type) -> env

val find : env -> string -> Types.mal_type option

val find_exn : env -> string -> Types.mal_type
