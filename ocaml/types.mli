
type mal_type =
  | MalKeyword of string
  | MalSymbol of string
  | MalString of string
  | MalNum of int
  | MalFn of (mal_type list -> mal_type)
  | MalList of mal_type list
  | MalVec of mal_type list
  | MalHashMap of (mal_type * mal_type) list
