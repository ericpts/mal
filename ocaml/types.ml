
type mal_type =
  | MalList of mal_type list
  | MalSymbol of string
  | MalNum of int
