open Base

let rec pr_str = function
  | Types.MalNum n -> Int.to_string n
  | Types.MalSymbol s -> s
  | Types.MalList lst ->
    List.map ~f:pr_str lst
  |> String.concat ~sep:" "
  |> fun x -> "(" ^ x ^ ")"
