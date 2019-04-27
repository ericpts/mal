open Base

let rec pr_str = function
  | Types.MalKeyword kw -> kw
  | Types.MalSymbol s -> s
  | Types.MalNum n -> Int.to_string n
  | Types.MalString s -> "\"" ^ s ^ "\""
  | Types.MalNil -> "<nil>"
  | Types.MalList lst ->
    begin
      List.map ~f:pr_str lst
      |> String.concat ~sep:" "
      |> fun x -> "(" ^ x ^ ")"
    end
  | Types.MalFn _ -> "<fun>"
  | Types.MalVec vec ->
    begin
      List.map ~f:pr_str vec
      |> String.concat ~sep:" "
      |> fun x -> "[" ^ x ^ "]"
    end
  | Types.MalHashMap hm ->
    begin
      List.map hm ~f:(fun (x, y) ->
          (pr_str x) ^ " " ^ (pr_str y)
        )
      |> String.concat ~sep:" "
      |> fun x -> "{" ^ x ^ "}"
    end
