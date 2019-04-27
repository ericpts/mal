
let list_to_pairs (lst: 'a list): (('a * 'a) list) =
  let rec impl (acc: ('a * 'a) list) (cur: 'a list) =
    match cur with
    | [] -> List.rev acc
    | hd::sd::rs -> impl ((hd, sd)::acc) rs
    | _::[] -> Failure (
        "list has odd number of elements ") |> raise
  in
  impl [] lst
