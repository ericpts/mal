open Core

type token = string

type reader = token list

let tokenize s =
  let re = Pcre.regexp {foo|[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)|foo} in
  let tokens =
    Pcre.extract_all ~full_match:false ~rex: re s
    (* This generates an array of arrays, and each sub-array has only one element, the actual match.*)
    |> Array.map ~f: (fun a -> Array.get a 0)
  in
  Array.to_list tokens
  |> List.filter ~f:(fun t -> not (String.equal t ""))


let peek r =
  match r with
  | hd :: _ -> hd
  | [] -> raise (Failure "unexpected EOF")


let next = function
  | [] -> raise (Failure "unexpected EOF")
  | hd :: tl -> (hd, tl)


let is_empty = function
  | [] -> true
  | _ -> false

let matching_paren = function
  | "(" -> ")"
  | "[" -> "]"
  | "{" -> "}"
  | ")" -> "("
  | "]" -> "["
  | "}" -> "{"
  | _ -> raise (Failure "incorrect paren")


let rec read_atom r =
  let cur, r = next r in
  let mal_type = match cur.[0] with
  | '0' .. '9' -> Types.MalNum (int_of_string cur)
  | '-' -> begin
      match String.length cur with
        | 1 -> Types.MalSymbol "-"
        | _ -> match cur.[1] with
          | '0' .. '9' -> Types.MalNum (int_of_string cur)
          | _ -> Types.MalSymbol cur
    end
  | _ -> Types.MalSymbol cur
  in
  mal_type, r

and read_list r paren =
  let tok, r = next r in
  assert (String.equal tok (matching_paren paren));

  (* We'll build the list in reverse order, since it is more time-efficient
  to append at the beginning than at the end.
  *)
  let rec impl acc r =
    match is_empty r with
    | true -> raise (Failure "unbalanced paranthesis")
    | false ->
      let cur = peek r in
      match cur with
      | ")" | "]" | "}" -> begin
          if cur = paren then
            let _, r = next r in
            Types.MalList (List.rev acc), r
          else
            raise (Failure "unbalanced paranthesis")
        end
      | _ -> begin
          let new_el, r = read_form r in
          impl (new_el :: acc) r
        end
  in
  impl [] r

and read_form r =
  match peek r with
  | "(" -> read_list r ")"
  | "[" -> read_list r "]"
  | "{" -> read_list r "}"
  | _ -> read_atom r

and read_str s =
  let reader = tokenize s in
  (* List.iter ~f: (Stdio.printf "!%s!\n") reader; *)
  let form, reader = read_form reader in
  match is_empty reader with
  | true -> form
  | false -> raise (Failure "improper expression: did not reach end of input")
