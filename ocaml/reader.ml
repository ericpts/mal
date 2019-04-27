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
  | ':' -> Types.MalKeyword cur
  | '"' -> Types.MalString (read_string cur)
  | _ -> Types.MalSymbol cur
  in
  mal_type, r


and read_string t_init =
  assert (t_init.[0] = '"');
  let t = String.drop_prefix t_init 1 in
  let last = (String.suffix t 1) in
  if not (String.equal last "\"") then
    raise (Failure ("unbalanced string quotes in token " ^ t_init))
  else
    String.drop_suffix t 1

and read_list_like r open_token close_token =
  let tok, r = next r in
  assert (String.equal tok open_token);

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
          if cur = close_token then
            let _, r = next r in
            List.rev acc, r
          else
            raise (Failure "unbalanced paranthesis")
        end
      | _ -> begin
          let new_el, r = read_form r in
          impl (new_el :: acc) r
        end
  in
  impl [] r


and read_list r =
  let lst, r = read_list_like r "(" ")" in
  Types.MalList lst, r

and read_vec r =
  let vec, r = read_list_like r "[" "]" in
  Types.MalVec vec, r

and read_hash_map r =
  let lst, r = read_list_like r "{" "}" in
  let keys_are_equal k1 k2 = match k1 with
    | Types.MalString str1 -> begin
        match k2 with
        | Types.MalString str2 -> str1 = str2
        | Types.MalKeyword _ -> false
        | _ -> raise (Failure ("cannot use " ^ (Printer.pr_str k2) ^ " as hash map key"))
      end
    | Types.MalKeyword str1 -> begin
        match k2 with
        | Types.MalKeyword str2 -> str1 = str2
        | Types.MalString _ -> false
        | _ -> raise (Failure ("cannot use " ^ (Printer.pr_str k2) ^ " as hash map key"))
      end
    | _ -> raise (Failure ("cannot use " ^ (Printer.pr_str k1) ^ " as hash map key"))
  in
  let rec impl acc rem = match rem with
    | [] -> acc
    | _::[] -> raise (Failure "hash map contains odd number of elements")
    | hd::tl::rst ->
      let new_acc = List.Assoc.add acc hd tl ~equal:keys_are_equal in
      impl new_acc rst
  in
  Types.MalHashMap (impl [] lst), r


and read_form r =
  match peek r with
  | "(" -> read_list r
  | "[" -> read_vec r
  | "{" -> read_hash_map r
  | _ -> read_atom r


and read_str s =
  let reader = tokenize s in
  (* List.iter ~f: (Stdio.printf "!%s!\n") reader; *)
  let form, reader = read_form reader in
  match is_empty reader with
  | true -> form
  | false -> raise (Failure "improper expression: did not reach end of input")
