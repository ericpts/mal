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
  (* Drop the last element. *)
  |> List.rev
  |> List.tl_exn
  |> List.rev


let peek r =
  match r with
  | hd :: _ -> hd
  | [] -> raise (Failure "Incorrect peek()")


let next = function
  | [] -> raise (Failure "Incorrect next()")
  | hd :: tl -> (hd, tl)


let is_empty = function
  | [] -> true
  | _ -> false

let rec read_atom r =
  let cur, r = next r in
  let mal_type = match cur.[0] with
  | '0' .. '9' | '-' -> Types.MalNum (int_of_string cur)
  | _ -> Types.MalSymbol cur
  in
  mal_type, r


and read_list r =
  let tok, r = next r in
  assert (String.equal tok "(");
  (* We'll build the list in reverse order, since it is more time-efficient
  to append at the beginning than at the end.
  *)
  let rec impl acc r =
    match peek r with
    | ")" -> begin
        let _, r = next r in
        Types.MalList (List.rev acc), r
      end
    | _ -> begin
        let new_el, r = read_form r in
        impl (new_el :: acc) r
      end
  in
  impl [] r

and read_form r =
  match peek r with
  | "(" -> read_list r
  | _ -> read_atom r

and read_str s =
  let reader = tokenize s in
  let form, reader = read_form reader in
  assert (is_empty reader);
  form
