open Base

let repl_env symbol =
  let make_numeric_fn (fn: int list -> int):
    (Types.mal_type) =
    let f_mal (mal_lst: Types.mal_type list) =
      List.map ~f:(fun el -> match el with
          | Types.MalNum x -> x
          | _ -> raise (Failure ("cannot apply numerical function to non-numerical value " ^ (Printer.pr_str el)))
        ) mal_lst
      |> fn
      |> Types.MalNum
    in
    Types.MalFn f_mal
  in

  match symbol with
  | "+" -> List.reduce_exn ~f:( + ) |> make_numeric_fn
  | "-" -> List.reduce_exn ~f:( - ) |> make_numeric_fn
  | "*" -> List.reduce_exn ~f:( * ) |> make_numeric_fn
  | "/" -> List.reduce_exn ~f:( / ) |> make_numeric_fn
  | "%" -> List.reduce_exn ~f:( % ) |> make_numeric_fn
  | _ -> raise (Failure ("unknown symbol" ^ symbol))

let read str =
  Reader.read_str str

let rec eval_ast ast env =
  match ast with
  | Types.MalList lst -> List.map ~f:(fun ast -> eval ast env) lst |> Types.MalList
  | Types.MalVec vec -> List.map ~f:(fun ast -> eval ast env) vec |> Types.MalVec
  | Types.MalString s -> Types.MalString s
  | Types.MalSymbol sym -> env sym
  | Types.MalKeyword kw -> Types.MalKeyword kw
  | Types.MalNum num -> Types.MalNum num
  | Types.MalFn fn -> Types.MalFn fn
  | Types.MalHashMap hm -> List.Assoc.map ~f:(fun ast -> eval ast env) hm |> Types.MalHashMap
and
  eval ast env =
  match ast with
  | Types.MalList lst -> begin
      match lst with
      | [] -> Types.MalList []
      | _::_ ->
        let eval_lst = eval_ast ast env in
        match eval_lst with
        | Types.MalList (fn::rst) -> begin
            match fn with
            | Types.MalFn fn -> (fn rst)
            | _ -> raise (Failure ("cannot call " ^ (Printer.pr_str fn) ^ " as function"))
          end
        | _ -> raise (Failure ("failed to parse list" ^ (Printer.pr_str ast)))
    end
  | _ -> eval_ast ast env


let print exp = exp

let rep str =
  try
    let result = eval (read str) repl_env in
    Printer.pr_str result
  with
  Failure f -> Printf.sprintf "error: %s\n" f


let rec main () =
  let line = Readline.readline "user> " in
  match line with
  | Some line -> begin
      Readline.add_history line;
      Stdio.printf "%s\n" (rep line);
      Stdio.Out_channel.flush Stdio.stdout;
      main ()
    end
  | None -> ()

let () =
  main ()
