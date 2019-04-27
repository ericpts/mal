open Base

let repl_env =
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

  let add_numeric_fn name fn env =
    Env.set env (name, (List.reduce_exn ~f:fn |> make_numeric_fn))
  in

  Env.create ~outer:None
  |> add_numeric_fn "+" ( + )
  |> add_numeric_fn "-" ( - )
  |> add_numeric_fn "*" ( * )
  |> add_numeric_fn "/" ( / )
  |> add_numeric_fn "%" ( % )


let read str =
  Reader.read_str str

let rec eval_ast (ast: Types.mal_type) (env: Env.env) : (Types.mal_type * Env.env) =
  let map_eval_list (lst: Types.mal_type list) : (Types.mal_type list * Env.env) =
    List.fold_right ~init: ([], env) ~f: (fun cur (acc, env) ->
        let el, nenv = (eval cur env) in
        el::acc, nenv
      ) lst
  in

  let map_eval_assoc_list (lst: (Types.mal_type * Types.mal_type) list) : ((Types.mal_type * Types.mal_type) list) * Env.env =
    List.fold_right ~init: ([], env) ~f: (fun (k, v) (acc, env) ->
        let nv, nenv = (eval v env) in
        (k, nv)::acc, nenv
      ) lst
  in

  match ast with
  | Types.MalList lst -> let (nlst, nenv) = map_eval_list lst in Types.MalList nlst, nenv
  | Types.MalVec vec -> let (nvec, nenv) = map_eval_list vec in Types.MalVec nvec, nenv
  | Types.MalHashMap hm -> let (nhm, nenv) = map_eval_assoc_list hm in Types.MalHashMap nhm, nenv
  | Types.MalString s -> Types.MalString s, env
  | Types.MalSymbol sym -> Env.find_exn env sym, env
  | Types.MalKeyword kw -> Types.MalKeyword kw, env
  | Types.MalNum num -> Types.MalNum num, env
  | Types.MalFn fn -> Types.MalFn fn, env
  | Types.MalNil -> Types.MalNil, env


and


eval (ast: Types.mal_type) (env: Env.env) : Types.mal_type * Env.env =
  match ast with
  | Types.MalList [] -> Types.MalList [], env
  | Types.MalList ((Types.MalSymbol "def!")::rs) -> begin
      match rs with
      | [Types.MalSymbol k; v] ->
        let v, env = eval v env in v, Env.set env (k, v)
      | _ -> raise (Failure ("invalid def! form: " ^ (Printer.pr_str ast)))
    end
  | Types.MalList ((Types.MalSymbol "let*")::rs) -> begin
      match rs with
      | [Types.MalVec binds; body] | [Types.MalList binds; body] -> begin
        let binds = Util.list_to_pairs binds in
        let binds = List.map binds ~f: (fun (k, v) -> match k with
            | Types.MalSymbol k -> (k, v)
            | _ -> raise (Failure
                            ("wrong binding in let* form: " ^ (Printer.pr_str k)))
          ) in
        let sub_env =
          List.fold_left binds ~init: (Env.create ~outer:(Some env)) ~f: (
            fun env (k, v) -> Env.set env (k, eval v env |> fst)
          ) in
        eval body sub_env |> fst, env
      end
      | _ -> raise (Failure ("could not parse let* form: " ^ (Printer.pr_str ast)))

    end
  | Types.MalList _ -> begin
      let eval_lst, env = eval_ast ast env in
      match eval_lst with
      | Types.MalList (fn::rst) -> begin
          match fn with
          | Types.MalFn fn -> (fn rst), env
          | _ -> raise (Failure ("cannot call " ^ (Printer.pr_str fn) ^ " as a function"))
        end
      | _ -> raise (Failure ("failed to parse list" ^ (Printer.pr_str ast)))
    end
  | _ -> eval_ast ast env


let print exp = exp

let rep str env =
  try
    let result, env = eval (read str) env in
    Printer.pr_str result, env
  with
  Failure f -> Printf.sprintf "error: %s\n" f, env


let rec main env =
  let line = Readline.readline "user> " in
  match line with
  | Some line -> begin
      Readline.add_history line;
      let output, nenv = rep line env in
      Stdio.printf "%s\n" output;
      Stdio.Out_channel.flush Stdio.stdout;
      main nenv
    end
  | None -> ()

let () =
  main repl_env
