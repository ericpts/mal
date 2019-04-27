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
    List.fold_left ~init: ([], env) ~f: (fun (acc, env) cur ->
        let el, nenv = (eval cur env) in
        el::acc, nenv
      ) lst
  in

  let map_eval_assoc_list (lst: (Types.mal_type * Types.mal_type) list) : ((Types.mal_type * Types.mal_type) list) * Env.env =
    List.fold_left ~init: ([], env) ~f: (fun (acc, env) (k, v) ->
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
  | Types.MalList lst -> begin
      match lst with
      | [] -> Types.MalList [], env
      | (Types.MalSymbol "def!")::rs -> begin
            match rs with
            | [Types.MalSymbol k; v] -> Types.MalNil, Env.set env (k, v)
            | _ -> raise (Failure ("invalid def! form: " ^ (Printer.pr_str ast)))
          end
      | _ -> begin
          let eval_lst, env = eval_ast ast env in
          match eval_lst with
          | Types.MalList (fn::rst) -> begin
              match fn with
              | Types.MalFn fn -> (fn rst), env
              | _ -> raise (Failure ("cannot call " ^ (Printer.pr_str fn) ^ " as a function"))
            end
          | _ -> raise (Failure ("failed to parse list" ^ (Printer.pr_str ast)))
        end
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
