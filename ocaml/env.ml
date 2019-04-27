open Base

type env = {
  outer: env option;
  data: (string * Types.mal_type) list;
}


let create ~outer = {
  outer = outer;
  data = [];
}

let set env (k, v) = {
  outer = env.outer;
  data = List.Assoc.add env.data ~equal:String.equal k v;
}

let rec find env k =
  match List.Assoc.find env.data ~equal:String.equal k with
  | Some v -> Some v
  | None -> Option.bind env.outer ~f: (fun e -> find e k)

let find_exn env k =
  match find env k with
  | Some v -> v
  | None -> raise (Failure ("failed to locate key " ^ k ^ " in env"))
