open Base

let read str =
  Reader.read_str str

let eval ast env = ast

let print exp = exp

let rep str =
  try
    let result = eval (read str) "" in
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
