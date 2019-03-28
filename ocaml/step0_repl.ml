#use "topfind";;
#require "lwt";;
#require "base";;
#require "stdio";;

open Base

let read str = str

let eval ast env = ast

let print exp = exp

let rep str =
  let result = eval (read str) "" in
  print result


let rec main () =
  Stdio.printf "user> ";
  Stdio.Out_channel.flush Stdio.stdout;
  let line = Stdio.In_channel.input_line Stdio.stdin in
  match line with
  | Some line -> Stdio.printf "%s\n" (rep line); main ()
  | None -> ()

let () =
  main ()
