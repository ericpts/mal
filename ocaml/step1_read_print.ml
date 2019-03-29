#use "topfind";;

#load "readline.cma";;

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
