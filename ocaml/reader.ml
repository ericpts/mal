#use "topfind";;

#require "re2";;

let tokenize s =
  let re = Re2.create_exn {foo|[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)|foo} in
  Re2.find_submatches_exn re s
