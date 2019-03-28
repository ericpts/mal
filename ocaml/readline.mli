(* File generated from readline.idl *)


external readline : string -> string option
	= "camlidl_readline_readline"

external using_history : unit -> unit
	= "camlidl_readline_using_history"

external add_history : string -> unit
	= "camlidl_readline_add_history"

external clear_history : unit -> unit
	= "camlidl_readline_clear_history"

external read_history : string -> int
	= "camlidl_readline_read_history"

external write_history : string -> int
	= "camlidl_readline_write_history"

external rl_variable_bind : string -> string -> int
	= "camlidl_readline_rl_variable_bind"

