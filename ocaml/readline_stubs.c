/* File generated from readline.idl */

#include <stddef.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#ifdef Custom_tag
#include <caml/custom.h>
#include <caml/bigarray.h>
#endif
#include <caml/camlidlruntime.h>

#include <readline/readline.h>
#include <readline/history.h>
value camlidl_readline_readline(
	value _v_prompt)
{
  char *prompt; /*in*/
  char *_res;
  value _v1;
  value _vres;

  prompt = String_val(_v_prompt);
  _res = readline(prompt);
  if (_res == NULL) {
    _vres = Val_int(0);
  } else {
    _v1 = copy_string(_res);
    Begin_root(_v1)
      _vres = camlidl_alloc_small(1, 0);
      Field(_vres, 0) = _v1;
    End_roots();
  }
  return _vres;
}

value camlidl_readline_using_history(value _unit)
{
  using_history();
  return Val_unit;
}

value camlidl_readline_add_history(
	value _v_line)
{
  char const *line; /*in*/
  line = String_val(_v_line);
  add_history(line);
  return Val_unit;
}

value camlidl_readline_clear_history(value _unit)
{
  clear_history();
  return Val_unit;
}

value camlidl_readline_read_history(
	value _v_file)
{
  char const *file; /*in*/
  int _res;
  value _vres;

  file = String_val(_v_file);
  _res = read_history(file);
  _vres = Val_int(_res);
  return _vres;
}

value camlidl_readline_write_history(
	value _v_file)
{
  char const *file; /*in*/
  int _res;
  value _vres;

  file = String_val(_v_file);
  _res = write_history(file);
  _vres = Val_int(_res);
  return _vres;
}

value camlidl_readline_rl_variable_bind(
	value _v_var,
	value _v_val)
{
  char const *var; /*in*/
  char const *val; /*in*/
  int _res;
  value _vres;

  var = String_val(_v_var);
  val = String_val(_v_val);
  _res = rl_variable_bind(var, val);
  _vres = Val_int(_res);
  return _vres;
}

