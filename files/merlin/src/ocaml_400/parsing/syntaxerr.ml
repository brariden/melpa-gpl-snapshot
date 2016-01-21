(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Auxiliary type for reporting syntax errors *)

type error =
    Unclosed of Location.t * string * Location.t * string
  | Expecting of Location.t * string
  | Applicative_path of Location.t
  | Variable_in_scope of Location.t * string
  | Other of Location.t

exception Error of error
exception Escape_error of Lexing.position

let prepare_error = function
  | Unclosed(opening_loc, opening, closing_loc, closing) ->
      Location.errorf ~loc:closing_loc
        ~sub:[
          Location.error ~loc:opening_loc
            (Printf.sprintf "Error: This '%s' might be unmatched" opening)
        ]
        ~if_highlight:
          (Printf.sprintf "Syntax error: '%s' expected, \
                           the highlighted '%s' might be unmatched"
             closing opening)
        "Error: Syntax error: '%s' expected" closing

  | Expecting (loc, nonterm) ->
      Location.errorf ~loc "Error: Syntax error: %s expected." nonterm
  | Applicative_path loc ->
      Location.errorf ~loc
        "Error: Syntax error: applicative paths of the form F(X).t \
         are not supported when the option -no-app-func is set."
  | Variable_in_scope (loc, var) ->
      Location.errorf ~loc
        "Error: In this scoped type, variable '%s@ \
         is reserved for the local type %s."
        var var
  | Other loc ->
      Location.error ~loc "Error: Syntax error"

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (prepare_error err)
      | Escape_error pos ->
        let loc = {Location. loc_start = pos; loc_end = pos;
                   loc_ghost = false} in
        Some (prepare_error (Other loc))
      | _ -> None
    )


let report_error ppf err =
  Location.report_error ppf (prepare_error err)

let location_of_error = function
  | Unclosed(l,_,_,_)
  | Applicative_path l
  | Variable_in_scope(l,_)
  | Other l
  | Expecting (l, _) -> l
