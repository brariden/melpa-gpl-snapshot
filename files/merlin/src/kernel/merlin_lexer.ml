(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std

type keywords = Raw_lexer.keywords

(* Lexing step *)
type item =
  | Valid of Lexing.position * Raw_parser.token * Lexing.position
  | Error of Raw_lexer.error * Location.t

let item_start (Valid (p,_,_) | Error (_,{Location. loc_start = p})) =
  p

let item_end (Valid (_,_,p) | Error (_,{Location. loc_end = p})) =
  p

(** Create an empty list new lexer *)
let empty ~filename =
  let pos =
    { Lexing.
      pos_fname = filename;
      pos_lnum  = 1;
      pos_bol   = 0;
      pos_cnum  = 0;
    }
  in
  History.initial ([], Valid (pos, Raw_parser.ENTRYPOINT, pos))

type t = {
  (* Result *)
  mutable history: (exn list * item) History.t;
  (* Input buffer *)
  refill: string option ref; (* Input not yet sent to lexer *)
  refill_empty: bool ref;    (* Lexer internal buffer status *)
  (* Lexer data *)
  state: Raw_lexer.state;
  lexbuf: Lexing.lexbuf;
  mutable resume: (unit -> Raw_parser.token Raw_lexer.result) option;
  mutable marker: Merlin_parser.frame option;
}

let history t = t.history

(** Prepare for lexing.
    Returns the start position (end position of last valid token), and a
    lexing function that will append at most one token to the history at each
    call. *)
let make_lexbuf empty refill position =
  Lexing.from_strings ~position ~empty ""
    (fun () ->
       match !refill with
       | Some s -> refill := None; s
       | None -> "")

let start keywords history =
  let position = match History.focused history with
    | _, Valid (_,_,p) -> p
    | _, Error (_,l) -> l.Location.loc_end
  in
  let refill = ref None in
  let refill_empty = ref true in
  let lexbuf = make_lexbuf refill_empty refill position in
  {
    history;
    state = Raw_lexer.make keywords;
    resume = None; refill; refill_empty; lexbuf;
    marker = None;
  }

let position t = Lexing.immediate_pos t.lexbuf

let feed t str =
  let warnings = ref (fst (History.focused t.history)) in
  Parsing_aux.catch_warnings warnings (fun () ->
    if not t.lexbuf.Lexing.lex_eof_reached then begin
      t.refill := Some str;
      let append item =
        begin match item with
          | Error (e,l) -> warnings := Raw_lexer.Error (e,l) :: !warnings
          | _ -> ()
        end;
        t.history <- History.insert (!warnings, item) t.history
      in
      let rec aux = function
        (* Lexer interrupted, there is data to refill or eof reached: continue. *)
        | Raw_lexer.Refill f
          when !(t.refill) <> None || not !(t.refill_empty) || str = "" ->
          aux (f ())
        (* Lexer interrupted, nothing to refill, return to caller. *)
        | Raw_lexer.Refill r ->
          t.resume <- Some r
        (* EOF Reached: notify EOF to parser, stop now *)
        | Raw_lexer.Return Raw_parser.EOF ->
          begin match History.focused t.history with
            | _, Valid (_,Raw_parser.EOF,_) -> ()
            | _ ->
              append (Valid (t.lexbuf.Lexing.lex_start_p,
                            Raw_parser.EOF,
                            t.lexbuf.Lexing.lex_curr_p));
          end
        | Raw_lexer.Return token ->
          append (Valid (t.lexbuf.Lexing.lex_start_p,
                        token,
                        t.lexbuf.Lexing.lex_curr_p));
          continue ()
        | Raw_lexer.Fail (e,l) ->
          append (Error (e,l));
          continue ()
      and continue () =
        aux (Raw_lexer.token t.state t.lexbuf)
      in
      begin match t.resume with
        (* At the beginning *)
        | None when History.position t.history = 0 ->
          aux (Raw_lexer.skip_sharp_bang t.state t.lexbuf)
        (* Next token *)
        | None -> continue ()
        (* Resume *)
        | Some f ->
          t.resume <- None;
          aux (f ())
      end;
      true
    end
    else
      false
  )


let eof t = t.lexbuf.Lexing.lex_eof_reached

let equal it1 it2 =
  match it1, it2 with
  | Valid (s1,t1,e1), Valid (s2,t2,e2) ->
    Lexing.compare_pos s1 s2 = 0 &&
    Lexing.compare_pos e1 e2 = 0 &&
    t1 = t2
  | Error (v1,l1), Error (v2,l2) ->
    Lexing.compare_pos l1.Location.loc_start l2.Location.loc_start = 0 &&
    Lexing.compare_pos l1.Location.loc_end l2.Location.loc_end = 0 &&
    v1 = v2
  | _ -> false

let put_mark t mark =
  t.marker <- mark

let get_mark t = t.marker

let item_location = function
  | Valid (loc_start,_,loc_end) ->
    {Location. loc_start; loc_end; loc_ghost = false}
  | Error (_,loc) ->
    loc

let token is = function
  | Valid (_,op,_) -> (is op <> None)
  | _ -> false

let extract_op for_locate = function
  | Error _ -> assert false
  | Valid (s,t,e) ->
    let t = Option.get (Raw_parser_values.is_operator t) in
    let t = if for_locate then t else "(" ^ t ^ ")" in
    Location.mkloc t {Location. loc_start = s; loc_end = e; loc_ghost = false}

let extract_ident = function
  | Error _ -> assert false
  | Valid (s,t,e) ->
    let t =
      match Raw_parser_values.is_ident t with
      | Some t -> t
      | None ->
        match Raw_parser_values.is_operator t with
        | Some t -> "( " ^ t ^ " )"
        | None -> assert false
    in
    Location.mkloc t {Location. loc_start = s; loc_end = e; loc_ghost = false}

(* [reconstruct_identifier] is impossible to read at the moment, here is a
   pseudo code version of the function:
   (many thanks to Gabriel for this contribution)

        00| let h = parse (focus h) with
        01|   | . { h+1 }
        02|   | _ { h }
        03| in
        04| parse h with
        05| | BOF x=operator       { [x] }
        06| | ¬( x=operator        { [x] }
        07| | ' x=ident            { [] }
        08| | _ {
        09|   let acc, h = parse (h ! tail h) with
        10|     | x=ident !          { [x], h }
        11|     | ( ! x=operator )   { [x], h }
        12|     | ( x=operator ! )   { [x], h - 1 }
        13|     | ( x=operator ) !   { [x], h - 2 }
        14|     | _ { [], h }
        15|   in
        16|   let h = h - 1 in
        17|   let rec head acc = parse (h !) with
        18|     | tl x=ident . ! { head (x :: acc) tl }
        19|     | x=ident . !    { ident :: acc }
        20|     | _              { acc }
        21|   in head acc
        22| }

   Now for the explainations:
     line 0-3:  if we're on a dot, skip it and move to the right

     line 5,6:  if we're on an operator not preceeded by an opening parenthesis,
                just return that.

     line 7:    if we're on a type variable, don't return anything.
                reconstruct_identifier is called when locating and getting the
                type of an expression, in both cases there's nothing we can do
                with a type variable.
                See #317

     line 8-22: two step approach:
       - line 9-15:  retrieve the identifier
                     OR retrieve the parenthesed operator and move before the
                        opening parenthesis

       - line 16-21: retrieve the "path" prefix of the identifier/operator we
                     got in the previous step.


   Additionnaly, the message of commit fc0b152 explains what we consider is an
   identifier:

     «
        Interpreting an OCaml identifier out of context is a bit ambiguous.

        A prefix of the form (UIDENT DOT)* is the module path,
        A UIDENT suffix is either a module name, a module type name (in case the
        whole path is a module path), or a value constructor.
        A LIDENT suffix is either a value name, a type constructor or a module
        type name.
        A LPAREN OPERATOR RPAREN suffix is a value name (and soon, maybe a
        value constructor if beginning by ':' ?!) .

        In the middle, LIDENT DOT (UIDENT DOT)* is projection of the field of a
        record.  In this case, merlin will drop everything up to the first
        UIDENT and complete in the scope of the (UIDENT DOT)* interpreted as a
        module path.
        Soon, the last UIDENT might also be the type of an inline record.
        (Module2.f.Module1.A <- type of the record of the value constructor named A of
        type f, defined in Module1 and aliased in Module2, pfffff).
     »
*)


let reconstruct_identifier ?(for_locate=false) h =
  (*List.iter (fun (_,item) ->
      match item with
      | Valid (_,tok,_) ->
        let sym = Raw_parser_values.symbol_of_token tok in
        let cls = Raw_parser_values.class_of_symbol sym in
        prerr_endline (Raw_parser_values.string_of_class cls)
      | _ -> () ) (History.tail h);*)
  let h = match History.focused h with
    | _, Valid (_,Raw_parser.DOT,_) -> History.move 1 h
    | _ -> h
  in
  match History.head h with
  | List.One (_, op) when token Raw_parser_values.is_operator op ->
    [ extract_op for_locate op ]
  | List.More ((_, op), (List.More ((_, rest), _) | List.One (_, rest)))
    when token Raw_parser_values.is_operator op
      && not (token Raw_parser_values.is_lparen rest) ->
    [ extract_op for_locate op ]
  | List.More ((_, id), ( List.More ((_, quote), _)
                        | List.One (_, quote)))
    when token Raw_parser_values.is_ident id
      && token Raw_parser_values.is_quote quote ->
    []
  | _ ->
    let acc, h = match History.head h, History.tail h with
      | (List.More((_, ident), _) | List.One (_, ident)), _
        when token Raw_parser_values.is_ident ident -> [ident], h
      | ( List.More ((_, Valid (_,Raw_parser.LPAREN,_)), _)
        | List.One (_, Valid (_,Raw_parser.LPAREN,_))),
        (_, op) :: (_, Valid (_,Raw_parser.RPAREN,_)) :: _
        when token Raw_parser_values.is_operator op -> [op], h
      | List.More ((_, op),
                  ( List.More ((_, Valid (_,Raw_parser.LPAREN,_)), _)
                  | List.One (_, Valid (_,Raw_parser.LPAREN,_)))),
        (_, Valid (_,Raw_parser.RPAREN,_)) :: _
        when token Raw_parser_values.is_operator op -> [op], History.move (-1) h
      | List.More ((_, Valid (_,Raw_parser.RPAREN,_)),
                  List.More ((_, op),
                              ( List.More ((_, Valid (_,Raw_parser.LPAREN,_)), _)
                              | List.One (_, Valid (_,Raw_parser.LPAREN,_))))),
        _
        when token Raw_parser_values.is_operator op -> [op], History.move (-2) h
      | _ -> [], h
    in
    let h = History.move (-1) h in
    let rec head acc = function
      | List.More ((_, Valid (_,Raw_parser.DOT,_)),
                  List.More ((_, ident), tl))
        when token Raw_parser_values.is_ident ident -> head (ident :: acc) tl
      | List.More ((_, Valid (_,Raw_parser.DOT,_)),
                  List.One (_, ident))
        when token Raw_parser_values.is_ident ident -> (ident :: acc)
      | _ -> acc
    in
    List.map ~f:extract_ident (head acc (History.head h))

let is_uppercase {Location. txt = x} =
  x <> "" && Char.is_uppercase x.[0]

let rec drop_lowercase acc = function
  | [x] -> List.rev (x :: acc)
  | x :: xs when not (is_uppercase x) -> drop_lowercase [] xs
  | x :: xs -> drop_lowercase (x :: acc) xs
  | [] -> List.rev acc

let identifier_suffix ident =
  match List.last ident with
  | Some x when is_uppercase x -> drop_lowercase [] ident
  | _ -> ident
