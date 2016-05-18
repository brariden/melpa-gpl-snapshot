%{
open Asttypes
open Longident
open Parsetree
open Ast_helper


let rloc loc_start loc_end =
  { Location. loc_start; loc_end; loc_ghost = false; }
let gloc loc_start loc_end =
  { Location. loc_start; loc_end; loc_ghost = true; }
let mkloc =
  Location.mkloc

let mktyp startpos endpos d   = Typ.mk ~loc:(rloc startpos endpos) d
let mkpat startpos endpos d   = Pat.mk ~loc:(rloc startpos endpos) d
let mkexp startpos endpos d   = Exp.mk ~loc:(rloc startpos endpos) d
let mkmty startpos endpos d   = Mty.mk ~loc:(rloc startpos endpos) d
let mksig startpos endpos d   = [Sig.mk ~loc:(rloc startpos endpos) d]
let mkmod startpos endpos d   = Mod.mk ~loc:(rloc startpos endpos) d
let mkstr startpos endpos d   = [Str.mk ~loc:(rloc startpos endpos) d]
let ghstr startpos endpos d   = [Str.mk ~loc:(gloc startpos endpos) d]
let mkclass startpos endpos d = Cl.mk  ~loc:(rloc startpos endpos) d
let mkcty startpos endpos d   = Cty.mk ~loc:(rloc startpos endpos) d
let mkctf startpos endpos ?attrs d = Ctf.mk ~loc:(rloc startpos endpos) ?attrs d
let mkcf  startpos endpos ?attrs d = [Cf.mk  ~loc:(rloc startpos endpos) ?attrs d]

let mkrhs startpos endpos rhs = mkloc rhs (rloc startpos endpos)
let mkoption d =
  let loc = {d.ptyp_loc with Location. loc_ghost = true} in
  Typ.mk ~loc (Ptyp_constr(mkloc (Ldot (Lident "*predef*", "option")) loc,[d]))

let reloc_pat startpos endpos x= { x with ppat_loc = rloc startpos endpos };;
let reloc_exp startpos endpos x= { x with pexp_loc = rloc startpos endpos };;
let reloc_exp_fake startpos endpos x =
  let str = mkloc "merlin.loc" (rloc startpos endpos) in
  { x with pexp_attributes = (str , PStr []) :: x.pexp_attributes }

let mkoperator startpos endpos name =
  let loc = rloc startpos endpos in
  Exp.mk ~loc (Pexp_ident(mkloc (Lident name) loc))

let mkpatvar startpos endpos name =
  Pat.mk ~loc:(rloc startpos endpos) (Ppat_var (mkrhs startpos endpos name))

(*
  Ghost expressions and patterns:
  expressions and patterns that do not appear explicitly in the
  source file they have the loc_ghost flag set to true.
  Then the profiler will not try to instrument them and the
  -annot option will not try to display their type.

  Every grammar rule that generates an element with a location must
  make at most one non-ghost element, the topmost one.

  How to tell whether your location must be ghost:
  A location corresponds to a range of characters in the source file.
  If the location contains a piece of code that is syntactically
  valid (according to the documentation), and corresponds to the
  AST node, then the location must be real; in all other cases,
  it must be ghost.
*)
let ghexp startpos endpos d = Exp.mk ~loc:(gloc startpos endpos) d
let ghpat startpos endpos d = Pat.mk ~loc:(gloc startpos endpos) d
let ghtyp startpos endpos d = Typ.mk ~loc:(gloc startpos endpos) d
let ghloc startpos endpos d = { txt = d; loc = gloc startpos endpos }

let mkinfix startpos endpos arg1 startpos2 endpos2 name arg2 =
  mkexp startpos endpos
    (Pexp_apply(mkoperator startpos2 endpos2 name, [Nolabel, arg1; Nolabel, arg2]))

let neg_float_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus startpos endpos name arg =
  match name, arg.pexp_desc with
  | "-", Pexp_constant(Const_int n) ->
      mkexp startpos endpos (Pexp_constant(Const_int(-n)))
  | "-", Pexp_constant(Const_int32 n) ->
      mkexp startpos endpos (Pexp_constant(Const_int32(Int32.neg n)))
  | "-", Pexp_constant(Const_int64 n) ->
      mkexp startpos endpos (Pexp_constant(Const_int64(Int64.neg n)))
  | "-", Pexp_constant(Const_nativeint n) ->
      mkexp startpos endpos (Pexp_constant(Const_nativeint(Nativeint.neg n)))
  | ("-" | "-."), Pexp_constant(Const_float f) ->
      mkexp startpos endpos (Pexp_constant(Const_float(neg_float_string f)))
  | _ ->
      mkexp startpos endpos (Pexp_apply(mkoperator startpos endpos ("~" ^ name), [Nolabel, arg]))

let mkuplus startpos endpos name arg =
  let desc = arg.pexp_desc in
  match name, desc with
  | "+", Pexp_constant(Const_int _)
  | "+", Pexp_constant(Const_int32 _)
  | "+", Pexp_constant(Const_int64 _)
  | "+", Pexp_constant(Const_nativeint _)
  | ("+" | "+."), Pexp_constant(Const_float _) -> mkexp startpos endpos desc
  | _ ->
      mkexp startpos endpos (Pexp_apply(mkoperator startpos endpos ("~" ^ name), [Nolabel, arg]))

let mkexp_cons consloc args loc =
  Exp.mk ~loc (Pexp_construct(mkloc (Lident "::") consloc, Some args))

let mkpat_cons consloc args loc =
  Pat.mk ~loc (Ppat_construct(mkloc (Lident "::") consloc, Some args))

let rec mktailexp startpos endpos = function
    [] ->
      let loc = gloc startpos endpos in
      let nil = { txt = Lident "[]"; loc = loc } in
      Exp.mk ~loc (Pexp_construct (nil, None))
  | e1 :: el ->
      let open Location in
      let exp_el = mktailexp e1.pexp_loc.loc_end endpos el in
      let loc = gloc e1.pexp_loc.loc_start exp_el.pexp_loc.loc_end in
      let arg = Exp.mk ~loc (Pexp_tuple [e1; exp_el]) in
      mkexp_cons loc arg loc

let rec mktailpat startpos endpos = function
    [] ->
      let loc = gloc startpos endpos in
      let nil = { txt = Lident "[]"; loc = loc } in
      Pat.mk ~loc (Ppat_construct (nil, None))
  | p1 :: pl ->
      let open Location in
      let pat_pl = mktailpat p1.ppat_loc.loc_end endpos pl in
      let loc = gloc p1.ppat_loc.loc_start pat_pl.ppat_loc.loc_end in
      let arg = Pat.mk ~loc (Ppat_tuple [p1; pat_pl]) in
      mkpat_cons loc arg loc

let mkstrexp e attrs =
  { pstr_desc = Pstr_eval (e, attrs); pstr_loc = e.pexp_loc }

let mkexp_constraint startpos endpos e (t1, t2) =
  match t1, t2 with
  | Some t, None -> mkexp startpos endpos (Pexp_constraint(e, t))
  | _, Some t -> mkexp startpos endpos (Pexp_coerce(e, t1, t))
  | None, None -> e

let array_function startpos endpos par assign =
  let op = if assign then par^"<-" else par in
  ghloc startpos endpos (Lident op)

let syntax_error startpos endpos =
  Parsing_aux.raise_warning (Syntaxerr.Escape_error (rloc startpos endpos))

let unclosed opening_name opstart opend closing_name clstart clend =
  raise
    Syntaxerr.(Error (Unclosed (rloc opstart opend, opening_name,
                                rloc clstart clend, closing_name)))

let expecting startpos endpos nonterm =
  raise
    Syntaxerr.(Error (Expecting (rloc startpos endpos, nonterm)))

let not_expecting startpos endpos nonterm =
  Parsing_aux.raise_warning
    Syntaxerr.(Error (Not_expecting (rloc startpos endpos, nonterm)))

let bigarray_function startpos endpos order assign =
  let op =
    match order with
    | 1 -> ".{}"
    | 2 -> ".{,}"
    | 3 -> ".{,,}"
    | _ -> ".{,..,}"
  in
  let op = if assign then op^"<-" else op in
  ghloc startpos endpos (Lident op)

let bigarray_untuplify = function
    { pexp_desc = Pexp_tuple explist; pexp_loc = _ } -> explist
  | exp -> [exp]

let bigarray_get (startpos,endpos) (startop,endop) arr arg =
  let ghexp = ghexp startop endop in
  let mkexp = mkexp startpos endpos in
  let bigarray_function = bigarray_function startop endop in
  let get order = bigarray_function order false in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(get 1)),
                       [Nolabel, arr; Nolabel, c1]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(get 2)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(get 3)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2; Nolabel, c3]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(get 0)),
                       [Nolabel, arr; Nolabel, ghexp(Pexp_array coords)]))

let bigarray_set (startpos,endpos) (startop,endop) arr arg newval =
  let ghexp = ghexp startop endop in
  let mkexp = mkexp startpos endpos in
  let bigarray_function = bigarray_function startop endop in
  let set order = bigarray_function order true in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(set 1)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, newval]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(set 2)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2; Nolabel, newval]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(set 3)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2; Nolabel, c3; Nolabel, newval]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(set 0)),
                       [Nolabel, arr;
                        Nolabel, ghexp(Pexp_array coords);
                        Nolabel, newval]))

let lapply startpos endpos p1 p2 =
  if Clflags.applicative_functors ()
  then Lapply(p1, p2)
  else raise (Syntaxerr.Error(Syntaxerr.Applicative_path (rloc startpos endpos)))

let exp_of_label startpos endpos lbl =
  mkexp startpos endpos (Pexp_ident(mkrhs startpos endpos (Lident(Longident.last lbl))))

let pat_of_label startpos endpos lbl =
  mkpat startpos endpos (Ppat_var (mkrhs startpos endpos (Longident.last lbl)))

let check_variable vl loc v =
  if List.mem v vl then
    Parsing_aux.raise_warning Syntaxerr.(Error(Variable_in_scope(loc,v)))

let varify_constructors var_names t =
  let rec loop t =
    let desc =
      match t.ptyp_desc with
      | Ptyp_any -> Ptyp_any
      | Ptyp_var x ->
          check_variable var_names t.ptyp_loc x;
          Ptyp_var x
      | Ptyp_arrow (label,core_type,core_type') ->
          Ptyp_arrow(label, loop core_type, loop core_type')
      | Ptyp_tuple lst -> Ptyp_tuple (List.map loop lst)
      | Ptyp_constr( { txt = Lident s }, []) when List.mem s var_names ->
          Ptyp_var s
      | Ptyp_constr(longident, lst) ->
          Ptyp_constr(longident, List.map loop lst)
      | Ptyp_object (lst, o) ->
          Ptyp_object (List.map (fun (s, attrs, t) -> (s, attrs, loop t)) lst, o)
      | Ptyp_class (longident, lst) ->
          Ptyp_class (longident, List.map loop lst)
      | Ptyp_alias(core_type, string) ->
          check_variable var_names t.ptyp_loc string;
          Ptyp_alias(loop core_type, string)
      | Ptyp_variant(row_field_list, flag, lbl_lst_option) ->
          Ptyp_variant(List.map loop_row_field row_field_list,
                       flag, lbl_lst_option)
      | Ptyp_poly(string_lst, core_type) ->
          List.iter (check_variable var_names t.ptyp_loc) string_lst;
          Ptyp_poly(string_lst, loop core_type)
      | Ptyp_package(longident,lst) ->
          Ptyp_package(longident,List.map (fun (n,typ) -> (n,loop typ) ) lst)
      | Ptyp_extension (s, arg) ->
          Ptyp_extension (s, arg)
    in
    {t with ptyp_desc = desc}
  and loop_row_field  =
    function
      | Rtag(label,attrs,flag,lst) ->
          Rtag(label,attrs,flag,List.map loop lst)
      | Rinherit t ->
          Rinherit (loop t)
  in
  loop t

let wrap_type_annotation startpos endpos newtypes core_type body =
  let mkexp = mkexp startpos endpos in
  let ghtyp = ghtyp startpos endpos in
  let exp = mkexp(Pexp_constraint(body,core_type)) in
  let exp =
    List.fold_right (fun newtype exp -> mkexp (Pexp_newtype (newtype, exp)))
      newtypes exp
  in
  (exp, ghtyp (Ptyp_poly(newtypes,varify_constructors newtypes core_type)))

let wrap_exp_attrs startpos endpos body (ext, attrs) =
  (* todo: keep exact location for the entire attribute *)
  let body = {body with pexp_attributes = attrs @ body.pexp_attributes} in
  match ext with
  | None -> body
  | Some id -> ghexp startpos endpos (Pexp_extension (id, PStr [mkstrexp body []]))

let mkexp_attrs startpos endpos d attrs =
  wrap_exp_attrs startpos endpos (mkexp startpos endpos d) attrs

let fake_tydecl tydecl = tydecl.ptype_name, tydecl
let fake_untydecl (ptype_name,tydecl) = {tydecl with ptype_name}
let fake_vb_app f vb = {vb with pvb_expr = Fake.app f vb.pvb_expr}

%}

(**
 * `Shift n             : when at the left of the annotated symbol, it is a
 *                        valid recovery to shift it, and this strategy has
 *                        priority [n]
 * `Shift_token (n,tok) : when at the right of the annotated symbol shifting
 *                        [tok] is a strategy with priority [n]
 * `Cost n              : the annotated symbol cost is adjusted by [n]
 * `Indent n            : the annotated symbol column should be adjusted by
 *                        [n] before computing recovery guide
 * `Unclosed name       : the current symbol is the opening a pair that should
 *                        closed. [name] will be reported to the user in case
 *                        of syntax error.
 * `Close               : this symbol close the pair opened by an earlier
 *                        `Unclosed
 * `Item kind           : this symbol begins the definition of an [kind] item,
 *                        where kind is let, type, ...
 **)
%annot <[ `Shift of int | `Shift_token of int * token | `Cost of int
        | `Indent of int
        | `Unclosed of string | `Close
        | `Item of string ]>

(* Tokens *)

%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BACKQUOTE
%token BANG
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token CLASS
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COMMA
%token CONSTRAINT
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <string> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token INHERIT
%token INITIALIZER
%token <int> INT
%token <int32> INT32
%token <int64> INT64
%token <string> LABEL
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETAT
%token LBRACKETATAT
%token LBRACKETATATAT
%token LBRACKETBAR
%token LBRACKETGREATER
%token LBRACKETLESS
%token LBRACKETPERCENT
%token LBRACKETPERCENTPERCENT
%token LESS
%token LESSMINUS
%token LET
%token <string> LIDENT
%token LPAREN
%token MATCH
%token METHOD
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token <nativeint> NATIVEINT
%token NEW
%token OBJECT
%token OF
%token OPEN
%token <string> OPTLABEL
%token OR
%token PERCENT
%token PLUS
%token PLUSDOT
%token PLUSEQ
%token <string> PREFIXOP
%token PRIVATE
%token QUESTION
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token SHARP
%token <string> SHARPOP
%token SIG
%token STAR
%token <string * string option> STRING
%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH
%token <string * Location.t> COMMENT

%token EOL

%token LET_LWT
%token TRY_LWT
%token MATCH_LWT
%token FINALLY_LWT
%token FOR_LWT
%token WHILE_LWT
%token JSNEW
%token P4_QUOTATION
%token CUSTOM_BANG
%token OUNIT_TEST
%token OUNIT_TEST_UNIT
%token OUNIT_TEST_MODULE
%token OUNIT_BENCH
%token OUNIT_BENCH_FUN
%token OUNIT_BENCH_INDEXED
%token OUNIT_BENCH_MODULE
%token NONREC
%token SHARPSHARP

%token ENTRYPOINT EXITPOINT

(* Unused tokens, for compatibility with MetaOCaml builds *)

%token DOTLESS
%token DOTTILDE
%token GREATERDOT
%token <string> LETOP

(* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*)

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          (* below EQUAL ({lbl=...; lbl=...}) *)
%nonassoc LET LET_LWT                   (* above SEMI ( ...; let ... in ...) *)
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 (* below BAR  (match ... with ...) *)
%nonassoc FINALLY_LWT
%nonassoc AND             (* above WITH (module rec A: SIG with ... and ...) *)
%nonassoc THEN                          (* below ELSE (if ... then ...) *)
%nonassoc ELSE                          (* (if ... then ... else ...) *)
%nonassoc LESSMINUS                     (* below COLONEQUAL (lbl <- x := e) *)
%right    COLONEQUAL                    (* expr (e := e := e) *)
%nonassoc AS
%left     BAR                           (* pattern (p|p|p) *)
%nonassoc below_COMMA
%left     COMMA                         (* expr/expr_comma_list (e,e,e) *)
%right    MINUSGREATER                  (* core_type2 (t -> t -> t) *)
%right    OR BARBAR                     (* expr (e || e || e) *)
%right    AMPERSAND AMPERAMPER          (* expr (e && e && e) *)
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   (* expr (e OP e OP e) *)
%right    INFIXOP1                      (* expr (e OP e OP e) *)
%nonassoc below_LBRACKETAT
%nonassoc LBRACKETAT
%nonassoc LBRACKETATAT
%right    COLONCOLON                    (* expr (e :: e :: e) *)
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ (* expr (e OP e OP e) *)
%left     INFIXOP3 STAR PERCENT         (* expr (e OP e OP e) *)
%right    INFIXOP4                      (* expr (e OP e OP e) *)
%nonassoc prec_unary_minus prec_unary_plus (* unary - *)
%nonassoc prec_constant_constructor     (* cf. simple_expr (C versus C x) *)
%nonassoc prec_constr_appl              (* above AS BAR COLONCOLON COMMA *)
%nonassoc below_SHARP
%nonassoc SHARP SHARPSHARP              (* simple_expr/toplevel_directive *)
%left     SHARPOP
%nonassoc below_DOT
%nonassoc DOT

(* Finally, the first tokens of simple_expr are above everything else. *)
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT
          LBRACKETPERCENT LBRACKETPERCENTPERCENT
          P4_QUOTATION JSNEW CUSTOM_BANG

(* Entry points *)

(* for implementation files *)
%start implementation
%type <Parsetree.structure> implementation

(* for interface files *)
%start interface
%type <Parsetree.signature> interface

(* merlin: for inline expression *)
%start parse_expression
%type <Parsetree.expression> parse_expression

(* Prevent some warnings... *)
%start dummy
%type <unit> dummy

%%

(* Entry points *)

implementation:
| ENTRYPOINT structure EOF
    { $2 }

interface:
| ENTRYPOINT signature EOF
    { $2 }

parse_expression:
| ENTRYPOINT seq_expr EOF
    { $2 }

dummy:
| EOL
| COMMENT
| GREATERRBRACKET
| ENTRYPOINT
| LET_LWT
| TRY_LWT
| MATCH_LWT
| FINALLY_LWT
| FOR_LWT
| WHILE_LWT
| JSNEW
| P4_QUOTATION
| OUNIT_TEST
| OUNIT_TEST_UNIT
| OUNIT_TEST_MODULE
| OUNIT_BENCH
| OUNIT_BENCH_FUN
| OUNIT_BENCH_INDEXED
| OUNIT_BENCH_MODULE
    { () }

(* Module expressions *)

functor_arg:
| LPAREN RPAREN
    { mkrhs $startpos($2) $endpos($2) "*", None }
| LPAREN functor_arg_name COLON module_type RPAREN
    { mkrhs $startpos($2) $endpos($2) $2, Some $4 }

functor_arg_name:
| UIDENT
    { $1 }
| UNDERSCORE
    { "_" }

functor_args:
| functor_args functor_arg
    { $2 :: $1 }
| functor_arg
    { [ $1 ] }

module_expr:
| mod_longident
    { mkmod $startpos $endpos (Pmod_ident (mkrhs $startpos($1) $endpos($1) $1)) }
| STRUCT @{`Unclosed "struct"}
  structure END @{`Close}
    { mkmod $startpos $endpos (Pmod_structure($2)) }
| FUNCTOR functor_args MINUSGREATER module_expr
    { List.fold_left (fun acc (n, t) -> mkmod $startpos $endpos (Pmod_functor(n, t, acc))) $4 $2 }
| module_expr LPAREN module_expr RPAREN
    { mkmod $startpos $endpos (Pmod_apply($1, $3)) }
| module_expr LPAREN RPAREN
    { mkmod $startpos $endpos (Pmod_apply($1, mkmod $startpos $endpos (Pmod_structure []))) }
| LPAREN @{`Unclosed "("}
  module_expr COLON module_type RPAREN @{`Close}
    { mkmod $startpos $endpos (Pmod_constraint($2, $4)) }
| LPAREN @{`Unclosed "("} module_expr RPAREN @{`Close}
    { $2 }
| LPAREN @{`Unclosed "("}
  VAL expr RPAREN @{`Close}
    { mkmod $startpos $endpos (Pmod_unpack $3) }
| LPAREN @{`Unclosed "("}
  VAL expr COLON package_type RPAREN @{`Close}
    { mkmod $startpos $endpos (Pmod_unpack(
              ghexp $startpos $endpos (Pexp_constraint($3, ghtyp $startpos $endpos (Ptyp_package $5))))) }
| LPAREN @{`Unclosed "("}
  VAL expr COLON package_type COLONGREATER package_type RPAREN @{`Close}
    { mkmod $startpos $endpos (Pmod_unpack(
              ghexp $startpos $endpos (Pexp_coerce($3, Some(ghtyp $startpos $endpos (Ptyp_package $5)),
                                    ghtyp $startpos $endpos (Ptyp_package $7))))) }
| LPAREN @{`Unclosed "("}
  VAL expr COLONGREATER package_type RPAREN @{`Close}
    { mkmod $startpos $endpos (Pmod_unpack(
              ghexp $startpos $endpos (Pexp_coerce($3, None, ghtyp $startpos $endpos (Ptyp_package $5))))) }
| module_expr attribute
    { Mod.attr $1 $2 }
| extension
    { mkmod $startpos $endpos (Pmod_extension $1) }

structure:
| v = structure_head
| v = structure_head EXITPOINT
  { v }

structure_head:
| toplevel_directives seq_expr post_item_attributes structure_tail
  @{`Shift_token (1,EXITPOINT)}
    { mkstrexp $2 $3 :: $4 }
| toplevel_directives structure_tail
  @{`Shift_token (1,EXITPOINT)}
    { $2 }

structure_tail:
| (* empty *)
    { [] }
| SEMISEMI structure_head
  @{`Shift_token (1,EXITPOINT)}
    { $2 }
| structure_item structure_tail
  @{`Shift_token (1,EXITPOINT)}
    { $1 @ $2 }

structure_item:
| LET @{`Item "let"}
  ext_attributes rec_flag let_bindings
    {
      match $4 with
        [ {pvb_pat = { ppat_desc = Ppat_any; ppat_loc = _ };
           pvb_expr = exp; pvb_attributes = attrs}] ->
          let exp = wrap_exp_attrs $startpos $endpos exp $2 in
          mkstr $startpos $endpos (Pstr_eval (exp, attrs))
      | l ->
        let str = mkstr $startpos $endpos (Pstr_value($3, List.rev l)) in
        let (ext, attrs) = $2 in
        if attrs <> [] then not_expecting $startpos($2) $endpos($2) "attribute";
        match ext with
        | None -> str
        | Some id -> ghstr $startpos $endpos (Pstr_extension((id, PStr str), []))
    }
| EXTERNAL @{`Item "external"}
  val_ident COLON core_type EQUAL primitive_declaration
  post_item_attributes
    { mkstr $startpos $endpos
        (Pstr_primitive (Val.mk (mkrhs $startpos($2) $endpos($2) $2) $4
                           ~prim:$6 ~attrs:$7 ~loc:(rloc $startpos $endpos))) }
| VAL @{`Item "val"}
  val_ident COLON core_type post_item_attributes
    { mkstr $startpos $endpos
        (Pstr_primitive (Val.mk (mkrhs $startpos($2) $endpos($2) $2) $4
                           ~attrs:$5 ~loc:(rloc $startpos $endpos)))}
| TYPE @{`Item "type"}
  rf = nonrec_flag decls = type_declarations
    { mkstr $startpos $endpos (Pstr_type (rf, List.rev decls) ) }
| TYPE @{`Item "type"}
  rf = nonrec_flag exts = str_type_extension
     { if rf <> Recursive then not_expecting $startpos(rf) $endpos(rf) "nonrec flag";
       mkstr $startpos $endpos (Pstr_typext exts) }
| EXCEPTION @{`Item "exception"}
  str_exception_declaration
    { mkstr $startpos $endpos (Pstr_exception $2) }
| MODULE @{`Item "module"}
  module_binding
    { mkstr $startpos $endpos (Pstr_module $2) }
| MODULE REC @{`Item "recursive module"}
  module_bindings
    { mkstr $startpos $endpos (Pstr_recmodule(List.rev $3)) }
| MODULE TYPE @{`Item "module type"}
  ident post_item_attributes
    { mkstr $startpos $endpos (Pstr_modtype (Mtd.mk (mkrhs $startpos($3) $endpos($3) $3)
                              ~attrs:$4 ~loc:(rloc $startpos $endpos))) }
| MODULE TYPE @{`Item "module type"}
  ident EQUAL module_type post_item_attributes
    { mkstr $startpos $endpos (Pstr_modtype (Mtd.mk (mkrhs $startpos($3) $endpos($3) $3)
                              ~typ:$5 ~attrs:$6 ~loc:(rloc $startpos $endpos))) }
| open_statement
    { mkstr $startpos $endpos (Pstr_open $1) }
| CLASS @{`Item "class"}
  class_declarations
    { mkstr $startpos $endpos (Pstr_class (List.rev $2)) }
| CLASS TYPE @{`Item "class type"}
  class_type_declarations
    { mkstr $startpos $endpos (Pstr_class_type (List.rev $3)) }
| INCLUDE @{`Item "include"}
  module_expr post_item_attributes
    { mkstr $startpos $endpos (Pstr_include (Incl.mk $2 ~attrs:$3
                                             ~loc:(rloc $startpos $endpos))) }
| item_extension post_item_attributes
    { mkstr $startpos $endpos (Pstr_extension ($1, $2)) }
| floating_attribute
    { mkstr $startpos $endpos (Pstr_attribute $1) }

module_binding_body:
| EQUAL module_expr
    { $2 }
| COLON module_type EQUAL module_expr
    { mkmod $startpos $endpos (Pmod_constraint($4, $2)) }
| functor_arg module_binding_body
    { mkmod $startpos $endpos (Pmod_functor(fst $1, snd $1, $2)) }

module_bindings:
| module_binding
    { [$1] }
| module_bindings AND module_binding
    { $3 :: $1 }

module_binding:
| UIDENT module_binding_body post_item_attributes
    { Mb.mk (mkrhs $startpos($1) $endpos($1) $1) $2 ~attrs:$3 ~loc:(rloc $startpos $endpos) }

(* Module types *)

module_type:
| mty_longident
    { mkmty $startpos $endpos (Pmty_ident (mkrhs $startpos($1) $endpos($1) $1)) }
| SIG @{`Unclosed "sig"} signature END @{`Close}
    { mkmty $startpos $endpos (Pmty_signature $2) }
| FUNCTOR functor_args MINUSGREATER module_type %prec below_WITH
    { List.fold_left (fun acc (n, t) -> mkmty $startpos $endpos (Pmty_functor(n, t, acc))) $4 $2 }
| module_type WITH with_constraints
    { mkmty $startpos $endpos (Pmty_with($1, List.rev $3)) }
| MODULE TYPE OF module_expr %prec below_LBRACKETAT
    { mkmty $startpos $endpos (Pmty_typeof $4) }
(*| LPAREN @{`Unclosed "("} MODULE mod_longident RPAREN @{`Close}
    { mkmty $startpos $endpos  (Pmty_alias (mkrhs $startpos($3) $endpos($3) $3)) }*)
| LPAREN @{`Unclosed "("} module_type RPAREN @{`Close}
    { $2 }
| extension
    { mkmty $startpos $endpos (Pmty_extension $1) }
| module_type attribute
    { Mty.attr $1 $2 }

signature:
| (* empty *)
    { [] }
| SEMISEMI signature
    { $2 }
| signature_item signature
    { $1 @ $2 }

signature_item:
| VAL @{`Item "val"}
  val_ident COLON core_type post_item_attributes
    { mksig $startpos $endpos (Psig_value
                (Val.mk (mkrhs $startpos($2) $endpos($2) $2) $4 ~attrs:$5 ~loc:(rloc $startpos $endpos))) }
| EXTERNAL @{`Item "external"}
  val_ident COLON core_type EQUAL primitive_declaration post_item_attributes
    { mksig $startpos $endpos (Psig_value
                (Val.mk (mkrhs $startpos($2) $endpos($2) $2) $4 ~prim:$6 ~attrs:$7
                   ~loc:(rloc $startpos $endpos))) }
| TYPE @{`Item "type"}
  nonrec_flag type_declarations
    { mksig $startpos $endpos (Psig_type ($2, List.rev $3)) }
| TYPE @{`Item "type"}
  nonrec_flag sig_type_extension
    { if $2 <> Recursive then not_expecting $startpos($2) $endpos($2) "nonrec flag";
      mksig $startpos $endpos (Psig_typext $3) }
| EXCEPTION @{`Item "exception"}
  sig_exception_declaration
    { mksig $startpos $endpos (Psig_exception $2) }
| MODULE @{`Item "module"}
  UIDENT module_declaration post_item_attributes
    { mksig $startpos $endpos (Psig_module (Md.mk (mkrhs $startpos($2) $endpos($2) $2)
                             $3 ~attrs:$4 ~loc:(rloc $startpos $endpos))) }
| MODULE @{`Item "module"}
  UIDENT EQUAL mod_longident post_item_attributes
    { mksig $startpos $endpos (Psig_module (Md.mk (mkrhs $startpos($2) $endpos($2) $2)
                             (Mty.alias ~loc:(rloc $startpos($4) $endpos($4)) (mkrhs $startpos($4) $endpos($4) $4))
                             ~attrs:$5
                             ~loc:(rloc $startpos $endpos)
                          )) }
| MODULE REC @{`Item "recursive module"}
  module_rec_declarations
    { mksig $startpos $endpos (Psig_recmodule (List.rev $3)) }
| MODULE TYPE @{`Item "module type"}
  ident post_item_attributes
    { mksig $startpos $endpos (Psig_modtype (Mtd.mk (mkrhs $startpos($3) $endpos($3) $3)
                              ~attrs:$4 ~loc:(rloc $startpos $endpos))) }
| MODULE TYPE @{`Item "module type"}
  ident EQUAL module_type post_item_attributes
    { mksig $startpos $endpos (Psig_modtype (Mtd.mk (mkrhs $startpos($3) $endpos($3) $3) ~typ:$5
                              ~loc:(rloc $startpos $endpos)
                              ~attrs:$6)) }
| open_statement
    { mksig $startpos $endpos (Psig_open $1) }
| INCLUDE @{`Item "include"}
  module_type post_item_attributes %prec below_WITH
    { mksig $startpos $endpos (Psig_include (Incl.mk $2 ~attrs:$3
                                             ~loc:(rloc $startpos $endpos))) }
| CLASS @{`Item "class"}
  class_descriptions
    { mksig $startpos $endpos (Psig_class (List.rev $2)) }
| CLASS TYPE @{`Item "class type"}
  class_type_declarations
    { mksig $startpos $endpos (Psig_class_type (List.rev $3)) }
| item_extension post_item_attributes
    { mksig $startpos $endpos (Psig_extension ($1, $2)) }
| floating_attribute
    { mksig $startpos $endpos (Psig_attribute $1) }

open_statement:
| OPEN @{`Item "open"}
  override_flag mod_longident post_item_attributes
    { Opn.mk (mkrhs $startpos($3) $endpos($3) $3) ~override:$2 ~attrs:$4
        ~loc:(rloc $startpos $endpos) }

module_declaration:
| COLON module_type
    { $2 }
| LPAREN UIDENT COLON module_type RPAREN module_declaration
    { mkmty $startpos $endpos (Pmty_functor(mkrhs $startpos($2) $endpos($2) $2, Some $4, $6)) }
| LPAREN RPAREN module_declaration
    { mkmty $startpos $endpos (Pmty_functor(mkrhs $startpos($1) $endpos($1) "*", None, $3)) }

module_rec_declarations:
| module_rec_declaration
    { [$1] }
| module_rec_declarations AND module_rec_declaration
    { $3 :: $1 }

module_rec_declaration:
| UIDENT COLON module_type post_item_attributes
    { Md.mk (mkrhs $startpos($1) $endpos($1) $1) $3 ~attrs:$4 ~loc:(rloc $startpos $endpos) }

(* Class expressions *)

class_declarations:
| class_declarations AND class_declaration
    { $3 @ $1 }
| class_declaration
    { $1 }

class_declaration:
| virtual_flag class_type_parameters LIDENT class_fun_binding
  post_item_attributes
    {
      [Ci.mk (mkrhs $startpos($3) $endpos($3) $3) $4
         ~virt:$1 ~params:$2
         ~attrs:$5 ~loc:(rloc $startpos $endpos)]
    }

class_fun_binding:
| EQUAL class_expr
    { $2 }
| COLON class_type EQUAL class_expr
    { mkclass $startpos $endpos (Pcl_constraint($4, $2)) }
| labeled_simple_pattern class_fun_binding
    { let (l,o,p) = $1 in mkclass $startpos $endpos (Pcl_fun(l, o, p, $2)) }

class_type_parameters:
| (* empty *)
    { [] }
| LBRACKET type_parameter_list RBRACKET
    { List.rev $2 }

class_fun_def:
| labeled_simple_pattern MINUSGREATER class_expr
    { let (l,o,p) = $1 in mkclass $startpos $endpos (Pcl_fun(l, o, p, $3)) }
| labeled_simple_pattern class_fun_def
    { let (l,o,p) = $1 in mkclass $startpos $endpos (Pcl_fun(l, o, p, $2)) }

class_expr:
| class_simple_expr
    { $1 }
| FUN class_fun_def
    { $2 }
| class_simple_expr simple_labeled_expr_list
    { mkclass $startpos $endpos (Pcl_apply($1, List.rev $2)) }
| LET rec_flag let_bindings_no_attrs IN @{`Shift 2} class_expr
    { mkclass $startpos $endpos (Pcl_let ($2, List.rev $3, $5)) }
| class_expr attribute
    { Cl.attr $1 $2 }
| extension
    { mkclass $startpos $endpos (Pcl_extension $1) }

class_simple_expr:
| LBRACKET core_type_comma_list RBRACKET class_longident
    { mkclass $startpos $endpos (Pcl_constr(mkloc $4 (rloc $startpos($4) $endpos($4)), List.rev $2)) }
| class_longident
    { mkclass $startpos $endpos (Pcl_constr(mkrhs $startpos($1) $endpos($1) $1, [])) }
| OBJECT @{`Unclosed "object"} @{`Item "object"} class_structure END @{`Close}
    { mkclass $startpos $endpos (Pcl_structure($2)) }
| LPAREN @{`Unclosed "("} class_expr COLON class_type RPAREN @{`Close}
    { mkclass $startpos $endpos (Pcl_constraint($2, $4)) }
| LPAREN @{`Unclosed "("} class_expr RPAREN @{`Close}
    { $2 }

class_structure:
| class_self_pattern class_fields
    { Cstr.mk $1 (List.rev $2) }

class_self_pattern:
| LPAREN pattern RPAREN
    { reloc_pat $startpos $endpos $2 }
| LPAREN pattern COLON core_type RPAREN
    { mkpat $startpos $endpos (Ppat_constraint($2, $4)) }
| (* empty *)
    { ghpat $startpos $endpos (Ppat_any) }

class_fields:
| (* empty *)
    { [] }
| class_fields class_field
    { $2 @ $1 }

class_field:
| INHERIT override_flag class_expr parent_binder attrs = post_item_attributes
    { mkcf $startpos $endpos (Pcf_inherit ($2, $3, $4)) ~attrs }
| VAL value attrs = post_item_attributes
    { mkcf $startpos $endpos (Pcf_val $2) ~attrs }
| METHOD method_ attrs = post_item_attributes
    { mkcf $startpos $endpos (Pcf_method $2) ~attrs }
| CONSTRAINT constrain_field attrs = post_item_attributes
    { mkcf $startpos $endpos (Pcf_constraint $2) ~attrs }
| INITIALIZER seq_expr attrs = post_item_attributes
    { mkcf $startpos $endpos (Pcf_initializer $2) ~attrs }
| item_extension attrs = post_item_attributes
    { mkcf $startpos $endpos (Pcf_extension $1) ~attrs }
| floating_attribute
    { mkcf $startpos $endpos (Pcf_attribute $1) }

parent_binder:
| AS LIDENT
    { Some $2 }
| (* empty *)
    { None }

value:
(* TODO: factorize these rules (also with method): *)
| override_flag MUTABLE VIRTUAL label COLON core_type
    { if $1 = Override then syntax_error $startpos $endpos;
        mkloc $4 (rloc $startpos($4) $endpos($4)), Mutable, Cfk_virtual $6 }
| VIRTUAL mutable_flag label COLON core_type
    { mkrhs $startpos($3) $endpos($3) $3, $2, Cfk_virtual $5 }
| override_flag mutable_flag label EQUAL seq_expr
    { mkrhs $startpos($3) $endpos($3) $3, $2, Cfk_concrete ($1, $5) }
| override_flag mutable_flag label type_constraint EQUAL seq_expr
    {
       let e = mkexp_constraint $startpos $endpos $6 $4 in
       mkrhs $startpos($3) $endpos($3) $3, $2, Cfk_concrete ($1, e)
      }

method_:
(* TODO: factorize those rules... *)
| override_flag PRIVATE VIRTUAL label COLON poly_type
    { if $1 = Override then syntax_error $startpos $endpos;
        mkloc $4 (rloc $startpos($4) $endpos($4)), Private, Cfk_virtual $6 }
| override_flag VIRTUAL private_flag label COLON poly_type
    { if $1 = Override then syntax_error $startpos $endpos;
        mkloc $4 (rloc $startpos($4) $endpos($4)), $3, Cfk_virtual $6 }
| override_flag private_flag label strict_binding
    { mkloc $3 (rloc $startpos($3) $endpos($3)), $2, Cfk_concrete ($1, ghexp $startpos $endpos (Pexp_poly ($4, None))) }
| override_flag private_flag label COLON poly_type EQUAL seq_expr
    { mkloc $3 (rloc $startpos($3) $endpos($3)), $2, Cfk_concrete ($1, ghexp $startpos $endpos (Pexp_poly($7, Some $5))) }
| override_flag private_flag label COLON TYPE lident_list DOT core_type EQUAL v10 = seq_expr
    { let exp, poly = wrap_type_annotation $startpos $endpos $6 $8 v10 in
        mkloc $3 (rloc $startpos($3) $endpos($3)), $2, Cfk_concrete ($1, ghexp $startpos $endpos (Pexp_poly(exp, Some poly))) }

(* Class types *)
class_type:
| class_signature
    { $1 }
| QUESTION LIDENT COLON simple_core_type_or_tuple_no_attr MINUSGREATER class_type
    { mkcty $startpos $endpos (Pcty_arrow(Optional $2 , mkoption $4, $6)) }
| OPTLABEL simple_core_type_or_tuple_no_attr MINUSGREATER class_type
    { mkcty $startpos $endpos (Pcty_arrow(Optional $1, mkoption $2, $4)) }
| LIDENT COLON simple_core_type_or_tuple_no_attr MINUSGREATER class_type
    { mkcty $startpos $endpos (Pcty_arrow(Labelled $1, $3, $5)) }
| simple_core_type_or_tuple_no_attr MINUSGREATER class_type
    { mkcty $startpos $endpos (Pcty_arrow(Nolabel, $1, $3)) }

class_signature:
| LBRACKET core_type_comma_list RBRACKET clty_longident
    { mkcty $startpos $endpos (Pcty_constr (mkloc $4 (rloc $startpos($4) $endpos($4)), List.rev $2)) }
| clty_longident
    { mkcty $startpos $endpos (Pcty_constr (mkrhs $startpos($1) $endpos($1) $1, [])) }
| OBJECT @{`Unclosed "object"} @{`Item "object"} class_sig_body END @{`Close}
    { mkcty $startpos $endpos (Pcty_signature $2) }
| class_signature attribute
    { Cty.attr $1 $2 }
| extension
    { mkcty $startpos $endpos (Pcty_extension $1) }

class_sig_body:
| class_self_type class_sig_fields
    { Csig.mk $1 (List.rev $2) }

class_self_type:
| LPAREN core_type RPAREN
    { $2 }
| (* empty *)
    { mktyp $startpos $endpos (Ptyp_any) }

class_sig_fields:
| (* empty *)
    { [] }
| class_sig_fields class_sig_field
    { $2 :: $1 }

class_sig_field:
| INHERIT class_signature attrs = post_item_attributes
    { mkctf $startpos $endpos  (Pctf_inherit $2) ~attrs }
| VAL value_type attrs = post_item_attributes
    { mkctf $startpos $endpos  (Pctf_val $2) ~attrs }
| METHOD private_virtual_flags label COLON poly_type attrs = post_item_attributes
    {
      let (p, v) = $2 in
      mkctf $startpos $endpos  (Pctf_method ($3, p, v, $5)) ~attrs
    }
| CONSTRAINT constrain_field attrs = post_item_attributes
    { mkctf $startpos $endpos  (Pctf_constraint $2) ~attrs }
| item_extension attrs = post_item_attributes
    { mkctf $startpos $endpos (Pctf_extension $1) ~attrs }
| floating_attribute
    { mkctf $startpos $endpos (Pctf_attribute $1) }

value_type:
| VIRTUAL mutable_flag label COLON core_type
    { $3, $2, Virtual, $5 }
| MUTABLE virtual_flag label COLON core_type
    { $3, Mutable, $2, $5 }
| label COLON core_type
    { $1, Immutable, Concrete, $3 }

constrain:
| core_type EQUAL core_type
    { $1, $3, (rloc $startpos $endpos) }

constrain_field:
| core_type EQUAL core_type
    { $1, $3 }

class_descriptions:
| class_descriptions AND class_description
    { $3 @ $1 }
| class_description
    { $1 }

class_description:
| virtual_flag class_type_parameters LIDENT COLON class_type post_item_attributes
    {
      [Ci.mk (mkrhs $startpos($3) $endpos($3) $3) $5
         ~virt:$1 ~params:$2
         ~attrs:$6 ~loc:(rloc $startpos $endpos)]
    }

class_type_declarations:
| class_type_declarations AND class_type_declaration
    { $3 @ $1 }
| class_type_declaration
    { $1 }

class_type_declaration:
| virtual_flag class_type_parameters LIDENT EQUAL class_signature post_item_attributes
    {
      [Ci.mk (mkrhs $startpos($3) $endpos($3) $3) $5
         ~virt:$1 ~params:$2
         ~attrs:$6 ~loc:(rloc $startpos $endpos)]
    }

(* Core expressions *)

seq_expr:
| expr %prec below_SEMI
    { $1 }
| expr SEMI
    { reloc_exp $startpos $endpos $1 }
| expr SEMI @{`Shift 1} seq_expr
    { mkexp $startpos $endpos (Pexp_sequence($1, $3)) }

labeled_simple_pattern:
| QUESTION LPAREN label_let_pattern opt_default RPAREN
    { (Optional (fst $3), $4, snd $3) }
| QUESTION label_var
    { (Optional (fst $2), None, snd $2) }
| OPTLABEL LPAREN let_pattern opt_default RPAREN
    { (Optional $1, $4, $3) }
| OPTLABEL pattern_var
    { (Optional $1, None, $2) }
| TILDE LPAREN label_let_pattern RPAREN
    { (Labelled (fst $3), None, snd $3) }
| TILDE label_var
    { (Labelled (fst $2), None, snd $2) }
| LABEL simple_pattern
    { (Labelled $1, None, $2) }
| simple_pattern
    { (Nolabel, None, $1) }

pattern_var:
| LIDENT
    { mkpat $startpos $endpos (Ppat_var (mkrhs $startpos($1) $endpos($1) $1)) }
| UNDERSCORE
    { mkpat $startpos $endpos  Ppat_any }

opt_default:
| (* empty *)
    { None }
| EQUAL seq_expr
    { Some $2 }

label_let_pattern:
| label_var
    { $1 }
| label_var COLON core_type
    { let (lab, pat) = $1 in (lab, mkpat $startpos $endpos (Ppat_constraint(pat, $3))) }

label_var:
| LIDENT
    { ($1, mkpat $startpos $endpos (Ppat_var (mkrhs $startpos($1) $endpos($1) $1))) }

let_pattern:
| pattern
    { $1 }
| pattern COLON core_type
    { mkpat $startpos $endpos (Ppat_constraint($1, $3)) }

expr:
| simple_expr %prec below_SHARP
    { $1 }
| simple_expr simple_labeled_expr_list
    { mkexp $startpos $endpos (Pexp_apply($1, List.rev $2)) }
| LET @{`Item "let"} ext_attributes rec_flag let_bindings_no_attrs _in = IN @{`Shift 2} expr = seq_expr
    { let expr = reloc_exp_fake $endpos(_in) $endpos expr in
      mkexp_attrs $startpos $endpos (Pexp_let($3, List.rev $4, expr)) $2 }
| LET MODULE @{`Item "let module"}
  ext_attributes UIDENT module_binding_body _in = IN @{`Shift 2} expr = seq_expr
    { let expr = reloc_exp_fake $endpos(_in) $endpos expr in
      mkexp_attrs $startpos $endpos (Pexp_letmodule(mkrhs $startpos($4) $endpos($4) $4, $5, expr)) $3 }
| LET OPEN @{`Item "let open"}
  expr_open _in = IN @{`Shift 2} expr = seq_expr
    { let expr = reloc_exp_fake $endpos(_in) $endpos expr in
      let (flag,id,ext) = $3 in
      mkexp_attrs $startpos $endpos (Pexp_open(flag, id, expr)) ext }
| FUNCTION @{`Item "function"}
  ext_attributes opt_bar match_cases
    { mkexp_attrs $startpos $endpos (Pexp_function(List.rev $4)) $2 }
| FUN @{`Item "fun"}
  ext_attributes labeled_simple_pattern fun_def
    { let (l,o,p) = $3 in
        mkexp_attrs $startpos $endpos (Pexp_fun(l, o, p, $4)) $2 }
| FUN @{`Item "fun"}
  ext_attributes newtype fun_def
    { mkexp_attrs $startpos $endpos (Pexp_newtype($3, $4)) $2 }
| MATCH @{`Item "match"}
  ext_attributes seq_expr WITH opt_bar match_cases
    { mkexp_attrs $startpos $endpos (Pexp_match($3, List.rev $6)) $2 }
| TRY @{`Item "try"}
  ext_attributes seq_expr WITH opt_bar match_cases
    { mkexp_attrs $startpos $endpos (Pexp_try($3, List.rev $6)) $2 }
| expr_comma_list %prec below_COMMA
    { mkexp $startpos $endpos (Pexp_tuple(List.rev $1)) }
| constr_longident simple_expr %prec below_SHARP
    { mkexp $startpos $endpos (Pexp_construct(mkrhs $startpos($1) $endpos($1) $1, Some $2)) }
| name_tag simple_expr %prec below_SHARP
    { mkexp $startpos $endpos (Pexp_variant($1, Some $2)) }
| IF @{`Item "if"}
  ext_attributes seq_expr
  THEN @{`Item "then clause"} expr
  ELSE @{`Item "else clause"} expr
    { mkexp_attrs $startpos $endpos (Pexp_ifthenelse($3, $5, Some $7)) $2 }
| IF @{`Item "if"}
  ext_attributes seq_expr
  THEN @{`Item "then clause"} expr
    { mkexp_attrs $startpos $endpos (Pexp_ifthenelse($3, $5, None)) $2 }
| WHILE @{`Item "while"}
  ext_attributes seq_expr DO @{`Item "while body"} seq_expr DONE
    { mkexp_attrs $startpos $endpos (Pexp_while($3, $5)) $2 }
| FOR @{`Item "for"}
  ext_attributes pattern EQUAL seq_expr direction_flag seq_expr
  DO @{`Item "for body"} seq_expr DONE
    { mkexp_attrs $startpos $endpos (Pexp_for($3, $5, $7, $6, $9)) $2 }
| expr COLONCOLON expr
    { mkexp_cons (rloc $startpos($2) $endpos($2)) (ghexp $startpos $endpos (Pexp_tuple[$1;$3])) (rloc $startpos $endpos) }
| LPAREN COLONCOLON RPAREN LPAREN expr COMMA expr RPAREN
    { mkexp_cons (rloc $startpos($2) $endpos($2)) (ghexp $startpos $endpos (Pexp_tuple[$5;$7])) (rloc $startpos $endpos) }
| expr INFIXOP0 expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) $2 $3 }
| expr INFIXOP1 expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) $2 $3 }
| expr INFIXOP2 expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) $2 $3 }
| expr INFIXOP3 expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) $2 $3 }
| expr INFIXOP4 expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) $2 $3 }
| expr PLUS expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "+" $3 }
| expr PLUSDOT expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "+." $3 }
| expr PLUSEQ expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "+=" $3 }
| expr MINUS expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "-" $3 }
| expr MINUSDOT expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "-." $3 }
| expr STAR expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "*" $3 }
| expr PERCENT expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "%" $3 }
| expr EQUAL expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "=" $3 }
| expr LESS expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "<" $3 }
| expr GREATER expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) ">" $3 }
| expr OR expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "or" $3 }
| expr BARBAR expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "||" $3 }
| expr AMPERSAND expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "&" $3 }
| expr AMPERAMPER expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "&&" $3 }
| expr COLONEQUAL expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) ":=" $3 }
| subtractive expr %prec prec_unary_minus
    { mkuminus $startpos $endpos $1 $2 }
| additive expr %prec prec_unary_plus
    { mkuplus $startpos $endpos $1 $2 }
| simple_expr DOT label_longident LESSMINUS expr
    { mkexp $startpos $endpos (Pexp_setfield($1, mkrhs $startpos($3) $endpos($3) $3, $5)) }
| simple_expr _ops = DOT _ope = LPAREN seq_expr RPAREN LESSMINUS expr
    { mkexp $startpos $endpos
          (Pexp_apply(ghexp $startpos(_ops) $endpos(_ope)
                 (Pexp_ident(array_function $startpos(_ops) $endpos(_ope) ".()" false)),
                         [Nolabel,$1; Nolabel,$4; Nolabel,$7])) }
| simple_expr _ops = DOT _ope = LBRACKET seq_expr RBRACKET LESSMINUS expr
    { mkexp $startpos $endpos
          (Pexp_apply(ghexp $startpos(_ops) $endpos(_ope)
                 (Pexp_ident(array_function $startpos(_ops) $endpos(_ope) ".[]" false)),
                         [Nolabel,$1; Nolabel,$4; Nolabel,$7])) }
| simple_expr _ops = DOT _ope = LBRACE expr RBRACE LESSMINUS expr
    { bigarray_set ($startpos,$endpos) ($startpos(_ops),$endpos(_ope)) $1 $4 $7 }
| label LESSMINUS expr
    { mkexp $startpos $endpos (Pexp_setinstvar(mkrhs $startpos($1) $endpos($1) $1, $3)) }
| ASSERT ext_attributes simple_expr %prec below_SHARP
    { mkexp_attrs $startpos $endpos (Pexp_assert $3) $2 }
| LAZY ext_attributes simple_expr %prec below_SHARP
    { mkexp_attrs $startpos $endpos (Pexp_lazy $3) $2 }
| OBJECT @{`Unclosed "object"} @{`Item "object"} ext_attributes class_structure END @{`Close}
    { mkexp_attrs $startpos $endpos (Pexp_object $3) $2 }
| expr attribute
    { Exp.attr $1 $2 }

simple_expr:
| val_longident
    { mkexp $startpos $endpos (Pexp_ident (mkrhs $startpos($1) $endpos($1) $1)) }
| constant
    { mkexp $startpos $endpos (Pexp_constant $1) }
| constr_longident %prec prec_constant_constructor
    { mkexp $startpos $endpos (Pexp_construct(mkrhs $startpos($1) $endpos($1) $1, None)) }
| name_tag %prec prec_constant_constructor
    { mkexp $startpos $endpos (Pexp_variant($1, None)) }
| LPAREN @{`Unclosed "("} seq_expr RPAREN @{`Close}
    { reloc_exp $startpos $endpos $2 }
| BEGIN ext_attributes seq_expr END
    { wrap_exp_attrs $startpos $endpos (reloc_exp $startpos $endpos $3) $2 (* check location *) }
| BEGIN @{`Unclosed "begin"} ext_attributes END @{`Close}
    { mkexp_attrs $startpos $endpos (Pexp_construct (mkloc (Lident "()") (rloc $startpos $endpos),
                               None)) $2 }
| LPAREN seq_expr type_constraint RPAREN
    { mkexp_constraint $startpos $endpos $2 $3 }
| simple_expr DOT label_longident
    { mkexp $startpos $endpos (Pexp_field($1, mkrhs $startpos($3) $endpos($3) $3)) }
| mod_longident DOT LPAREN @{`Unclosed "("} seq_expr RPAREN @{`Close}
    { mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos($1) $endpos($1) $1, $4)) }
| simple_expr _ops = DOT _ope = LPAREN @{`Unclosed "("} seq_expr RPAREN @{`Close}
    { mkexp $startpos $endpos
          (Pexp_apply(ghexp $startpos(_ops) $endpos(_ope)
                 (Pexp_ident(array_function $startpos(_ops) $endpos(_ope) ".()" false)),
                         [Nolabel,$1; Nolabel,$4])) }
| simple_expr _ops = DOT _ope = LBRACKET @{`Unclosed "["} seq_expr RBRACKET @{`Close}
    { mkexp $startpos $endpos
          (Pexp_apply(ghexp $startpos(_ops) $endpos(_ope)
                 (Pexp_ident(array_function $startpos(_ops) $endpos(_ope) ".[]" false)),
                         [Nolabel,$1; Nolabel,$4])) }
| simple_expr _ops = DOT _ope = LBRACE @{`Unclosed "{"} expr RBRACE @{`Close}
    { bigarray_get ($startpos,$endpos) ($startpos(_ops),$endpos(_ope)) $1 $4 }
| LBRACE @{`Unclosed "{"} record_expr RBRACE @{`Close}
    { let (exten, fields) = $2 in mkexp $startpos $endpos (Pexp_record(fields, exten)) }
| mod_longident DOT LBRACE @{`Unclosed "{"}  record_expr RBRACE @{`Close}
    { let (exten, fields) = $4 in
        let rec_exp = mkexp $startpos $endpos (Pexp_record(fields, exten)) in
        mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos($1) $endpos($1) $1, rec_exp)) }
| LBRACKETBAR @{`Unclosed "[|"} expr_semi_list opt_semi BARRBRACKET @{`Close}
    { mkexp $startpos $endpos  (Pexp_array(List.rev $2)) }
| LBRACKETBAR BARRBRACKET
    { mkexp $startpos $endpos  (Pexp_array []) }
| mod_longident DOT LBRACKETBAR @{`Unclosed "[|"} expr_semi_list opt_semi BARRBRACKET @{`Close}
    { mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos($1) $endpos($1) $1, mkexp $startpos($4) $endpos($4) (Pexp_array(List.rev $4)))) }
| LBRACKET @{`Unclosed "["} expr_semi_list opt_semi RBRACKET @{`Close}
    { reloc_exp $startpos $endpos (mktailexp $startpos($4) $endpos($4) (List.rev $2)) }
| mod_longident DOT LBRACKET @{`Unclosed "["} expr_semi_list opt_semi RBRACKET @{`Close}
    { let list_exp = reloc_exp $startpos $endpos (mktailexp $startpos($6) $endpos($6) (List.rev $4)) in
        mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos($1) $endpos($1) $1, list_exp)) }
| PREFIXOP simple_expr
    { mkexp $startpos $endpos (Pexp_apply(mkoperator $startpos($1) $endpos($1) $1, [Nolabel,$2])) }
| BANG simple_expr
    { mkexp $startpos $endpos (Pexp_apply(mkoperator $startpos($1) $endpos($1) "!", [Nolabel,$2])) }
| NEW ext_attributes class_longident
    { mkexp_attrs $startpos $endpos (Pexp_new(mkrhs $startpos($3) $endpos($3) $3)) $2 }
| LBRACELESS @{`Unclosed "{<"} field_expr_list GREATERRBRACE
    { mkexp $startpos $endpos  (Pexp_override $2) }
| LBRACELESS GREATERRBRACE
    { mkexp $startpos $endpos  (Pexp_override [])}
| mod_longident DOT LBRACELESS @{`Unclosed "{<"} field_expr_list GREATERRBRACE @{`Close}
    { mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos($1) $endpos($1) $1, mkexp $startpos($4) $endpos($4) (Pexp_override $4))) }
| simple_expr SHARP @{`Shift_token (1,LIDENT "")} label
    { mkexp $startpos $endpos (Pexp_send($1, $3)) }
| simple_expr SHARPOP @{`Shift_token (1,LIDENT "")} simple_expr
    { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) $2 $3 }
| LPAREN @{`Unclosed "("} MODULE module_expr RPAREN @{`Close}
    { mkexp $startpos $endpos  (Pexp_pack $3) }
| LPAREN @{`Unclosed "("} MODULE module_expr COLON package_type RPAREN @{`Close}
    { mkexp $startpos $endpos  (Pexp_constraint (ghexp $startpos $endpos (Pexp_pack $3),
                                ghtyp $startpos $endpos (Ptyp_package $5))) }
| mod_longident DOT LPAREN @{`Unclosed "("} MODULE module_expr COLON package_type RPAREN @{`Close}
    { mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos($1) $endpos($1) $1,
        mkexp $startpos $endpos (Pexp_constraint (ghexp $startpos $endpos (Pexp_pack $5),
                                ghtyp $startpos $endpos (Ptyp_package $7))))) }
| extension
    { mkexp $startpos $endpos  (Pexp_extension $1) }

simple_labeled_expr_list:
| labeled_simple_expr
    { [$1] }
| simple_labeled_expr_list labeled_simple_expr
    { $2 :: $1 }

labeled_simple_expr:
| simple_expr %prec below_SHARP
    { (Nolabel, $1) }
| label_expr
    { $1 }

label_expr:
| LABEL simple_expr %prec below_SHARP
    { (Labelled $1, $2) }
| TILDE label_ident
    { (Labelled (fst $2), snd $2) }
| QUESTION label_ident
    { (Optional (fst $2), snd $2) }
| OPTLABEL simple_expr %prec below_SHARP
    { (Optional $1, $2) }

label_ident:
| LIDENT
    { ($1, mkexp $startpos $endpos (Pexp_ident(mkrhs $startpos($1) $endpos($1) (Lident $1)))) }

let_bindings:
| let_binding
    { [$1] }
| let_bindings AND let_binding
    { $3 :: $1 }

let_bindings_no_attrs:
| l = let_bindings
    { List.iter (fun vb -> if vb.pvb_attributes <> [] then
        Parsing_aux.raise_warning
          (Syntaxerr.(Error(Not_expecting(vb.pvb_loc,"item attribute")))))
        l;
      l }

lident_list:
| LIDENT
    { [$1] }
| LIDENT lident_list
    { $1 :: $2 }

let_binding:
| let_binding_ post_item_attributes
    { let (p, e) = $1 in Vb.mk ~loc:(rloc $startpos $endpos) ~attrs:$2 p e }

let_binding_:
| val_ident fun_binding
    { (mkpatvar $startpos($1) $endpos($1) $1, $2) }
| val_ident COLON typevar_list DOT core_type EQUAL seq_expr
    { (ghpat $startpos $endpos (Ppat_constraint(mkpatvar $startpos($1) $endpos($1) $1,
                               ghtyp $startpos $endpos (Ptyp_poly(List.rev $3,$5)))),
         $7) }
| val_ident COLON TYPE lident_list DOT core_type EQUAL seq_expr
    { let exp, poly = wrap_type_annotation $startpos $endpos $4 $6 $8 in
        (ghpat $startpos $endpos (Ppat_constraint(mkpatvar $startpos($1) $endpos($1) $1, poly)), exp) }
| pattern EQUAL seq_expr
    { ($1, $3) }
| simple_pattern_not_ident COLON core_type EQUAL seq_expr
    { (ghpat $startpos $endpos (Ppat_constraint($1, $3)), $5) }

fun_binding:
| strict_binding
    { $1 }
| type_constraint EQUAL seq_expr
    { mkexp_constraint $startpos $endpos $3 $1 }

strict_binding:
| EQUAL seq_expr
    { $2 }
| labeled_simple_pattern fun_binding
    { let (l, o, p) = $1 in ghexp $startpos $endpos (Pexp_fun(l, o, p, $2)) }
| LPAREN TYPE LIDENT RPAREN fun_binding
    { mkexp $startpos $endpos (Pexp_newtype($3, $5)) }

match_cases:
| match_case
    { [$1] }
| match_cases @{`Indent (-2)} BAR match_case
    { $3 :: $1 }

match_case:
| @{`Item "pattern"} pattern
  MINUSGREATER @{`Item "match action"} expr = seq_expr
    { Exp.case $1 (reloc_exp_fake $endpos($2) $endpos expr) }
| @{`Item "pattern"} pattern
  WHEN @{`Item "when guard"} seq_expr
  MINUSGREATER @{`Item "match action"} expr = seq_expr
    { Exp.case $1 ~guard:$3 (reloc_exp_fake $endpos($4) $endpos expr) }

fun_def:
| MINUSGREATER seq_expr
(* Cf #5939: we used to accept (fun p when e0 -> e) *)
    { $2 }
| labeled_simple_pattern fun_def
    {
       let (l,o,p) = $1 in
       ghexp $startpos $endpos (Pexp_fun(l, o, p, $2))
      }
| LPAREN TYPE LIDENT RPAREN fun_def
    { mkexp $startpos $endpos (Pexp_newtype($3, $5)) }

expr_comma_list:
| expr_comma_list COMMA expr
    { $3 :: $1 }
| expr COMMA expr
    { [$3; $1] }

record_expr:
| simple_expr WITH lbl_expr_list
    { (Some $1, $3) }
| lbl_expr_list
    { (None, $1) }

lbl_expr_list:
| lbl_expr
    { [$1] }
| lbl_expr SEMI lbl_expr_list
    { $1 :: $3 }
| lbl_expr SEMI
    { [$1] }

lbl_expr:
| label_longident EQUAL expr
    { (mkrhs $startpos($1) $endpos($1) $1,$3) }
| label_longident
    { (mkrhs $startpos($1) $endpos($1) $1, exp_of_label $startpos($1) $endpos($1) $1) }

field_expr_list:
| field_expr opt_semi { [$1] }
| field_expr SEMI field_expr_list { $1 :: $3 }

field_expr:
| label EQUAL expr
    { (mkrhs $startpos($1) $endpos($1) $1,$3) }
| label
    { (mkrhs $startpos($1) $endpos($1) $1, exp_of_label $startpos($1) $endpos($1) (Lident $1)) }

expr_semi_list:
| expr
    { [$1] }
| expr_semi_list SEMI expr
    { $3 :: $1 }

type_constraint:
| COLON @{`Item "type constraint"}
  core_type
    { (Some $2, None) }
| COLON @{`Item "type constraint"}
  core_type COLONGREATER core_type
    { (Some $2, Some $4) }
| COLONGREATER @{`Item "type constraint"}
  core_type
    { (None, Some $2) }

(* Patterns *)

pattern:
| simple_pattern
    { $1 }
| pattern AS val_ident
    { mkpat $startpos $endpos (Ppat_alias($1, mkrhs $startpos($3) $endpos($3) $3)) }
(*| pattern AS error
    { expecting $startpos($3) $endpos($3) "identifier" }*)
| pattern_comma_list %prec below_COMMA
    { mkpat $startpos $endpos (Ppat_tuple(List.rev $1)) }
| constr_longident pattern %prec prec_constr_appl
    { mkpat $startpos $endpos (Ppat_construct(mkrhs $startpos($1) $endpos($1) $1, Some $2)) }
| name_tag pattern %prec prec_constr_appl
    { mkpat $startpos $endpos (Ppat_variant($1, Some $2)) }
| pattern COLONCOLON pattern
    { mkpat_cons (rloc $startpos($2) $endpos($2)) (ghpat $startpos $endpos (Ppat_tuple[$1;$3])) (rloc $startpos $endpos) }
(*| pattern COLONCOLON error
    { expecting $startpos($3) $endpos($3) "pattern" }*)
| LPAREN COLONCOLON RPAREN LPAREN pattern COMMA pattern RPAREN
    { mkpat_cons (rloc $startpos($2) $endpos($2)) (ghpat $startpos $endpos (Ppat_tuple[$5;$7])) (rloc $startpos $endpos) }
| pattern BAR pattern
    { mkpat $startpos $endpos (Ppat_or($1, $3)) }
(*| pattern BAR error
    { expecting $startpos($3) $endpos($3) "pattern" }*)
| LAZY simple_pattern
    { mkpat $startpos $endpos (Ppat_lazy $2) }
| EXCEPTION pattern %prec prec_constr_appl
    { mkpat $startpos $endpos (Ppat_exception $2) }
| pattern attribute
    { Pat.attr $1 $2 }

simple_pattern:
| val_ident %prec below_EQUAL
    { mkpat $startpos $endpos (Ppat_var (mkrhs $startpos($1) $endpos($1) $1)) }
| simple_pattern_not_ident
    { $1 }

simple_pattern_not_ident:
| UNDERSCORE
    { mkpat $startpos $endpos (Ppat_any) }
| signed_constant
    { mkpat $startpos $endpos (Ppat_constant $1) }
| signed_constant DOTDOT signed_constant
    { mkpat $startpos $endpos (Ppat_interval ($1, $3)) }
| constr_longident
    { mkpat $startpos $endpos (Ppat_construct(mkrhs $startpos($1) $endpos($1) $1, None)) }
| name_tag
    { mkpat $startpos $endpos (Ppat_variant($1, None)) }
| SHARP type_longident
    { mkpat $startpos $endpos (Ppat_type (mkrhs $startpos($2) $endpos($2) $2)) }
| LBRACE @{`Unclosed "{"}
  lbl_pattern_list RBRACE @{`Close}
    { let (fields, closed) = $2 in mkpat $startpos $endpos (Ppat_record(fields, closed)) }
| LBRACKET @{`Unclosed "["}
  pattern_semi_list opt_semi RBRACKET @{`Close}
    { reloc_pat $startpos $endpos (mktailpat $startpos($4) $endpos($4) (List.rev $2)) }
| LBRACKETBAR @{`Unclosed "[|"}
  pattern_semi_list opt_semi BARRBRACKET @{`Close}
    { mkpat $startpos $endpos (Ppat_array(List.rev $2)) }
| LBRACKETBAR BARRBRACKET
    { mkpat $startpos $endpos (Ppat_array []) }
| LPAREN @{`Unclosed "("} pattern RPAREN @{`Close}
    { reloc_pat $startpos $endpos $2 }
| LPAREN @{`Unclosed "("} pattern COLON core_type RPAREN @{`Close}
    { mkpat $startpos $endpos (Ppat_constraint($2, $4)) }
(*| LPAREN pattern COLON error
    { expecting $startpos($4) $endpos($4) "type" }*)
| LPAREN MODULE @{`Unclosed "("} UIDENT RPAREN @{`Close}
    { mkpat $startpos $endpos (Ppat_unpack (mkrhs $startpos($3) $endpos($3) $3)) }
| LPAREN MODULE @{`Unclosed "("} UIDENT COLON package_type RPAREN @{`Close}
    { mkpat $startpos $endpos (Ppat_constraint(mkpat $startpos $endpos(Ppat_unpack (mkrhs $startpos($3) $endpos($3) $3)),
                              ghtyp $startpos $endpos (Ptyp_package $5))) }
| extension
    { mkpat $startpos $endpos (Ppat_extension $1) }

pattern_comma_list:
| pattern_comma_list COMMA pattern
    { $3 :: $1 }
| pattern COMMA pattern
    { [$3; $1] }
(*| pattern COMMA error
    { expecting $startpos($3) $endpos($3) "pattern" }*)

pattern_semi_list:
| pattern
    { [$1] }
| pattern_semi_list SEMI pattern
    { $3 :: $1 }

lbl_pattern_list:
| lbl_pattern
    { [$1], Closed }
| lbl_pattern SEMI
    { [$1], Closed }
| lbl_pattern SEMI UNDERSCORE opt_semi
    { [$1], Open }
| lbl_pattern SEMI lbl_pattern_list
    { let (fields, closed) = $3 in $1 :: fields, closed }

lbl_pattern:
| label_longident EQUAL pattern
    { (mkrhs $startpos($1) $endpos($1) $1,$3) }
| label_longident
    { (mkrhs $startpos($1) $endpos($1) $1, pat_of_label $startpos($1) $endpos($1) $1) }

(* Primitive declarations *)

primitive_declaration:
| STRING
    { [fst $1] }
| STRING primitive_declaration
    { fst $1 :: $2 }

(* Type declarations *)

type_declarations:
| type_declaration
    { [$1] }
| type_declarations AND type_declaration
    { $3 :: $1 }

type_declaration:
| optional_type_parameters LIDENT type_kind constraints post_item_attributes
    { let (kind, priv, manifest) = $3 in
        Type.mk (mkrhs $startpos($2) $endpos($2) $2)
          ~params:$1 ~cstrs:(List.rev $4)
          ~kind ~priv ?manifest ~attrs:$5 ~loc:(rloc $startpos $endpos)
       }

constraints:
| constraints CONSTRAINT constrain
    { $3 :: $1 }
| (* empty *)
    { [] }

type_kind:
| (* empty *)
    { (Ptype_abstract, Public, None) }
| EQUAL core_type
    { (Ptype_abstract, Public, Some $2) }
| EQUAL PRIVATE core_type
    { (Ptype_abstract, Private, Some $3) }
| EQUAL constructor_declarations
    { (Ptype_variant(List.rev $2), Public, None) }
| EQUAL PRIVATE constructor_declarations
    { (Ptype_variant(List.rev $3), Private, None) }
| EQUAL private_flag BAR constructor_declarations
    { (Ptype_variant(List.rev $4), $2, None) }
| EQUAL private_flag LBRACE label_declarations opt_semi RBRACE
    { (Ptype_record(List.rev $4), $2, None) }
| EQUAL core_type EQUAL private_flag opt_bar constructor_declarations
    { (Ptype_variant(List.rev $6), $4, Some $2) }
| EQUAL core_type EQUAL private_flag LBRACE label_declarations opt_semi RBRACE
    { (Ptype_record(List.rev $6), $4, Some $2) }
| EQUAL DOTDOT
    { (Ptype_open, Public, None) }
| EQUAL core_type EQUAL DOTDOT
    { (Ptype_open, Public, Some $2) }

optional_type_parameters:
| (* empty *)
    { [] }
| optional_type_parameter
    { [$1] }
| LPAREN optional_type_parameter_list RPAREN
    { List.rev $2 }

optional_type_parameter:
| type_variance optional_type_variable
    { $2, $1 }

optional_type_parameter_list:
| optional_type_parameter
    { [$1] }
| optional_type_parameter_list COMMA optional_type_parameter
    { $3 :: $1 }

optional_type_variable:
| QUOTE ident
    { mktyp $startpos $endpos (Ptyp_var $2) }
| UNDERSCORE
    { mktyp $startpos $endpos (Ptyp_any) }

type_parameters:
| (* empty *)
    { [] }
| type_parameter
    { [$1] }
| LPAREN type_parameter_list RPAREN
    { List.rev $2 }

type_parameter:
| type_variance type_variable
    { $2, $1 }

type_variance:
| (* empty *)
    { Invariant }
| PLUS
    { Covariant }
| MINUS
    { Contravariant }

type_variable:
| QUOTE ident
    { mktyp $startpos $endpos (Ptyp_var $2) }

type_parameter_list:
| type_parameter
    { [$1] }
| type_parameter_list COMMA type_parameter
    { $3 :: $1 }

constructor_declarations:
| constructor_declaration
    { [$1] }
| constructor_declarations @{`Indent (-2)} BAR constructor_declaration
    { $3 :: $1 }

constructor_declaration:
| constr_ident generalized_constructor_arguments attributes
    {
      let args,res = $2 in
      Type.constructor (mkrhs $startpos($1) $endpos($1) $1) ~args ?res ~loc:(rloc $startpos $endpos) ~attrs:$3
    }

str_exception_declaration:
| extension_constructor_declaration post_item_attributes
    {
      let ext = $1 in
      {ext with pext_attributes = ext.pext_attributes @ $2}
    }
| extension_constructor_rebind post_item_attributes
    {
      let ext = $1 in
      {ext with pext_attributes = ext.pext_attributes @ $2}
    }

sig_exception_declaration:
| extension_constructor_declaration post_item_attributes
    {
      let ext = $1 in
      {ext with pext_attributes = ext.pext_attributes @ $2}
    }

generalized_constructor_arguments:
| (* empty *)
    { (Pcstr_tuple [],None) }
| OF constructor_arguments
    { ($2,None) }
| COLON constructor_arguments MINUSGREATER simple_core_type_no_attr
    { ($2,Some $4) }
| COLON simple_core_type_no_attr
    { (Pcstr_tuple [],Some $2) }

constructor_arguments:
| core_type_list { Pcstr_tuple (List.rev $1) }
| LBRACE label_declarations opt_semi RBRACE { Pcstr_record (List.rev $2) }

label_declarations:
| label_declaration
    { [$1] }
| label_declarations SEMI label_declaration
    { $3 :: $1 }

label_declaration:
| mutable_flag label COLON poly_type_no_attr attributes
  {
    Type.field (mkrhs $startpos($2) $endpos($2) $2) $4 ~mut:$1 ~attrs:$5 ~loc:(rloc $startpos $endpos)
  }

(* Type extensions *)

str_type_extension:
| optional_type_parameters type_longident
  PLUSEQ private_flag opt_bar str_extension_constructors
  post_item_attributes
    { Te.mk (mkrhs $startpos($2) $endpos($2) $2) (List.rev $6)
        ~params:$1 ~priv:$4 ~attrs:$7 }

sig_type_extension:
| optional_type_parameters type_longident
  PLUSEQ private_flag opt_bar sig_extension_constructors
  post_item_attributes
    { Te.mk (mkrhs $startpos($2) $endpos($2) $2) (List.rev $6)
        ~params:$1 ~priv:$4 ~attrs:$7 }

str_extension_constructors:
| extension_constructor_declaration
    { [$1] }
| extension_constructor_rebind
    { [$1] }
| str_extension_constructors BAR extension_constructor_declaration
    { $3 :: $1 }
| str_extension_constructors BAR extension_constructor_rebind
    { $3 :: $1 }

sig_extension_constructors:
| extension_constructor_declaration
    { [$1] }
| sig_extension_constructors BAR extension_constructor_declaration
    { $3 :: $1 }

extension_constructor_declaration:
| constr_ident generalized_constructor_arguments attributes
    { let args, res = $2 in
      Te.decl (mkrhs $startpos($1) $endpos($1) $1) ~args ?res
              ~loc:(rloc $startpos $endpos) ~attrs:$3
    }

extension_constructor_rebind:
| constr_ident EQUAL constr_longident attributes
    { Te.rebind (mkrhs $startpos($1) $endpos($1) $1)
                (mkrhs $startpos($3) $endpos($3) $3)
                ~loc:(rloc $startpos $endpos) ~attrs:$4
    }

(* "with" constraints (additional type equations over signature components) *)

with_constraints:
| with_constraint
    { $1 }
| with_constraints AND with_constraint
    { $3 @ $1 }

with_constraint:
| TYPE type_parameters label_longident with_type_binder core_type_no_attr constraints
    { [Pwith_type
          (mkrhs $startpos($3) $endpos($3) $3,
           (Type.mk (mkrhs $startpos($3) $endpos($3) (Longident.last $3))
              ~params:$2
              ~cstrs:(List.rev $6)
              ~manifest:$5
              ~priv:$4
              ~loc:(rloc $startpos $endpos)))] }
| TYPE type_parameters label COLONEQUAL core_type_no_attr
    { [Pwith_typesubst
          (Type.mk (mkrhs $startpos($3) $endpos($3) $3)
             ~params:$2
             ~manifest:$5
             ~loc:(rloc $startpos $endpos))] }
| MODULE mod_longident EQUAL mod_ext_longident
    { [Pwith_module (mkrhs $startpos($2) $endpos($2) $2, mkrhs $startpos($4) $endpos($4) $4)] }
| MODULE UIDENT COLONEQUAL mod_ext_longident
    { [Pwith_modsubst (mkrhs $startpos($2) $endpos($2) $2, mkrhs $startpos($4) $endpos($4) $4)] }

with_type_binder:
| EQUAL
    { Public }
| EQUAL PRIVATE
    { Private }

(* Polymorphic types *)

typevar_list:
| QUOTE ident
    { [$2] }
| typevar_list QUOTE ident
    { $3 :: $1 }

poly_type:
| core_type
    { $1 }
| typevar_list DOT core_type
    { mktyp $startpos $endpos (Ptyp_poly(List.rev $1, $3)) }

poly_type_no_attr:
| core_type_no_attr
    { $1 }
| typevar_list DOT core_type_no_attr
    { mktyp $startpos $endpos (Ptyp_poly(List.rev $1, $3)) }

(* Core types *)

core_type:
| core_type_no_attr
    { $1 }
| core_type attribute
    { Typ.attr $1 $2 }

core_type_no_attr:
| core_type2
    { $1 }
| core_type2 AS QUOTE ident
    { mktyp $startpos $endpos (Ptyp_alias($1, $4)) }

core_type2:
| simple_core_type_or_tuple
    { $1 }
| QUESTION LIDENT COLON core_type2 MINUSGREATER core_type2
    { mktyp $startpos $endpos (Ptyp_arrow(Optional $2 , mkoption $4, $6)) }
| OPTLABEL core_type2 MINUSGREATER core_type2
    { mktyp $startpos $endpos (Ptyp_arrow(Optional $1 , mkoption $2, $4)) }
| LIDENT COLON core_type2 MINUSGREATER core_type2
    { mktyp $startpos $endpos (Ptyp_arrow(Labelled $1, $3, $5)) }
| core_type2 MINUSGREATER core_type2
    { mktyp $startpos $endpos (Ptyp_arrow(Nolabel, $1, $3)) }

simple_core_type:
| simple_core_type2 %prec below_SHARP
    { $1 }
| LPAREN core_type_comma_list RPAREN %prec below_SHARP
    { match $2 with [sty] -> sty
                  | _ ->
                    syntax_error $startpos $endpos;
                    mktyp $startpos $endpos (Ptyp_any)
    }

simple_core_type_no_attr:
| simple_core_type2 %prec below_SHARP
    { $1 }
| LPAREN core_type_comma_list RPAREN %prec below_SHARP
    { match $2 with [sty] -> sty
                  | _ ->
                    syntax_error $startpos $endpos;
                    mktyp $startpos $endpos (Ptyp_any)
    }

simple_core_type2:
| QUOTE ident
    { mktyp $startpos $endpos (Ptyp_var $2) }
| UNDERSCORE
    { mktyp $startpos $endpos (Ptyp_any) }
| type_longident
    { mktyp $startpos $endpos (Ptyp_constr(mkrhs $startpos($1) $endpos($1) $1, [])) }
| simple_core_type2 type_longident
    { mktyp $startpos $endpos (Ptyp_constr(mkrhs $startpos($2) $endpos($2) $2, [$1])) }
| LPAREN core_type_comma_list RPAREN type_longident
    { mktyp $startpos $endpos (Ptyp_constr(mkrhs $startpos($4) $endpos($4) $4, List.rev $2)) }
| LESS meth_list GREATER
    { let (f, c) = $2 in mktyp $startpos $endpos (Ptyp_object (f, c)) }
| LESS GREATER
    { mktyp $startpos $endpos (Ptyp_object ([], Closed)) }
| SHARP class_longident
    { mktyp $startpos $endpos (Ptyp_class(mkrhs $startpos($2) $endpos($2) $2, [])) }
| simple_core_type2 SHARP class_longident
    { mktyp $startpos $endpos (Ptyp_class(mkrhs $startpos($3) $endpos($3) $3, [$1])) }
| LPAREN core_type_comma_list RPAREN SHARP class_longident
    { mktyp $startpos $endpos (Ptyp_class(mkrhs $startpos($5) $endpos($5) $5, List.rev $2)) }
| LBRACKET tag_field RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant([$2], Closed, None)) }
(* PR#3835: this is not LR(1), would need lookahead=2
  | LBRACKET simple_core_type RBRACKET
      { mktyp $startpos $endpos (Ptyp_variant([$2], Closed, None)) }
*)
| LBRACKET BAR row_field_list RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant(List.rev $3, Closed, None)) }
| LBRACKET row_field BAR row_field_list RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant($2 :: List.rev $4, Closed, None)) }
| LBRACKETGREATER opt_bar row_field_list RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant(List.rev $3, Open, None)) }
| LBRACKETGREATER RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant([], Open, None)) }
| LBRACKETLESS opt_bar row_field_list RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant(List.rev $3, Closed, Some [])) }
| LBRACKETLESS opt_bar row_field_list GREATER name_tag_list RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant(List.rev $3, Closed, Some (List.rev $5))) }
| LPAREN MODULE package_type RPAREN
    { mktyp $startpos $endpos (Ptyp_package $3) }
| extension
    { mktyp $startpos $endpos  (Ptyp_extension $1) }

package_type:
| mty_longident
    { (mkrhs $startpos($1) $endpos($1) $1, []) }
| mty_longident WITH package_type_cstrs
    { (mkrhs $startpos($1) $endpos($1) $1, $3) }

package_type_cstr:
| TYPE label_longident EQUAL core_type
    { (mkrhs $startpos($2) $endpos($2) $2, $4) }

package_type_cstrs:
| package_type_cstr
    { [$1] }
| package_type_cstr AND package_type_cstrs
    { $1::$3 }

row_field_list:
| row_field
    { [$1] }
| row_field_list BAR row_field
    { $3 :: $1 }

row_field:
| tag_field
    { $1 }
| simple_core_type
    { Rinherit $1 }

tag_field:
| name_tag OF opt_ampersand amper_type_list attributes
    { Rtag ($1, $5, $3, List.rev $4) }
| name_tag attributes
    { Rtag ($1, $2, true, []) }

opt_ampersand:
| AMPERSAND
    { true }
| (* empty *)
    { false }

amper_type_list:
| core_type_no_attr
    { [$1] }
| amper_type_list AMPERSAND core_type_no_attr
    { $3 :: $1 }

name_tag_list:
| name_tag
    { [$1] }
| name_tag_list name_tag
    { $2 :: $1 }

simple_core_type_or_tuple:
| simple_core_type %prec below_LBRACKETAT
    { $1 }
| simple_core_type STAR core_type_list
    { mktyp $startpos $endpos (Ptyp_tuple($1 :: List.rev $3)) }

simple_core_type_or_tuple_no_attr:
| simple_core_type_no_attr
    { $1 }
| simple_core_type_no_attr STAR core_type_list_no_attr
    { mktyp $startpos $endpos (Ptyp_tuple($1 :: List.rev $3)) }

core_type_comma_list:
| core_type
    { [$1] }
| core_type_comma_list COMMA core_type
    { $3 :: $1 }

core_type_list:
| simple_core_type %prec below_LBRACKETAT
    { [$1] }
| core_type_list STAR simple_core_type
    { $3 :: $1 }

core_type_list_no_attr:
| simple_core_type_no_attr
    { [$1] }
| core_type_list STAR simple_core_type_no_attr
    { $3 :: $1 }

meth_list:
| field SEMI meth_list
    { let (f, c) = $3 in ($1 :: f, c) }
| field opt_semi
    { [$1], Closed }
| DOTDOT
    { [], Open }

field:
| label COLON poly_type_no_attr attributes
    { ($1, $4, $3) }

label:
| LIDENT
    { $1 }

(* Constants *)

constant:
| INT
    { Const_int $1 }
| CHAR
    { Const_char $1 }
| STRING
    { let (s, d) = $1 in Const_string (s, d) }
| FLOAT
    { Const_float $1 }
| INT32
    { Const_int32 $1 }
| INT64
    { Const_int64 $1 }
| NATIVEINT
    { Const_nativeint $1 }

signed_constant:
| constant
    { $1 }
| MINUS INT
    { Const_int(- $2) }
| MINUS FLOAT
    { Const_float("-" ^ $2) }
| MINUS INT32
    { Const_int32(Int32.neg $2) }
| MINUS INT64
    { Const_int64(Int64.neg $2) }
| MINUS NATIVEINT
    { Const_nativeint(Nativeint.neg $2) }
| PLUS INT
    { Const_int $2 }
| PLUS FLOAT
    { Const_float $2 }
| PLUS INT32
    { Const_int32 $2 }
| PLUS INT64
    { Const_int64 $2 }
| PLUS NATIVEINT
    { Const_nativeint $2 }

(* Identifiers and long identifiers *)

ident:
| UIDENT
    { $1 }
| LIDENT
    { $1 }

val_ident:
| LIDENT
    { $1 }
| LPAREN @{`Unclosed "("} operator RPAREN @{`Close}
    { $2 }
(*| LPAREN $2 =error
    { expecting $startpos($2) $endpos($2) "operator" }*)
(*| LPAREN MODULE error
    { expecting $startpos($3) $endpos($3) "module-expr" }*)

operator:
| PREFIXOP
    { $1 }
| INFIXOP0
    { $1 }
| INFIXOP1
    { $1 }
| INFIXOP2
    { $1 }
| INFIXOP3
    { $1 }
| INFIXOP4
    { $1 }
| SHARPOP
    { $1 }
| BANG
    { "!" }
| PLUS
    { "+" }
| PLUSDOT
    { "+." }
| MINUS
    { "-" }
| MINUSDOT
    { "-." }
| STAR
    { "*" }
| EQUAL
    { "=" }
| LESS
    { "<" }
| GREATER
    { ">" }
| OR
    { "or" }
| BARBAR
    { "||" }
| AMPERSAND
    { "&" }
| AMPERAMPER
    { "&&" }
| COLONEQUAL
    { ":=" }
| PLUSEQ
    { "+=" }
| PERCENT
    { "%" }
| index_operator
    { $1 }

index_operator:
| DOT index_operator_core opt_assign_arrow { $2^$3 }

index_operator_core:
| LPAREN RPAREN                         { ".()" }
| LBRACKET RBRACKET                     { ".[]" }
| LBRACE RBRACE                         { ".{}" }
| LBRACE COMMA RBRACE                   { ".{,}" }
| LBRACE COMMA COMMA RBRACE             { ".{,,}" }
| LBRACE COMMA DOTDOT COMMA RBRACE      { ".{,..,}" }

opt_assign_arrow:
| (* empty *) { "" }
| LESSMINUS   { "<-" }

constr_ident:
| UIDENT
    { $1 }
(*  | LBRACKET RBRACKET                           { "[]" } *)
| LPAREN RPAREN
    { "()" }
| COLONCOLON
    { "::" }
(*  | LPAREN COLONCOLON RPAREN                    { "::" } *)
| FALSE
    { "false" }
| TRUE
    { "true" }

val_longident:
| val_ident
    { Lident $1 }
| mod_longident DOT val_ident
    { Ldot($1, $3) }

constr_longident:
| mod_longident %prec below_DOT
    { $1 }
| LBRACKET RBRACKET
    { Lident "[]" }
| LPAREN RPAREN
    { Lident "()" }
| FALSE
    { Lident "false" }
| TRUE
    { Lident "true" }

label_longident:
| LIDENT
    { Lident $1 }
| mod_longident DOT LIDENT
    { Ldot($1, $3) }

type_longident:
| LIDENT
    { Lident $1 }
| mod_ext_longident DOT LIDENT
    { Ldot($1, $3) }

mod_longident:
| UIDENT
    { Lident $1 }
| mod_longident DOT UIDENT
    { Ldot($1, $3) }

mod_ext_longident:
| UIDENT
    { Lident $1 }
| mod_ext_longident DOT UIDENT
    { Ldot($1, $3) }
| mod_ext_longident LPAREN mod_ext_longident RPAREN
    { lapply $startpos $endpos $1 $3 }

mty_longident:
| ident
    { Lident $1 }
| mod_ext_longident DOT ident
    { Ldot($1, $3) }

clty_longident:
| LIDENT
    { Lident $1 }
| mod_ext_longident DOT LIDENT
    { Ldot($1, $3) }

class_longident:
| LIDENT
    { Lident $1 }
| mod_longident DOT LIDENT
    { Ldot($1, $3) }

(* Miscellaneous *)

name_tag:
| BACKQUOTE ident
    { $2 }

rec_flag:
| (* empty *)
    { Nonrecursive }
| REC
    { Recursive }

nonrec_flag:
| (* empty *)
    { Recursive }
| NONREC
    { Nonrecursive }

direction_flag:
| TO
    { Upto }
| DOWNTO
    { Downto }

private_flag:
| (* empty *)
    { Public }
| PRIVATE
    { Private }

mutable_flag:
| (* empty *)
    { Immutable }
| MUTABLE
    { Mutable }

virtual_flag:
| (* empty *)
    { Concrete }
| VIRTUAL
    { Virtual }

private_virtual_flags:
| (* empty *)
    { Public, Concrete }
| PRIVATE
    { Private, Concrete }
| VIRTUAL
    { Public, Virtual }
| PRIVATE VIRTUAL
    { Private, Virtual }
| VIRTUAL PRIVATE
    { Private, Virtual }

override_flag:
| (* empty *)
    { Fresh }
| BANG
    { Override }

opt_bar:
| (* empty *)
    { () }
| BAR
    { () }

opt_semi:
| (* empty *)
    { () }
| SEMI
    { () }

subtractive:
| MINUS
    { "-" }
| MINUSDOT
    { "-." }

additive:
| PLUS
    { "+" }
| PLUSDOT
    { "+." }


(* Attributes and extensions *)

single_attr_id:
| LIDENT
    { $1 }
| UIDENT
    { $1 }
| AND
    { "and" }
| AS
    { "as" }
| ASSERT
    { "assert" }
| BEGIN
    { "begin" }
| CLASS
    { "class" }
| CONSTRAINT
    { "constraint" }
| DO
    { "do" }
| DONE
    { "done" }
| DOWNTO
    { "downto" }
| ELSE
    { "else" }
| END
    { "end" }
| EXCEPTION
    { "exception" }
| EXTERNAL
    { "external" }
| FALSE
    { "false" }
| FOR
    { "for" }
| FUN
    { "fun" }
| FUNCTION
    { "function" }
| FUNCTOR
    { "functor" }
| IF
    { "if" }
| IN
    { "in" }
| INCLUDE
    { "include" }
| INHERIT
    { "inherit" }
| INITIALIZER
    { "initializer" }
| LAZY
    { "lazy" }
| LET
    { "let" }
| MATCH
    { "match" }
| METHOD
    { "method" }
| MODULE
    { "module" }
| MUTABLE
    { "mutable" }
| NEW
    { "new" }
| OBJECT
    { "object" }
| OF
    { "of" }
| OPEN
    { "open" }
| OR
    { "or" }
| PRIVATE
    { "private" }
| REC
    { "rec" }
| SIG
    { "sig" }
| STRUCT
    { "struct" }
| THEN
    { "then" }
| TO
    { "to" }
| TRUE
    { "true" }
| TRY
    { "try" }
| TYPE
    { "type" }
| VAL
    { "val" }
| VIRTUAL
    { "virtual" }
| WHEN
    { "when" }
| WHILE
    { "while" }
| WITH
    { "with" }
(* mod/land/lor/lxor/lsl/lsr/asr are not supported for now *)

attr_id:
| single_attr_id
    { mkloc $1 (rloc $startpos $endpos) }
| single_attr_id DOT attr_id
    { mkloc ($1 ^ "." ^ $3.txt) (rloc $startpos $endpos)}

attribute:
| LBRACKETAT attr_id payload RBRACKET
    { ($2, $3) }

post_item_attribute:
| LBRACKETATAT attr_id payload RBRACKET
    { ($2, $3) }

floating_attribute:
| LBRACKETATATAT @{`Item "attribute"}
  attr_id payload RBRACKET
    { ($2, $3) }

post_item_attributes:
| (* empty *)
    { [] }
| post_item_attribute post_item_attributes
    { $1 :: $2 }

attributes:
| (* empty *)
    { [] }
| attribute attributes
    { $1 :: $2 }

ext_attributes:
| (* empty *)
    { None, [] }
| attribute attributes
    { None, $1 :: $2 }
| PERCENT attr_id attributes
    { Some $2, $3 }

extension:
| LBRACKETPERCENT attr_id payload RBRACKET
    { ($2, $3) }

item_extension:
| LBRACKETPERCENTPERCENT @{`Item "extension"}
  attr_id payload RBRACKET
    { ($2, $3) }

payload:
| structure
    { PStr $1 }
| COLON core_type
    { PTyp $2 }
| QUESTION pattern
    { PPat ($2, None) }
| QUESTION pattern WHEN seq_expr
    { PPat ($2, Some $4) }

(* Merlin refactoring *)

newtype:
| LPAREN TYPE LIDENT RPAREN
    { $3 }

expr_open:
| override_flag ext_attributes mod_longident
    { $1, mkrhs $startpos($3) $endpos($3) $3, $2 }

(* Caml p4 extensions *)
structure_item:
| LET_LWT @{`Item "lwt"} ext_attributes rec_flag let_bindings
    { match $4 with
    | [ {pvb_pat = { ppat_desc = Ppat_any; ppat_loc = _ };
         pvb_expr = exp; pvb_attributes = attrs} ] ->
        let exp = wrap_exp_attrs $startpos $endpos exp $2 in
        mkstr $startpos $endpos (Pstr_eval (Fake.app Fake.Lwt.un_lwt exp, attrs))
    | _ ->
      let str = mkstr $startpos $endpos
            (Pstr_value ($3, List.rev_map (fake_vb_app Fake.Lwt.un_lwt) $4))
      in
      let (ext, attrs) = $2 in
      if attrs <> [] then not_expecting $startpos($2) $endpos($2) "attribute";
      match ext with
      | None -> str
      | Some id -> ghstr $startpos $endpos (Pstr_extension((id, PStr str), []))
    }
| TYPE @{`Item "type"} nonrec_flag type_declarations WITH with_extensions
    {
      let ghost_loc = Some (gloc $startpos($5) $endpos($5)) in
      let ty = List.map fake_tydecl $3 in
      let ast = Fake.TypeWith.generate_definitions ~ty ?ghost_loc $5 in
      mkstr $startpos $endpos (Pstr_type($2, List.rev $3)) @ ast
    }
| EXCEPTION @{`Item "exception"}
  str_exception_declaration WITH with_extensions
    { mkstr $startpos $endpos (Pstr_exception $2) }
| OUNIT_TEST option(STRING) EQUAL seq_expr
    { let expr = Fake.app Fake.OUnit.force_bool $4 in
      mkstr $startpos $endpos (Pstr_eval (expr,[]))
    }
| OUNIT_TEST_UNIT option(STRING) EQUAL seq_expr
    { let expr = Fake.app Fake.OUnit.force_unit $4 in
      mkstr $startpos $endpos (Pstr_eval (expr,[]))
    }
| OUNIT_TEST_MODULE option(STRING) EQUAL module_expr
    { let name = Fake.OUnit.fresh_test_module_ident () in
      mkstr $startpos $endpos
         (Pstr_module(Mb.mk (mkrhs $startpos($1) $endpos($2) name) $4))
    }
| OUNIT_BENCH STRING EQUAL seq_expr
    { let expr = $4 in
      mkstr $startpos $endpos (Pstr_eval (expr,[]))
    }
| OUNIT_BENCH_FUN STRING EQUAL seq_expr
    { let expr = Fake.app Fake.OUnit.force_unit_arrow_unit $4 in
      mkstr $startpos $endpos (Pstr_eval (expr,[]))
    }
| OUNIT_BENCH_INDEXED STRING val_ident simple_expr EQUAL seq_expr
    { let f_arg = mkpat $startpos $endpos
                      (Ppat_var (mkrhs $startpos($3) $endpos($3) $3))
      in
      let f_fun = mkexp $startpos $endpos
          (Pexp_fun(Nolabel, None, f_arg, $6))
      in
      let expr = Fake.(app (app OUnit.force_indexed f_fun) $4) in
      mkstr $startpos $endpos (Pstr_eval (expr,[]))
    }
| OUNIT_BENCH_MODULE STRING EQUAL module_expr
    { let name = Fake.OUnit.fresh_test_module_ident () in
      mkstr $startpos $endpos
         (Pstr_module(Mb.mk (mkrhs $startpos($1) $endpos($2) name) $4))
    }
;

signature_item:
| TYPE @{`Item "type"} nonrec_flag type_declarations WITH with_extensions
    {
      let ghost_loc = Some (gloc $startpos($5) $endpos($5)) in
      let ty = List.map fake_tydecl $3 in
      let decls = Fake.TypeWith.generate_sigs ~ty ?ghost_loc $5 in
      mksig $startpos $endpos (Psig_type($2, List.rev $3)) @ decls
    }
| EXCEPTION @{`Item "exception"}
  sig_exception_declaration WITH with_extensions
    { mksig $startpos $endpos (Psig_exception $2) }

with_extensions:
| LIDENT COMMA with_extensions { $1 :: $3 }
| LIDENT { [$1] }


expr:
| LET_LWT @{`Item "lwt"}
  ext_attributes rec_flag let_bindings IN @{`Shift 2} seq_expr
    { let expr = reloc_exp_fake $endpos($5) $endpos $6 in
      let expr = Pexp_let($3, List.rev_map (fake_vb_app Fake.Lwt.un_lwt) $4, expr) in
      Fake.app Fake.Lwt.in_lwt (mkexp_attrs $startpos $endpos expr $2) }
| MATCH_LWT @{`Item "match_lwt"}
  ext_attributes seq_expr WITH opt_bar match_cases
    { let expr = mkexp_attrs $startpos $endpos
          (Pexp_match(Fake.app Fake.Lwt.un_lwt $3, List.rev $6)) $2 in
      Fake.app Fake.Lwt.in_lwt expr }
| TRY_LWT @{`Item "try_lwt"}
  ext_attributes seq_expr %prec below_WITH
    { reloc_exp $startpos $endpos (Fake.app Fake.Lwt.in_lwt $3) }
| TRY_LWT @{`Item "try_lwt"}
  ext_attributes seq_expr WITH opt_bar match_cases
    { mkexp_attrs $startpos $endpos
        (Pexp_try(Fake.app Fake.Lwt.in_lwt $3, List.rev $6)) $2 }
| TRY_LWT @{`Item "try_lwt"}
  ext_attributes seq_expr FINALLY_LWT seq_expr
    { Fake.app (Fake.app Fake.Lwt.finally' $3) $5 }
| TRY_LWT @{`Item "try_lwt"}
  ext_attributes seq_expr WITH opt_bar match_cases FINALLY_LWT seq_expr
    { let expr = mkexp_attrs $startpos $endpos
        (Pexp_try (Fake.app Fake.Lwt.in_lwt $3, List.rev $6)) $2 in
      Fake.app (Fake.app Fake.Lwt.finally' expr) $8 }
| WHILE_LWT @{`Item "while_lwt"}
  ext_attributes seq_expr
  DO @{`Item "while_lwt body"} seq_expr DONE
  { let expr = Pexp_while ($3, Fake.(app Lwt.un_lwt $5)) in
    Fake.(app Lwt.to_lwt (mkexp_attrs $startpos $endpos expr $2)) }
| FOR_LWT @{`Item "for_lwt"}
  ext_attributes pattern EQUAL seq_expr direction_flag seq_expr
  DO @{`Item "for body"} seq_expr DONE
    { let expr = Pexp_for ($3, $5, $7, $6, Fake.(app Lwt.un_lwt $9)) in
      Fake.(app Lwt.to_lwt (mkexp_attrs $startpos $endpos expr $2)) }
| FOR_LWT @{`Item "for_lwt"}
  ext_attributes pattern IN seq_expr
  DO @{`Item "for body"} seq_expr DONE
    { mkexp_attrs $startpos $endpos
          (Pexp_let (Nonrecursive, [Vb.mk $3 (Fake.(app Lwt.un_stream $5))],
             Fake.(app Lwt.unit_lwt $7)))
          $2
    }
;

expr:
| simple_expr SHARPSHARP label LESSMINUS expr
    { let inst = Fake.(app Js.un_js $1) in
      let field = mkexp $startpos $endpos($3) (Pexp_send(inst, $3)) in
      let prop = Fake.(app Js.un_prop field) in
      let setter = mkexp $startpos $endpos($3) (Pexp_send(prop,"set")) in
      reloc_exp $startpos $endpos
      Fake.(app setter $5)
    }
;

simple_expr:
| simple_expr SHARPSHARP @{`Shift_token (1,LIDENT "")} label
    { let inst = Fake.(app Js.un_js $1) in
      let field = mkexp $startpos $endpos (Pexp_send(inst, $3)) in
      let prop = Fake.(app Js.un_prop field) in
      mkexp $startpos $endpos (Pexp_send(prop,"get"))
    }
| simple_expr SHARPSHARP label LPAREN RPAREN
    { let inst = Fake.(app Js.un_js $1) in
      let jsmeth = mkexp $startpos $endpos($3) (Pexp_send(inst, $3)) in
      Fake.(app Js.un_meth jsmeth)
    }
| simple_expr SHARPSHARP label LPAREN expr_comma_opt_list RPAREN
    { let inst = Fake.(app Js.un_js $1) in
      let meth = mkexp $startpos $endpos($3) (Pexp_send(inst, $3)) in
      let jsmeth =
        List.fold_left
          (fun meth arg ->
            reloc_exp meth.pexp_loc.Location.loc_start
                      arg.pexp_loc.Location.loc_end
            (Fake.app meth arg))
          meth (List.rev $5)
      in
      Fake.(app Js.un_meth jsmeth)
    }

simple_expr:
| P4_QUOTATION
    { reloc_exp $startpos $endpos Fake.any_val' }
(* Js_of_ocaml extension *)
| JSNEW simple_expr LPAREN RPAREN
    { reloc_exp $startpos $endpos
      Fake.(app Js.un_constr $2)
    }
| JSNEW simple_expr LPAREN expr_comma_opt_list RPAREN
    { let jsnew' = reloc_exp $startpos($1) $endpos($1) Fake.Js.un_constr in
      let constr = reloc_exp $startpos($1) $endpos($2) Fake.(app jsnew' $2) in
      reloc_exp $startpos $endpos
      (List.fold_left
         (fun constr arg ->
           reloc_exp constr.pexp_loc.Location.loc_start
                     arg.pexp_loc.Location.loc_end
           (Fake.app constr arg))
         constr (List.rev $4))
    }

expr_comma_opt_list:
    expr_comma_opt_list COMMA expr              { $3 :: $1 }
  | expr %prec COMMA                            { [$1] }
;

(* Custom-printf extension *)
simple_expr:
| CUSTOM_BANG simple_expr
    { match Fake.Custom_printf.bang $startpos $endpos $2 with
      | None -> mkexp $startpos $endpos (Pexp_apply(mkoperator $startpos($1) $endpos($1) "!", [Nolabel,$2]))
      | Some expr -> expr }

operator:
| CUSTOM_BANG
    { "!" }

override_flag:
| CUSTOM_BANG
    { Override }

(* Toplevel directives *)
toplevel_directives:
    (* empty *) { () }
  | toplevel_directives SHARP ident { () }
  | toplevel_directives SHARP ident STRING { () }
  | toplevel_directives SHARP ident INT { () }
  | toplevel_directives SHARP ident val_longident { () }
  | toplevel_directives SHARP ident FALSE { () }
  | toplevel_directives SHARP ident TRUE { () }
;

%%
