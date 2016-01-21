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
open Misc

let extract_const_string = function
  | Asttypes.Const_string (str, _) -> str
  | _ -> assert false

let arg_label_to_str s = s

let sig_item_idns =
  let open Types in function
  | Sig_value (id, _) -> id, `Vals
  | Sig_type (id, _, _) -> id, `Type
  | Sig_typext (id, _, _) -> id, `Type
  | Sig_module (id, _, _) -> id, `Mod
  | Sig_modtype (id, _) -> id, `Modtype
  | Sig_class (id, _, _) -> id, `Vals (* that's just silly *)
  | Sig_class_type (id, _, _) -> id, `Type (* :_D *)

let include_idents l = List.map sig_item_idns l

let lookup_constructor = Env.lookup_constructor
let lookup_label       = Env.lookup_label

let fold_types f id env acc =
  Env.fold_types (fun s p (decl,descr) acc -> f s p decl acc) id env acc

let fold_constructors f id env acc =
  Env.fold_constructors
    (fun constr acc -> f constr.Types.cstr_name constr acc)
    id env acc
let fold_labels = Env.fold_labels

let exp_open_env = function
  | Typedtree.Texp_open (_,_,_,env) -> env
  | _ -> assert false

let extract_functor_arg m = m

let extract_modtype_declaration m = m.Types.mtd_type
let extract_module_declaration m = m.Types.md_type, m.Types.md_attributes

let lookup_module name env =
  let path = Env.lookup_module ~load:true name env in
  let md = Env.find_module path env in
  path, extract_module_declaration md

let lookup_modtype name env =
  let path, mdtype = Env.lookup_modtype name env in
  path, mdtype.Types.mtd_type

let summary_prev =
  let open Env in
  function
  | Env_empty -> None
  | Env_open (s,_) | Env_value (s,_,_)
  | Env_type (s,_,_) | Env_extension (s,_,_)
  | Env_module (s,_,_) | Env_modtype (s,_,_)
  | Env_class (s,_,_) | Env_cltype (s,_,_)
  | Env_functor_arg (s,_) | Env_aliasmap (s,_) ->
    Some s

let summary_open_path = function
  | Env.Env_open (_, path) -> Some path
  | _ -> None

let signature_of_summary =
  let open Env in
  let open Types in
  function
  | Env_value (_,i,v)      -> Some (Sig_value (i,v))
  (* Trec_not == bluff, FIXME *)
  | Env_type (_,i,t)       -> Some (Sig_type (i,t,Trec_not))
  (* Texp_first == bluff, FIXME *)
  | Env_extension (_,i,e)  ->
    begin match e.ext_type_path with
    | Path.Pident id when Ident.name id = "exn" ->
      Some (Sig_typext (i,e, Text_exception))
    | _ ->
      Some (Sig_typext (i,e, Text_first))
    end
  | Env_module (_,i,m)     -> Some (Sig_module (i,m,Trec_not))
  | Env_modtype (_,i,m)    -> Some (Sig_modtype (i,m))
  | Env_class (_,i,c)      -> Some (Sig_class (i,c,Trec_not))
  | Env_cltype (_,i,c)     -> Some (Sig_class_type (i,c,Trec_not))
  | Env_open _ | Env_empty | Env_functor_arg _ -> None
  | Env_aliasmap _ -> None

let rec last_ident =
  let open Env in
  function
  | Env_value (_,id,_)
  | Env_type (_,id,_)
  | Env_extension  (_,id,_)
  | Env_module (_,id,_)
  | Env_modtype(_,id,_)
  | Env_class (_,id,_)
  | Env_cltype (_,id,_)
  | Env_functor_arg (_,id) -> id
  | Env_empty -> raise Not_found
  | Env_open (s,_) | Env_aliasmap (s,_) -> last_ident s

let id_of_constr_decl c = c.Types.cd_id

let add_hidden_signature env sign =
  let add_item env comp =
    match comp with
    | Types.Sig_value(id, decl)     -> Env.add_value (Ident.hide id) decl env
    | Types.Sig_type(id, decl, _)   -> Env.add_type ~check:false (Ident.hide id) decl env
    | Types.Sig_typext(id, decl, _) -> Env.add_extension ~check:false (Ident.hide id) decl env
    | Types.Sig_module(id, mty, _)  -> Env.add_module (Ident.hide id) mty.Types.md_type env
    | Types.Sig_modtype(id, decl)   -> Env.add_modtype (Ident.hide id) decl env
    | Types.Sig_class(id, decl, _)  -> Env.add_class (Ident.hide id) decl env
    | Types.Sig_class_type(id, decl, _) -> Env.add_cltype (Ident.hide id) decl env
  in
  List.fold_left ~f:add_item ~init:env sign

let signature_ident =
  let open Types in function
  | Sig_value (i,_)
  | Sig_type (i,_,_)
  | Sig_typext (i,_,_)
  | Sig_modtype (i,_)
  | Sig_module (i,_,_)
  | Sig_class (i,_,_)
  | Sig_class_type (i,_,_) -> i

let union_loc_opt a b = match a,b with
  | None, None -> None
  | (Some _ as l), None | None, (Some _ as l) -> l
  | Some a, Some b -> Some (Parsing_aux.location_union a b)

let rec signature_loc =
  let open Types in
  let rec mod_loc = function
    | Mty_ident _ -> None
    | Mty_functor (_,m1,m2) ->
      begin match extract_functor_arg m1 with
      | Some m1 -> union_loc_opt (mod_loc m1) (mod_loc m2)
      | None -> mod_loc m2
      end
    | Mty_signature (lazy s) ->
        let rec find_first = function
          | x :: xs -> (match signature_loc x with
                        | (Some _ as v) -> v
                        | None -> find_first xs)
          | [] -> None
        in
        let a = find_first s and b = find_first (List.rev s) in
        union_loc_opt a b
    | _ -> None
  in
  function
  | Sig_value (_,v)    -> Some v.val_loc
  | Sig_type (_,t,_)   -> Some t.type_loc
  | Sig_typext (_,e,_) -> Some e.ext_loc
  | Sig_module (_,m,_) -> mod_loc (fst (extract_module_declaration m))
  | Sig_modtype (_,m) ->
    begin match extract_modtype_declaration m with
    | Some m -> mod_loc m
    | None -> None
    end
  | Sig_class (_,_,_)
  | Sig_class_type (_,_,_) -> None

let rec pattern_idlocs pat =
  let open Typedtree in
  match pat.pat_desc with
  | Tpat_var (id, _) -> [ Ident.name id , pat.pat_loc ]
  | Tpat_tuple patts
  | Tpat_array patts
  | Tpat_construct (_, _, patts) ->
    List.concat_map patts ~f:pattern_idlocs
  | Tpat_record (lst, _) ->
    List.map lst ~f:(fun (lid_loc, _, _pattern) ->
      Longident.last lid_loc.Asttypes.txt, lid_loc.Asttypes.loc
    ) (* TODO: handle rhs, i.e. [_pattern] *)
  | Tpat_variant (_, Some pat, _) -> pattern_idlocs pat
  | _ -> []

let identify_str_includes item =
  match item.Typedtree.str_desc with
  | Typedtree.Tstr_include { Typedtree. incl_type ; incl_mod } ->
    `Included (include_idents incl_type, `Mod_expr incl_mod)
  | _ -> `Not_included

let identify_sig_includes item =
  match item.Typedtree.sig_desc with
  | Typedtree.Tsig_include { Typedtree. incl_type ; incl_mod } ->
    `Included (include_idents incl_type, `Mod_type incl_mod)
  | _ -> `Not_included

let remove_indir_me me =
  match me.Typedtree.mod_desc with
  | Typedtree.Tmod_ident (path, _) -> `Alias path
  | Typedtree.Tmod_structure str -> `Str str
  | Typedtree.Tmod_functor (_param_id, param_name, _param_sig, me) ->
    `Functor (param_name, me.Typedtree.mod_loc, `Mod_expr me)
  | Typedtree.Tmod_apply (me1, me2, _) -> `Apply (me1, me2)
  | Typedtree.Tmod_constraint (me, _, _, _) -> `Mod_expr me
  | Typedtree.Tmod_unpack _ -> `Unpack

let remove_indir_mty mty =
  match mty.Typedtree.mty_desc with
  | Typedtree.Tmty_alias (path, _)
  | Typedtree.Tmty_ident (path, _) -> `Alias path
  | Typedtree.Tmty_signature sg -> `Sg sg
  | Typedtree.Tmty_functor (_param_id, param_name, _param_sig, mty) ->
    `Functor (param_name, mty.Typedtree.mty_loc, `Mod_type mty)
  | Typedtree.Tmty_with (mty, _) -> `Mod_type mty
  | Typedtree.Tmty_typeof me -> `Mod_expr me

let path_and_loc_of_cstr desc env =
  let open Types in
  match desc.cstr_tag with
  | Cstr_extension (path, loc) -> path, desc.cstr_loc
  | _ ->
    match desc.cstr_res.desc with
    | Tconstr (path, _, _) -> path, desc.cstr_loc
    | _ -> assert false

(* TODO: remove *)
let dest_tstr_eval str =
  let open Typedtree in
  match str.str_items with
  | [ { str_desc = Tstr_eval (exp,_) }] -> exp
  | _ -> failwith "unhandled expression"

let full_scrape = Env.scrape_alias

let rec subst_patt initial ~by patt =
  let f = subst_patt initial ~by in
  let open Typedtree in
  if patt == initial then by else
  match patt.pat_desc with
  | Tpat_any
  | Tpat_var _
  | Tpat_constant _ -> patt
  | Tpat_alias (p,x,y) ->
    { patt with pat_desc = Tpat_alias (f p, x, y) }
  | Tpat_tuple lst ->
    { patt with pat_desc = Tpat_tuple (List.map lst ~f)}
  | Tpat_construct (lid, cd, lst) ->
    { patt with pat_desc = Tpat_construct (lid, cd, List.map lst ~f) }
  | Tpat_variant (lbl, pat_opt, row_desc) ->
    { patt with pat_desc = Tpat_variant (lbl, Option.map pat_opt ~f, row_desc) }
  | Tpat_record (sub, flg) ->
    let sub' =
      List.map sub ~f:(fun (lid, lbl_descr, patt) -> lid, lbl_descr, f patt)
    in
    { patt with pat_desc = Tpat_record (sub', flg) }
  | Tpat_array lst ->
    { patt with pat_desc = Tpat_array (List.map lst ~f)}
  | Tpat_or (p1, p2, row) ->
    { patt with pat_desc = Tpat_or (f p1, f p2, row) }
  | Tpat_lazy p ->
    { patt with pat_desc = Tpat_lazy (f p) }

let rec rm_sub patt sub =
  let f p = rm_sub p sub in
  let open Typedtree in
  match patt.pat_desc with
  | Tpat_any
  | Tpat_var _
  | Tpat_constant _ -> patt
  | Tpat_alias (p,x,y) ->
    { patt with pat_desc = Tpat_alias (f p, x, y) }
  | Tpat_tuple lst ->
    { patt with pat_desc = Tpat_tuple (List.map lst ~f)}
  | Tpat_construct (lid, cd, lst) ->
    { patt with pat_desc = Tpat_construct (lid, cd, List.map lst ~f) }
  | Tpat_variant (lbl, pat_opt, row_desc) ->
    { patt with pat_desc = Tpat_variant (lbl, Option.map pat_opt ~f, row_desc) }
  | Tpat_record (sub, flg) ->
    let sub' =
      List.map sub ~f:(fun (lid, lbl_descr, patt) -> lid, lbl_descr, f patt)
    in
    { patt with pat_desc = Tpat_record (sub', flg) }
  | Tpat_array lst ->
    { patt with pat_desc = Tpat_array (List.map lst ~f)}
  | Tpat_or (p1, p2, row) ->
    if p1 == sub then p2 else if p2 == sub then p1 else
    { patt with pat_desc = Tpat_or (f p1, f p2, row) }
  | Tpat_lazy p ->
    { patt with pat_desc = Tpat_lazy (f p) }

let rec is_sub_patt patt ~sub =
  let open Typedtree in
  if patt == sub then true else
  match patt.pat_desc with
  | Tpat_any
  | Tpat_var _
  | Tpat_constant _
  | Tpat_variant (_, None, _) -> false

  | Tpat_alias (p,_,_)
  | Tpat_variant (_, Some p, _)
  | Tpat_lazy p ->
    is_sub_patt p ~sub

  | Tpat_tuple lst
  | Tpat_construct (_, _, lst)
  | Tpat_array lst ->
    List.exists lst ~f:(is_sub_patt ~sub)

  | Tpat_record (subs, flg) ->
    List.exists subs ~f:(fun (_, _, p) -> is_sub_patt p ~sub)

  | Tpat_or (p1, p2, row) ->
    is_sub_patt p1 ~sub || is_sub_patt p2 ~sub

let rec qualify_constructors f pat =
  let open Typedtree in
  let pat_desc =
    match pat.pat_desc with
    | Tpat_alias (p, id, loc) -> Tpat_alias (qualify_constructors f p, id, loc)
    | Tpat_tuple ps -> Tpat_tuple (List.map ps ~f:(qualify_constructors f))
    | Tpat_record (labels, closed) ->
      let labels =
        List.map labels
          ~f:(fun (lid, descr, pat) -> lid, descr, qualify_constructors f pat)
      in
      Tpat_record (labels, closed)
    | Tpat_construct (lid, cstr_desc, ps) ->
      let lid =
        match lid.Asttypes.txt with
        | Longident.Lident name ->
          begin match (Btype.repr pat.pat_type).Types.desc with
          | Types.Tconstr (path, _, _) ->
            let path = f pat.pat_env path in
            begin match Path.to_string_list path with
            | [] -> assert false
            | p :: ps ->
              let open Longident in
              match
                List.fold_left ps ~init:(Lident p)
                  ~f:(fun lid p -> Ldot (lid, p))
              with
              | Lident _ -> { lid with Asttypes.txt = Lident name }
              | Ldot (path, _) -> { lid with Asttypes.txt = Ldot (path, name) }
              | _ -> assert false
            end
          | _ -> lid
          end
        | _ -> lid (* already qualified *)
      in
      Tpat_construct (lid, cstr_desc, List.map ps ~f:(qualify_constructors f))
    | Tpat_array ps -> Tpat_array (List.map ps ~f:(qualify_constructors f))
    | Tpat_or (p1, p2, row_desc) ->
      Tpat_or (qualify_constructors f p1, qualify_constructors f p2, row_desc)
    | Tpat_lazy p -> Tpat_lazy (qualify_constructors f p)
    | desc -> desc
  in
  { pat with pat_desc = pat_desc }


let find_branch patterns sub =
  let rec aux before = function
    | [] -> raise Not_found
    | p :: after when is_sub_patt p ~sub -> before, after, p
    | p :: ps -> aux (p :: before) ps
  in
  aux [] patterns

let optional_label_sugar = function
  | Typedtree.Texp_construct (id, _, [e])
    when id.Location.loc.Location.loc_ghost
         && id.Location.txt = Longident.Lident "Some" ->
    Some e
  | _ -> None

let cstr_attributes c = c.Types.cstr_attributes
let val_attributes v = v.Types.val_attributes
let type_attributes t = t.Types.type_attributes
let lbl_attributes l = l.Types.lbl_attributes
let mtd_attributes t = t.Types.mtd_attributes

(* Taken from Leo White's doc-ock,
   https://github.com/lpw25/doc-ock/blob/master/src/docOckAttrs.ml
 *)
let read_doc_attributes attrs =
  let read_payload =
    let open Location in let open Parsetree in
    function
    | PStr[{ pstr_desc =
               Pstr_eval({ pexp_desc =
                             Pexp_constant(Asttypes.Const_string(str, _));
                           pexp_loc = loc;
                         }, _)
           }] -> Some(str, loc)
    | _ -> None
  in
  let rec loop = function
    | ({Location.txt =
          ("doc" | "ocaml.doc"); loc}, payload) :: rest ->
      read_payload payload
    | _ :: rest -> loop rest
    | [] -> None
  in
  loop attrs

module Parsetree = struct
  open Parsetree

  let arg_label_of_str x = x

  let format_params ~f params =
    let format_param (param,_variance) =
      match param.ptyp_desc with
      | Ptyp_any -> f "_"
      | Ptyp_var v -> f v
      | _ -> assert false (*TODO*)
    in
    List.map format_param params

  let extract_specific_parsing_info = function
    | { pexp_desc = Pexp_ident longident } -> `Ident longident
    | { pexp_desc = Pexp_construct (longident, _) } -> `Constr longident
    | _ -> `Other

  let map_constructors ~f lst =
    List.map lst ~f:(fun { pcd_name ; pcd_args ; pcd_res ; pcd_loc ; _ } ->
      f pcd_name.Location.txt pcd_args pcd_res pcd_loc
    )

  let args_of_constructor c = c.pcd_args

  let inspect_label { pld_name ; pld_mutable ; pld_type ; pld_loc ; _ } =
    pld_name, pld_mutable, pld_type, pld_loc
end

let get_class_field_desc_infos = function
  | Typedtree.Tcf_val (str_loc,_,_,_,_) -> Some (str_loc, `Value)
  | Typedtree.Tcf_method (str_loc,_,_)  -> Some (str_loc, `Method)
  | _ -> None

let no_label = ""
