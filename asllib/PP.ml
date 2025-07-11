(******************************************************************************)
(*                                ASLRef                                      *)
(******************************************************************************)
(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

open Format
open AST

type 'a printer = Format.formatter -> 'a -> unit

(* Adapted from stdlib >= 4.12.0 *)
let pp_print_seq ?(pp_sep = pp_print_cut) pp_v ppf v =
  let is_first = ref true in
  let pp_v v =
    if !is_first then is_first := false else pp_sep ppf ();
    pp_v ppf v
  in
  Seq.iter pp_v v

(* Just to display better error messages ... *)
let nonstandard_constraint_compare c1 c2 =
  match (c1, c2) with
  | Constraint_Exact _, Constraint_Exact _
  | Constraint_Range _, Constraint_Range _ ->
      compare c1 c2
  | Constraint_Exact e1, Constraint_Range (e3, e4) -> (
      match compare e1 e3 with
      | 0 -> ( match compare e1 e4 with 0 -> 1 | n -> n)
      | n -> n)
  | Constraint_Range (e1, e2), Constraint_Exact e3 -> (
      match compare e1 e3 with
      | 0 -> ( match compare e2 e3 with 0 -> -1 | n -> n)
      | n -> n)

let pp_comma f () = fprintf f ",@ "
let pp_comma_list pp_elt f = pp_print_list ~pp_sep:pp_comma pp_elt f

let pp_pos f { pos_start; pos_end; _ } =
  let open Lexing in
  let pp_char_num f { pos_cnum; pos_bol; _ } =
    pp_print_int f (pos_cnum - pos_bol)
  in
  if pos_start = dummy_pos || pos_end = dummy_pos then ()
  else (
    pp_open_hovbox f 2;
    fprintf f "File %s,@ " pos_start.pos_fname;
    if String.equal pos_start.pos_fname pos_end.pos_fname then
      if pos_start.pos_lnum = pos_end.pos_lnum then
        if pos_start.pos_cnum = pos_end.pos_cnum then
          fprintf f "line %d,@ character %a" pos_start.pos_lnum pp_char_num
            pos_start
        else
          fprintf f "line %d,@ characters %a to %a" pos_start.pos_lnum
            pp_char_num pos_start pp_char_num pos_end
      else
        fprintf f "line %d,@ character %a@ to@ line %d,@ character %a"
          pos_start.pos_lnum pp_char_num pos_start pos_end.pos_lnum pp_char_num
          pos_end
    else
      fprintf f "line %d,@ character %a" pos_start.pos_lnum pp_char_num
        pos_start;
    pp_close_box f ())

let pp_pos_str withpos =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  let () = pp_pos fmt withpos in
  let () = pp_print_flush fmt () in
  Buffer.contents buf

let pp_pos_str_no_char { pos_start; pos_end } =
  Printf.sprintf "file %S, line %d to line %d" pos_start.pos_fname
    pos_start.pos_lnum pos_end.pos_lnum

let binop_to_string : binop -> string = function
  | `AND -> "AND"
  | `BAND -> "&&"
  | `BEQ -> "<=>"
  | `BOR -> "||"
  | `DIV -> "DIV"
  | `DIVRM -> "DIVRM"
  | `XOR -> "XOR"
  | `EQ_OP -> "=="
  | `GT -> ">"
  | `GEQ -> ">="
  | `IMPL -> "==>"
  | `LT -> "<"
  | `LEQ -> "<="
  | `MOD -> "MOD"
  | `MINUS -> "-"
  | `MUL -> "*"
  | `NEQ -> "!="
  | `OR -> "OR"
  | `PLUS -> "+"
  | `RDIV -> "/"
  | `SHL -> "<<"
  | `SHR -> ">>"
  | `POW -> "^"
  | `CONCAT -> "::"

let unop_to_string = function BNOT -> "!" | NEG -> "-" | NOT -> "NOT"

let pp_literal f = function
  | L_Int i -> Z.pp_print f i
  | L_Bool true -> pp_print_string f "TRUE"
  | L_Bool false -> pp_print_string f "FALSE"
  | L_Real r ->
      fprintf f "(%a.0 / %a.0)" Z.pp_print (Q.num r) Z.pp_print (Q.den r)
  | L_BitVector bv -> Bitvector.pp_t f bv
  | L_String s -> fprintf f "%S" s
  | L_Label l -> fprintf f "%s" l

let rec pp_expr f e =
  match e.desc with
  | E_Literal v -> pp_literal f v
  | E_Var x -> pp_print_string f x
  | E_ATC (e, ty) -> fprintf f "@[%a@ as %a@]" pp_expr e pp_ty ty
  | E_Binop (b, e1, e2) ->
      fprintf f "(@[<hov 2>%a@ %s %a@])" pp_expr e1 (binop_to_string b) pp_expr
        e2
  | E_Unop (u, e) -> fprintf f "(%s %a)" (unop_to_string u) pp_expr e
  | E_Call { name; params = []; args } ->
      fprintf f "@[<hov 2>%s(%a)@]" name pp_expr_list args
  | E_Call { name; params; args } ->
      fprintf f "@[<hov 2>%s{%a}(%a)@]" name pp_expr_list params pp_expr_list
        args
  | E_Slice (e, args) ->
      fprintf f "@[<hov 2>%a[%a]@]" pp_expr e pp_slice_list args
  | E_GetArray (e1, e2) -> fprintf f "@[<hov 2>%a[[%a]]@]" pp_expr e1 pp_expr e2
  | E_GetEnumArray (e1, e2) ->
      fprintf f "@[<hov 2>%a[[%a]]@]" pp_expr e1 pp_expr e2
  | E_Cond (e1, e2, e3) ->
      fprintf f "@[<hv>@[<h>if %a@ then@]@;<1 2>%a@ else@;<1 2>%a@]" pp_expr e1
        pp_expr e2 pp_expr e3
  | E_GetField (e, x) -> fprintf f "@[%a@,.%s@]" pp_expr e x
  | E_GetFields (e, xs) ->
      fprintf f "@[%a@,.[@[%a@]]@]" pp_expr e (pp_comma_list pp_print_string) xs
  | E_GetCollectionFields (e, fields) ->
      fprintf f "@[%s@,.[@[%a@]]@]" e (pp_comma_list pp_print_string) fields
  | E_GetItem (e, i) -> fprintf f "@[%a@,.item%d@]" pp_expr e i
  | E_Record (ty, li) ->
      let pp_one f (x, e) = fprintf f "@[<h>%s =@ %a@]" x pp_expr e in
      fprintf f "@[<hv>%a {@ %a@;<1 -2>}@]" pp_ty ty (pp_comma_list pp_one) li
  | E_Tuple es -> fprintf f "@[<hv 2>(%a)@]" pp_expr_list es
  | E_Array { length; value } ->
      fprintf f "@[<hv 2>array[[%a]] of %a@]" pp_expr length pp_expr value
  | E_EnumArray { enum; value } ->
      fprintf f "@[<hv 2>array[[%s]] of %a@]" enum pp_expr value
  | E_Arbitrary ty -> fprintf f "@[<h>ARBITRARY :@ %a@]" pp_ty ty
  | E_Pattern (e, p) -> fprintf f "@[<hv 2>%a@ IN %a@]" pp_expr e pp_pattern p

and pp_expr_list f = pp_comma_list pp_expr f

and pp_slice f = function
  | Slice_Single e -> pp_expr f e
  | Slice_Range (e1, e2) -> fprintf f "@[<h>%a@,:%a@]" pp_expr e1 pp_expr e2
  | Slice_Length (e1, e2) -> fprintf f "@[<h>%a@,+:%a@]" pp_expr e1 pp_expr e2
  | Slice_Star (e1, e2) -> fprintf f "@[<h>%a@,*:%a@]" pp_expr e1 pp_expr e2

and pp_pattern f p =
  match p.desc with
  | Pattern_All -> pp_print_string f "{-}"
  | Pattern_Any li -> fprintf f "@[{%a}@]" (pp_comma_list pp_pattern) li
  | Pattern_Geq e -> fprintf f "@[>= %a@]" pp_expr e
  | Pattern_Leq e -> fprintf f "@[<= %a@]" pp_expr e
  | Pattern_Mask m -> fprintf f "'%s'" (Bitvector.mask_to_string m)
  | Pattern_Not { desc = Pattern_Any li; _ } ->
      fprintf f "@[!{%a}@]" (pp_comma_list pp_pattern) li
  | Pattern_Not p -> fprintf f "@[!{%a}@]" pp_pattern p
  | Pattern_Range (e1, e2) -> fprintf f "@[%a .. %a@]" pp_expr e1 pp_expr e2
  | Pattern_Single e -> pp_expr f e
  | Pattern_Tuple li -> fprintf f "@[(%a)@]" (pp_comma_list pp_pattern) li

and pp_slice_list f = pp_comma_list pp_slice f

and pp_ty f t =
  match t.desc with
  | T_Int UnConstrained -> pp_print_string f "integer"
  | T_Int (WellConstrained (cs, _)) ->
      fprintf f "@[integer {%a}@]" pp_int_constraints cs
  | T_Int PendingConstrained -> pp_print_string f "integer{-}"
  | T_Int (Parameterized (_uid, var)) -> fprintf f "@[integer {%s}@]" var
  | T_Real -> pp_print_string f "real"
  | T_String -> pp_print_string f "string"
  | T_Bool -> pp_print_string f "boolean"
  | T_Bits (width, []) -> fprintf f "@[bits(%a)@]" pp_expr width
  | T_Bits (width, fields) ->
      fprintf f "@[bits (%a)@ %a@]" pp_expr width pp_bitfields fields
  | T_Enum enum_ty ->
      fprintf f "@[<hov 2>enumeration {@,%a@;<0 -2>}@]"
        (pp_comma_list pp_print_string)
        enum_ty
  | T_Tuple ty_list -> fprintf f "@[(%a)@]" (pp_comma_list pp_ty) ty_list
  | T_Array (length, elt_type) ->
      fprintf f "@[array [[%a]] of %a@]" pp_array_index length pp_ty elt_type
  | T_Collection record_ty -> pp_record_like f "collection" record_ty
  | T_Record record_ty -> pp_record_like f "record" record_ty
  | T_Exception record_ty -> pp_record_like f "exception" record_ty
  | T_Named x -> pp_print_string f x

and pp_record_like f label record_ty =
  fprintf f "@[<hv 2>%s {@ %a@;<1 -2>}@]" label pp_fields record_ty

and pp_array_index f = function
  | ArrayLength_Expr e -> pp_expr f e
  | ArrayLength_Enum (enum, _) -> pp_print_string f enum

and pp_bitfield f = function
  | BitField_Simple (name, slices) ->
      fprintf f "@[<h>[%a]@ %s@]" pp_slice_list slices name
  | BitField_Nested (name, slices, bitfields) ->
      fprintf f "@[<h>[%a]@ %s@ %a@]" pp_slice_list slices name pp_bitfields
        bitfields
  | BitField_Type (name, slices, ty) ->
      fprintf f "@[<h>[%a]@ %s:@ %a@]" pp_slice_list slices name pp_ty ty

and pp_bitfields f bitfields =
  fprintf f "@[<hov 2>{@ %a@ }@]" (pp_comma_list pp_bitfield) bitfields

and pp_fields f =
  let pp_one f (field_name, field_type) =
    fprintf f "@[<h>%s:@ %a@]" field_name pp_ty field_type
  in
  pp_comma_list pp_one f

and pp_int_constraint f = function
  | Constraint_Exact x -> pp_expr f x
  | Constraint_Range (x, y) -> fprintf f "@[<h>%a..%a@]" pp_expr x pp_expr y

and pp_int_constraints f li =
  let li = List.sort nonstandard_constraint_compare li in
  let pp_max_int_constraint_list = 10 in
  if List.length li < pp_max_int_constraint_list then
    fprintf f "@[%a@]" (pp_comma_list pp_int_constraint) li
  else
    fprintf f "@[%a,@ ...@]"
      (pp_comma_list pp_int_constraint)
      (ASTUtils.list_take pp_max_int_constraint_list li)

let pp_typed_identifier f (name, ty) = fprintf f "@[%s:@ %a@]" name pp_ty ty

let rec pp_lexpr f le =
  match le.desc with
  | LE_Var x -> pp_print_string f x
  | LE_Slice (le, args) -> fprintf f "%a[%a]" pp_lexpr le pp_slice_list args
  | LE_SetArray (le, e) -> fprintf f "%a[[%a]]" pp_lexpr le pp_expr e
  | LE_SetEnumArray (le, e) -> fprintf f "%a[[%a]]" pp_lexpr le pp_expr e
  | LE_SetField (le, x) -> fprintf f "@[%a@,.%s@]" pp_lexpr le x
  | LE_SetCollectionFields (x, fields, _) ->
      fprintf f "@[%s@,.[@[%a@]]@]" x (pp_comma_list pp_print_string) fields
  | LE_SetFields (le, li, _) ->
      fprintf f "@[%a@,.@[[%a]@]@]" pp_lexpr le
        (pp_comma_list pp_print_string)
        li
  | LE_Discard -> pp_print_string f "-"
  | LE_Destructuring les -> fprintf f "@[( %a )@]" (pp_comma_list pp_lexpr) les

let pp_loop_limit =
  pp_print_option @@ fun f e -> fprintf f "@ @[<h 2>limit@ %a@]" pp_expr e

let pp_for_direction = function Up -> "to" | Down -> "downto"

let pp_local_decl_keyword f k =
  pp_print_string f
  @@
  match k with
  | LDK_Var -> "var"
  | LDK_Constant -> "constant"
  | LDK_Let -> "let"

let pp_local_decl_item f = function
  | LDI_Var x -> pp_print_string f x
  | LDI_Tuple ldis -> fprintf f "@[(%a)@]" (pp_comma_list pp_print_string) ldis

let rec pp_stmt f s =
  match s.desc with
  | S_Pass -> pp_print_string f "pass;"
  | S_Seq (s1, s2) -> fprintf f "%a@ %a" pp_stmt s1 pp_stmt s2
  | S_Assign (le, e) -> fprintf f "@[<h 2>%a =@ %a;@]" pp_lexpr le pp_expr e
  | S_Call { name; params = []; args } ->
      fprintf f "@[<hov 2>%s(%a);@]" name pp_expr_list args
  | S_Call { name; params; args } ->
      fprintf f "@[<hov 2>%s{%a}(%a);@]" name pp_expr_list params pp_expr_list
        args
  | S_Return (Some e) -> fprintf f "return %a;" pp_expr e
  | S_Return None -> fprintf f "return;"
  | S_Cond (e, s1, { desc = S_Pass; _ }) ->
      fprintf f "@[<hv>@[<h>if %a@ then@]@;<1 2>@[<hv>%a@]@ end;@]" pp_expr e
        pp_stmt s1
  | S_Cond (e, s1, s2) ->
      fprintf f
        "@[<hv>@[<h>if %a@ then@]@;\
         <1 2>@[<hv>%a@]@ else@;\
         <1 2>@[<hv>%a@]@ end;@]" pp_expr e pp_stmt s1 pp_stmt s2
  | S_Assert e -> fprintf f "@[<2>assert@ %a;@]" pp_expr e
  | S_While (e, limit, s) ->
      fprintf f "@[<hv>@[<h>while %a%a@ do@]@;<1 2>@[<hv>%a@]@ end;@]" pp_expr e
        pp_loop_limit limit pp_stmt s
  | S_Repeat (s, e, limit) ->
      fprintf f "@[<hv 2>repeat@;<1 2>@[<hv>%a@]@;<1 0>@[<h>until@ %a%a;@]@]"
        pp_stmt s pp_expr e pp_loop_limit limit
  | S_For { index_name; start_e; end_e; dir; body; limit } ->
      fprintf f "@[<hv>@[<h>for %a = %a %s %a%a@ do@]@;<1 2>@[<hv>%a@]@ end@]"
        pp_print_string index_name pp_expr start_e (pp_for_direction dir)
        pp_expr end_e pp_loop_limit limit pp_stmt body
  | S_Decl (ldk, ldi, None, None) ->
      fprintf f "@[<2>%a %a;@]" pp_local_decl_keyword ldk pp_local_decl_item ldi
  | S_Decl (ldk, ldi, None, Some e) ->
      fprintf f "@[<2>%a %a =@ %a;@]" pp_local_decl_keyword ldk
        pp_local_decl_item ldi pp_expr e
  | S_Decl (ldk, ldi, Some ty, None) ->
      fprintf f "@[<2>%a %a:@ %a;@]" pp_local_decl_keyword ldk
        pp_local_decl_item ldi pp_ty ty
  | S_Decl (ldk, ldi, Some ty, Some e) ->
      fprintf f "@[<2>%a %a:@ %a =@ %a;@]" pp_local_decl_keyword ldk
        pp_local_decl_item ldi pp_ty ty pp_expr e
  | S_Throw (Some (e, _ty_annotation)) ->
      fprintf f "@[<2>throw@ %a;@]" pp_expr e
  | S_Throw None -> fprintf f "throw;"
  | S_Try (s, catchers, Some s') ->
      fprintf f
        "@[<hv>@[try@ %a@]@ @[<v 2>catch@ %a@ @[<2>otherwise =>@ %a@]@]@ end@]"
        pp_stmt s
        (pp_print_list ~pp_sep:pp_print_space pp_catcher)
        catchers pp_stmt s'
  | S_Try (s, catchers, None) ->
      fprintf f "@[<2>try@ %a@ catch@ @[<v 2>%a@]@ end@]" pp_stmt s
        (pp_print_list ~pp_sep:pp_print_space pp_catcher)
        catchers
  | S_Print { args; newline = false; debug = false } ->
      fprintf f "@[<2>print(%a);@]" (pp_comma_list pp_expr) args
  | S_Print { args; newline = true; debug = false } ->
      fprintf f "@[<2>println(%a);@]" (pp_comma_list pp_expr) args
  | S_Print { args; debug = true } ->
      fprintf f "@[<2>DEBUG@ %a;@]" (pp_comma_list pp_expr) args
  | S_Unreachable -> fprintf f "Unreachable();"
  | S_Pragma (name, args) ->
      fprintf f "@[<2>pragma@ %a %a;@]" pp_print_string name
        (pp_comma_list pp_expr) args

and pp_catcher f (name, ty, s) =
  match name with
  | None -> fprintf f "@[<2>when@ %a@ => %a@]" pp_ty ty pp_stmt s
  | Some name ->
      fprintf f "@[<2>when %s@ :@ %a@ => %a@]" name pp_ty ty pp_stmt s

let pp_gdk f gdk =
  pp_print_string f
  @@
  match gdk with
  | GDK_Var -> "var"
  | GDK_Config -> "config"
  | GDK_Let -> "let"
  | GDK_Constant -> "constant"

let pp_decl f =
  let pp_global_storage f = function
    | { name; keyword; ty = None; initial_value = Some e } ->
        fprintf f "%a %s@ = %a" pp_gdk keyword name pp_expr e
    | { name; keyword; ty = Some t; initial_value = Some e } ->
        fprintf f "%a %s:@ %a@ = %a" pp_gdk keyword name pp_ty t pp_expr e
    | { name; keyword; ty = Some t; initial_value = None } ->
        fprintf f "%a %s:@ %a" pp_gdk keyword name pp_ty t
    | { name = _; keyword = _; ty = None; initial_value = None } -> assert false
  in
  let pp_func_sig f
      {
        name;
        args;
        return_type;
        parameters;
        subprogram_type;
        body = _;
        override;
      } =
    let pp_args = pp_comma_list pp_typed_identifier in
    let pp_return_type_opt f = function
      | Some return_type -> fprintf f "@;<1 -2>=> %a" pp_ty return_type
      | None -> ()
    in
    let pp_parameters f = function
      | [] -> ()
      | parameters ->
          let pp_one f = function
            | name, None -> pp_print_string f name
            | name, Some t -> pp_typed_identifier f (name, t)
          in
          fprintf f "@ {%a}" (pp_comma_list pp_one) parameters
    in
    let override_keyword =
      match override with
      | None -> ""
      | Some Impdef -> "impdef "
      | Some Implementation -> "implementation "
    in
    match subprogram_type with
    | ST_Function | ST_Procedure ->
        fprintf f "@[<hv 4>%sfunc @[%s%a@] (@,%a)%a@]" override_keyword name
          pp_parameters parameters pp_args args pp_return_type_opt return_type
    | ST_Getter ->
        fprintf f "@[<hv 4>%sgetter %s%a [@,%a]%a@]" override_keyword name
          pp_parameters parameters pp_args args pp_return_type_opt return_type
    | ST_EmptyGetter ->
        fprintf f "@[<hv 4>%sgetter %s%a@]" override_keyword name
          pp_return_type_opt return_type
    | ST_Setter ->
        let new_v, args =
          match args with [] -> assert false | h :: t -> (h, t)
        in
        fprintf f "@[<hv 4>%ssetter %s%a [@,%a]@ = %a@]" override_keyword name
          pp_parameters parameters pp_args args pp_typed_identifier new_v
    | ST_EmptySetter ->
        let new_v = match args with [ h ] -> h | _ -> assert false in
        fprintf f "@[<hv 4>%ssetter %s@ = %a]" override_keyword name
          pp_typed_identifier new_v
  in
  let pp_body f = function
    | SB_ASL s -> pp_stmt f s
    | SB_Primitive _ -> fprintf f "pass;@ // primitive"
  in
  fun d ->
    match d.desc with
    | D_Func func ->
        fprintf f "@[<v>%a@ begin@;<1 2>@[<v>%a@]@ end@]" pp_func_sig func
          pp_body func.body
    | D_TypeDecl (x, ty, None) -> fprintf f "@[<2>type %s of %a;@]" x pp_ty ty
    | D_TypeDecl (x, ty, Some (s, [])) ->
        fprintf f "@[<2>type %s@ of %a@ subtypes %s;@]" x pp_ty ty s
    | D_TypeDecl (x, ty, Some (s, fields)) ->
        fprintf f
          "@[<2>type %s@ of %a@ subtypes %s@ with @[<hv 2>{@ %a@;<1 -2>}@];@]" x
          pp_ty ty s pp_fields fields
    | D_GlobalStorage decl -> fprintf f "@[<2>%a;@]" pp_global_storage decl
    | D_Pragma (name, args) ->
        fprintf f "@[<2>pragma@ %a %a;@]" pp_print_string name
          (pp_comma_list pp_expr) args

let pp_t f ast =
  let pp_blank_line f () =
    pp_print_space f ();
    pp_print_cut f ()
  in
  fprintf f "@[<v>%a@]" (pp_print_list ~pp_sep:pp_blank_line pp_decl) ast

let ty_to_string = asprintf "%a" pp_ty
let t_to_string ast = asprintf "%a" pp_t ast
let literal_to_string = asprintf "%a" pp_literal

let pp_version f version =
  pp_print_string f
  @@ match version with `ASLv0 -> "ASLv0" | `ASLv1 -> "ASLv1" | `Any -> "any"
