(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2021-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module type S = sig
  type pte_atom
  (* TODO change the name *)
  type t
  (* TODO change the name *)
  val pp : t -> string
  (* TODO change the name *)
  val default : string -> t
  (* TODO change the name *)
  val compare : t -> t -> int
  (* TODO change the name *)
  val set_pteval : pte_atom -> t -> (unit -> string) -> t
  (* TODO change the name *)
  val can_fault : t -> bool

  type v = NoValue | Plain of int | PteValue of t
  val pp_v : ?hexa:bool -> v -> string
  val no_value : v
  val value_to_int : v -> int
  val value_of_int : int -> v
  val value_compare : v -> v -> int
end

module No(A:sig type arch_atom end) = struct
  type pte_atom = A.arch_atom
  type t = string
  let pp a = a
  let default s = s
  let compare _ _ = 0
  let set_pteval _ p _ = p
  let can_fault _t = false

  type v = NoValue | Plain of int | PteValue of t
  let value_to_int = function
      | NoValue -> -1
      | Plain v -> v
      (* TODO change *)
      | PteValue _ -> -1
  let no_value = NoValue
  let value_of_int v = Plain v
  let value_compare lhs rhs =
      match lhs, rhs with
      | NoValue, NoValue -> 0
      | NoValue, Plain _ -> -1
      | Plain _, NoValue -> 1
      | Plain lhs, Plain rhs -> Misc.int_compare lhs rhs
      (* TODO change *)
      | _ -> Warn.user_error "ERROR"

  let pp_v ?(hexa=false) = function
    | NoValue -> "**"
    | Plain v -> Printf.sprintf (if hexa then "0x%x" else "%d") v
    | PteValue p -> pp p
end


