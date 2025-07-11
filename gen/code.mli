(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Event components *)
type loc = Data of string | Code of Label.t
val as_data : loc -> string
val is_data : loc -> bool
val pp_loc : loc -> string
val loc_eq : loc -> loc -> bool
val loc_compare : loc -> loc -> int

module LocSet : MySet.S with type elt = loc
module LocMap : MyMap.S with type key = loc

val loc_none : loc
val ok_str : string
val ok : loc
val myok : int -> int -> loc
val myok_proc : int -> loc



type v = NoValue | Plain of int
val pp_v : ?hexa:bool -> v -> string
val no_value : v
val value_to_int : v -> int
val value_of_int : int -> v
val value_compare : v -> v -> int

type proc = Proc.t
val pp_proc : proc -> string

type env = (string * v) list

(* Direction of event *)
type dir = W | R

(* Edges compoments that do not depend on architecture *)

(* Change or proc accross edge *)
type ie = Int|Ext

(* Change of location across edge *)
type sd = Same|Diff

(* Direction of related events *)
type extr = Dir of dir | Irr | NoDir



(* Associated pretty print & generators *)
val pp_ie : ie -> string
val pp_dir : dir -> string
val pp_extr : extr -> string
val pp_sd : sd -> string
val seq_sd : sd -> sd -> sd
val fold_ie : (ie -> 'a -> 'a) -> 'a -> 'a
val fold_extr : (extr -> 'a -> 'a) -> 'a -> 'a
val fold_sd : (sd -> 'a -> 'a) -> 'a -> 'a
val fold_sd_extr : (sd -> extr -> 'a -> 'a) -> 'a -> 'a
val fold_sd_extr_extr : (sd -> extr -> extr -> 'a -> 'a) -> 'a -> 'a

type check =
  | Default | Sc | Uni | Thin | Critical | Free
  | Ppo | Transitive | Total | MixedCheck

val pp_check : check -> string
val checks : string list

(* Com *)
type com =  CRf | CFr | CWs

val pp_com : com -> string
val fold_com : (com -> 'a -> 'a) -> 'a -> 'a

(* Info in tests *)
type info = (string * string) list

(* Name of plain accesses *)
val plain : string

(* Memory bank (for MTE, KVM)  *)
type 'a bank = Ord | Tag | CapaTag | CapaSeal | Pte | VecReg of 'a | Pair | Instr

val pp_bank : 'a bank -> string

(* TODO consider change the type `v` *)
val add_tag : string -> int -> string

(* TODO consider change the type `v` *)
val add_capability : string -> int -> string

(* TODO consider change the type `v` *)
val add_vector : bool -> int list -> string
