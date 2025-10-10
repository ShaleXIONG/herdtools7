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

module type S = sig
  type t

  val has_explicit_handler : t ->  bool
  val dump_fun :
    out_channel ->
    Template.extra_args ->
    (string * CType.t) list ->
    string list ->
    Proc.t ->
    t ->
    unit

  val dump_call :
    string ->
    string list ->
    (CType.t -> string -> string) ->
    out_channel ->
    string ->
    ((string * CType.t) list * (string * CType.t) list) ->
    string list ->
    Proc.t ->
    t ->
    unit

(* Inline dump *)
  val dump :
    out_channel ->
    string ->
    ((string * CType.t) list * (string * CType.t) list) ->
    string list ->
    Proc.t ->
    t ->
    unit
end
