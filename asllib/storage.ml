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

open ASTUtils

let _runtime_assertions = true

type pointer = int

module PMap = struct
  module PM = Map.Make (Int)

  let[@warning "-32"] of_list li =
    List.fold_left (fun acc (key, value) -> PM.add key value acc) PM.empty li

  include PM
end

module PSet = Set.Make (Int)

type 'v t = { env : pointer IMap.t; mem : 'v PMap.t }

let alloc =
  let next = ref 0 in
  fun () ->
    let r = !next in
    next := r + 1;
    r

let empty = { env = IMap.empty; mem = PMap.empty }
let mem x t = IMap.mem x t.env

let assign x v t =
  let p = IMap.find x t.env in
  { t with mem = PMap.add p v t.mem }

let declare x v t =
  let () =
    if _runtime_assertions && mem x t then
      let () =
        Printf.eprintf "Storage element %s already declared in env.\n%!" x
      in
      assert false
  in
  let p = alloc () in
  { env = IMap.add x p t.env; mem = PMap.add p v t.mem }

let of_v_map map =
  let mem_list = ref [] in
  let env =
    IMap.map
      (fun v ->
        let p = alloc () in
        mem_list := (p, v) :: !mem_list;
        p)
      map
  in
  let mem = PMap.of_list !mem_list in
  { env; mem }

let add x v t = try assign x v t with Not_found -> declare x v t

let find x t =
  let p = IMap.find x t.env in
  PMap.find p t.mem

let find_opt x t = try Some (find x t) with Not_found -> None

let remove x t =
  try
    let p = IMap.find x t.env in
    { mem = PMap.remove p t.mem; env = IMap.remove x t.env }
  with Not_found -> t

let patch_mem ~t_env ~t_mem to_avoid =
  let env = t_env.env
  and mem =
    try
      List.fold_left
        (fun mem x ->
          let p = IMap.find x t_mem.env in
          PMap.remove p mem)
        t_mem.mem to_avoid
    with Not_found ->
      let () =
        Printf.eprintf "Bug in unsetting one of ";
        List.iter (fun s -> Printf.eprintf "%s, " s) to_avoid;
        Printf.eprintf "\n%!"
      in
      assert false
  in
  { env; mem }

let to_seq t =
  IMap.to_seq t.env
  |> Seq.map (fun (name, pointer) -> (name, PMap.find pointer t.mem))

let pp_print pp_elt =
  let open Format in
  let pp_sep f () = fprintf f ",@ " in
  let pp_one f (key, elt) = fprintf f "@[<h 2>%s |-> @[%a@]@]" key pp_elt elt in
  fun f t ->
    fprintf f "@[<hv 2>{@ %a}@]" (PP.pp_print_seq ~pp_sep pp_one) (to_seq t)

let map f t = { t with mem = PMap.map f t.mem }
