(****************************************************************************)
(*                           The diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Code

module type Config = sig
  include CompileCommon.Config
  val moreedges : bool
  val realdep : bool
end

module Make(Cfg:Config) : XXXCompile_gen.S =
  struct

    let do_memtag = Cfg.variant Variant_gen.MemTag

(* Common *)
    let naturalsize = TypBase.get_size Cfg.typ
    module A64 =
      AArch64Arch_gen.Make
        (struct
          let naturalsize = naturalsize
          let moreedges = Cfg.moreedges
          let fullmixed = Cfg.variant Variant_gen.FullMixed
          let variant = Cfg.variant
        end)
    include CompileCommon.Make(Cfg)(A64)

    let ppo _f k = k

    open A64

(* Nop instr code *)
    let nop = "NOP"

(* Utilities *)
    (* Reserve SME's slice index register *)
    let x12 = Ireg R12

    let next_reg x =
      if do_sme then
        begin
          let r,x = A64.alloc_reg x in
          match r with
          | Ireg R12 -> A64.alloc_reg x
          | _ -> r,x
        end
      else
        A64.alloc_reg x

    let next_reg2 x =
      let r1,x = next_reg x in
      let r2,x = next_reg x in
      r1,r2,x

    let to_scalar r = match r with
        | Vreg (r,(_,8)) -> A64.SIMDreg r
        | Vreg (r,(_,16)) -> A64.SIMDreg r
        | Vreg (r,(_,32)) -> A64.SIMDreg r
        | Vreg (r,(_,64)) -> A64.SIMDreg r
        | _ -> assert false (* ? *)

    let to_scalable_vec r = match r with
        | Vreg (r,(_,s)) -> A64.Zreg (r,s)
        | _ -> assert false (* ? *)

    let next_vreg x = A64.alloc_special x
    let next_scalar_reg x =
      let r,x = next_vreg x in
      to_scalar r,x

    let next_zreg x =
        let r,x = next_vreg x in
        to_scalable_vec r,x

    let next_preg x = A64.alloc_special2 x

    let with_mode m r = match r with
    | Preg (r,_) -> PMreg(r,m)
    | PMreg (r,_) -> PMreg(r,m)
    | _ -> assert false

    let with_direction dir r = match r with
    | ZAreg (tile,_,size) -> ZAreg (tile,Some dir,size)
    | _ -> assert false

    let next_zaslice n st =
      let dir = match n with
                | SIMD.SmV -> Vertical
                | SIMD.SmH -> Horizontal
                | _ -> assert false
      in
      let (i,r),st = A64.alloc_special3 st in
    with_direction dir r,i,st

    let pattern = function
    | 1 -> VL1
    | 2 -> VL2
    | 3 -> VL3
    | 4 -> VL4
    | 5 -> VL5
    | 6 -> VL6
    | 7 -> VL7
    | 8 -> VL8
    | _ -> assert false

    let pseudo = List.map (fun i -> Instruction i)

    let tempo1 st = A.alloc_trashed_reg "T1" st (* May be used for address  *)
    let tempo2 st = A.alloc_trashed_reg "T2" st (* May be used for second address *)
    let tempo3 st = A.alloc_trashed_reg "T3" st (* May be used for STRX flag *)
    let tempo4 st = A.alloc_loop_idx "I4" st (* Loop observer index *)

    let emit_vregs =
      let rec call_rec n st =
        let r1,st = next_vreg st in
        let (r2,rs),st = get_reg_list n st in
        (r1,(r2::rs)),st
      and get_reg_list n st =
        let open SIMD in
        match n with
        | Ne1 | NePa | NePaN | NeP | NeAcqPc | NeRel ->
           let r,st = next_vreg st in (r,[]),st
        | Ne2 | Ne2i -> call_rec Ne1 st
        | Ne3 | Ne3i -> call_rec Ne2 st
        | Ne4 | Ne4i -> call_rec Ne3 st
        | _ -> assert false
      in
      fun n st ->
        let (r,rs),st = get_reg_list n st in
        (r,rs),A.set_friends r rs st

    let emit_zregs =
      let rec call_rec n st =
        let r1,st = next_zreg st in
        let (r2,rs),st = get_reg_list n st in
        (r1,(r2::rs)),st
      and get_reg_list n st =
        let open SIMD in
        match n with
        | Sv1 -> let r,st = next_zreg st in (r,[]),st
        | Sv2i -> call_rec Sv1 st
        | Sv3i -> call_rec Sv2i st
        | Sv4i -> call_rec Sv3i st
        | _ -> assert false
      in
      fun n st ->
        let (r,rs),st = get_reg_list n st in
        (r,rs),A.set_friends r rs st

(******************)
(* Idiosyncrasies *)
(******************)

    let vloc = let open TypBase in
      let sz = match Cfg.typ with
      | Std (_,MachSize.S128) -> V128
      | Std (_,MachSize.Quad) -> V64
      | Int |Std (_,MachSize.Word) -> V32
      | Std (_,(MachSize.Short|MachSize.Byte)) -> V32
      | Pteval -> V64 in
      (* Minimum size is V64 with morello to reduce mixed-size gap with V128 *)
      if do_morello && sz = V32 then V64 else sz

    let sz2v =
      let open MachSize in
      function
        | Byte|Short|Word -> V32
        | Quad -> V64
        | S128 -> V128

    and v2sz =
      let open MachSize in
      function
      | V128 -> S128
      | V64 -> Quad
      | V32 -> Word

    let szloc = v2sz vloc

    let do_movi vdep r i = I_MOV (vdep,r,K i)
    let mov = do_movi vloc

    let mov_mixed sz r i =
      let sz =
        let open MachSize in
        match sz with
        | S128 -> Quad (* MOV C?,#X is not recognized *)
        | Byte|Short|Word|Quad -> sz in
      let v = sz2v sz in I_MOV (v,r,i)

    let mov_reg_addr r1 r2 =  I_MOV (V64,r1,RV (V64,r2))
    let mov_reg r1 r2 = I_MOV (vloc,r1,RV (vloc,r2))
    let mov_reg_mixed sz r1 r2 = let v = sz2v sz in I_MOV (v,r1,RV (v,r2))
    let movi_reg r1 i = I_MOVI_V (r1,i,S_NOEXT)

    module Extra = struct
      let use_symbolic = false
      type reg = A64.reg
      type instruction = A64.pseudo

      let mov r i = Instruction (mov r i)
      let mov_mixed sz r i = Instruction (mov_mixed sz r (K i))
      let mov_reg r1 r2 = Instruction (mov_reg r1 r2)
      let mov_reg_mixed sz r1 r2 = Instruction (mov_reg_mixed sz r1 r2)

    end

    module U = GenUtils.Make(Cfg)(A)(Extra)

    let op3i v op r1 r2 i  =
      let open OpExt in I_OP3 (v,op,r1,r2,Imm (i,0))
    and op3r v op r1 r2 r3 =
      let open OpExt in I_OP3 (v,op,r1,r2,Reg (r3,no_shift))

    let cbz r1 lbl = I_CBZ (vloc,r1,BranchTarget.Lbl lbl)
    let do_cbnz v r1 lbl = I_CBNZ (v,r1,BranchTarget.Lbl lbl)
    let cbnz = do_cbnz vloc
    let is_branch l = match l with
      | Instruction ins -> begin match ins with
        | I_CBZ _|I_CBNZ _|I_TBNZ _|I_TBZ _|I_BR _|I_BLR _|I_RET _ -> true
        | _ -> false
      end
      | _ -> false

    let do_cmpi v r i = op3i v SUBS ZR r i
    let cmpi r i = do_cmpi vloc r i

    let do_csel v r1 r2 r3 = I_CSEL (v,r1,r2,r3,EQ,Cpy)
    let do_cinc v r1 r2 r3 = I_CSEL (v,r1,r2,r3,EQ,Inc)

    let cmp r1 r2 = op3r vloc SUBS ZR r1 r2

    let b lbl = I_B (BranchTarget.Lbl lbl)
    let bne lbl = I_BC (NE,BranchTarget.Lbl lbl)

    let eor sz r1 r2 r3 = op3r sz EOR r1 r2 r3

    let andi sz r1 r2 k = op3i sz AND r1 r2 k

    let incr r = op3i V32 ADD r r 1

    let lsri64 r1 r2 k = op3i V64 LSR r1 r2 k

    let do_addi v r1 r2 k = op3i v ADD r1 r2 k
    let addi = do_addi vloc
    let addi_64 = do_addi V64

    let add v r1 r2 r3 = op3r v ADD r1 r2  r3
    let add_simd r1 r2 = I_ADD_SIMD (r1,r1,r2)

    let do_add64 v r1 r2 r3 =
      match v with
      | V64 -> add v r1 r2 r3
      | _ ->
         let ext = Ext.v2sext v in
         I_ADDSUBEXT (V64,Ext.ADD,r1,r2,(v,r3),(ext, None))

    let do_addcapa r1 r2 r3 =
      I_ADDSUBEXT (V128,Ext.ADD,r1,r2,(V64,r3),Ext.no_ext)
    let gctype r1 r2 = I_GC (GCTYPE,r1,r2)
    let gcvalue r1 r2 = I_GC (GCVALUE,r1,r2)
    let scvalue r1 r2 r3 = I_SC (SCVALUE,r1,r2,r3)
    let seal r1 r2 r3 = I_SEAL (r1,r2,r3)
    let cseal r1 r2 r3 = I_CSEAL (r1,r2,r3)

    let subi sz r1 r2 k = op3i sz SUB r1 r2 k
    let dec r1 r2 = subi vloc r1 r2 1

    let ldr_mixed r1 r2 sz o =
      let idx = MemExt.Imm (o,Idx) in
      let open MachSize in
      match sz with
      | Byte -> I_LDRBH (B,r1,r2,idx)
      | Short -> I_LDRBH (H,r1,r2,idx)
      | Word -> I_LDR (V32,r1,r2,idx)
      | Quad -> I_LDR (V64,r1,r2,idx)
      | S128 -> I_LDR (V128,r1,r2,idx)

    let do_ldr v r1 r2 = I_LDR (v,r1,r2,MemExt.Imm(0,Idx))
    let ldg r1 r2 = I_LDG (r1,r2,0)
    let ldct r1 r2 = I_LDCT(r1,r2)
    let do_ldar vr r1 r2 = I_LDAR (vr,AA,r1,r2)
    let do_ldapr vr r1 r2 = I_LDAR (vr,AQ,r1,r2)
    let ldxr r1 r2 = I_LDAR (vloc,XX,r1,r2)
    let ldaxr r1 r2 = I_LDAR (vloc,AX,r1,r2)
    let sxtw r1 r2 = I_SXTW (r1,r2)

    let do_ldr_idx v1 v2 r1 r2 idx =
      let open MemExt in
      let sext = match v2 with
        | V32 -> SXTW
        | V64|V128 -> LSL in
      I_LDR (v1,r1,r2,Reg (v2,idx,sext,0))

    let ldn n rs rt =
      let open SIMD in
      match n with
      | Ne1 | Ne2 | Ne3 | Ne4 -> I_LD1M (rs,rt,K 0)
      | Ne2i -> I_LD2M (rs,rt,K 0)
      | Ne3i -> I_LD3M (rs,rt,K 0)
      | Ne4i -> I_LD4M (rs,rt,K 0)
      | _ -> assert false

    let ldnsv n rs pg rn idx=
      let open SIMD in
      match n with
      | Sv1 -> I_LD1SP (VSIMD32,rs,with_mode Zero pg,rn,idx)
      | Sv2i -> I_LD2SP (VSIMD32,rs,with_mode Zero pg,rn,idx)
      | Sv3i -> I_LD3SP (VSIMD32,rs,with_mode Zero pg,rn,idx)
      | Sv4i -> I_LD4SP (VSIMD32,rs,with_mode Zero pg,rn,idx)
      | _ -> assert false

    let ldr_mixed_idx v r1 r2 idx sz  =
      let idx = MemExt.v2idx_reg v idx in
      let open MachSize in
      match sz with
      | Byte -> I_LDRBH (B,r1,r2,idx)
      | Short -> I_LDRBH (H,r1,r2,idx)
      | Word -> I_LDR (V32,r1,r2,idx)
      | Quad -> I_LDR (V64,r1,r2,idx)
      | S128 -> I_LDR (V128,r1,r2,idx)

    let str_mixed sz o r1 r2 =
      let idx = MemExt.Imm (o,Idx) in
      let open MachSize in
      match sz with
      | Byte -> I_STRBH (B,r1,r2,idx)
      | Short -> I_STRBH (H,r1,r2,idx)
      | Word -> I_STR (V32,r1,r2,idx)
      | Quad -> I_STR (V64,r1,r2,idx)
      | S128 -> I_STR (V128,r1,r2,idx)

    let do_str v r1 r2 = I_STR (v,r1,r2,MemExt.Imm (0,Idx))
    let str = do_str vloc
    let stg r1 r2 = I_STG (r1,r2,(0,Idx))
    let stct r1 r2 = I_STCT(r1,r2)
    let do_stlr v r1 r2 = I_STLR (v,r1,r2)
    let stlr = do_stlr vloc

    let do_str_idx v r1 r2 idx = I_STR (vloc,r1,r2,MemExt.v2idx_reg v idx)
    let str_idx = do_str_idx vloc
    let stxr r1 r2 r3 = I_STXR (vloc,YY,r1,r2,r3)
    let stlxr r1 r2 r3 = I_STXR (vloc,LY,r1,r2,r3)
    let stn n rs rt =
      let open SIMD in
      match n with
      | Ne1 | Ne2 | Ne3 | Ne4 -> I_ST1M (rs,rt,K 0)
      | Ne2i -> I_ST2M (rs,rt,K 0)
      | Ne3i -> I_ST3M (rs,rt,K 0)
      | Ne4i -> I_ST4M (rs,rt,K 0)
      | _ -> assert false

    let stnsv n rs pg rn idx =
      let open SIMD in
      match n with
      | Sv1 -> I_ST1SP (VSIMD32,rs,pg,rn,idx)
      | Sv2i -> I_ST2SP (VSIMD32,rs,pg,rn,idx)
      | Sv3i -> I_ST3SP (VSIMD32,rs,pg,rn,idx)
      | Sv4i -> I_ST4SP (VSIMD32,rs,pg,rn,idx)
      | _ -> assert false

    let stxr_sz t sz r1 r2 r3 =
      let open MachSize in
      match sz with
      | Byte -> I_STXRBH (B,t,r1,r2,r3)
      | Short -> I_STXRBH (H,t,r1,r2,r3)
      | Word -> I_STXR (V32,t,r1,r2,r3)
      | Quad -> I_STXR (V64,t,r1,r2,r3)
      | S128 -> I_STXR (V128,t,r1,r2,r3)

    let ldxr_sz t sz r1 r2 =
      let open MachSize in
      match sz with
      | Byte -> I_LDARBH (B,t,r1,r2)
      | Short -> I_LDARBH (H,t,r1,r2)
      | Word -> I_LDAR (V32,t,r1,r2)
      | Quad -> I_LDAR (V64,t,r1,r2)
      | S128 -> I_LDAR (V128,t,r1,r2)

    let sumi_addr_gen tempo st rA o = match o with
    | 0 -> rA,[],st
    | _ ->
        let r,st = tempo st in
        r,[addi_64 r rA o],st

    let sumi_addr st rA o = sumi_addr_gen tempo1 st rA o

    let str_mixed_idx sz v r1 r2 idx  =
      let idx = MemExt.v2idx_reg v idx in
      let open MachSize in
      match sz with
      | Byte -> I_STRBH (B,r1,r2,idx)
      | Short -> I_STRBH (H,r1,r2,idx)
      | Word -> I_STR (V32,r1,r2,idx)
      | Quad -> I_STR (V64,r1,r2,idx)
      | S128 -> I_STR (V128,r1,r2,idx)

    let swp_mixed sz a rS rT rN =
      let open MachSize in
      match sz with
      | Byte -> I_SWPBH (B,a,rS,rT,rN)
      | Short ->  I_SWPBH (H,a,rS,rT,rN)
      | Word ->  I_SWP (V32,a,rS,rT,rN)
      | Quad ->  I_SWP (V64,a,rS,rT,rN)
      | S128 ->  I_SWP (V128,a,rS,rT,rN)

    let swp a rS rT rN =  I_SWP (vloc,a,rS,rT,rN)

    let sctag a rN rM = I_SC (SCTAG,a,rN,rM)

    let cas_mixed sz a rS rT rN =
      let open MachSize in
      match sz with
      | Byte -> I_CASBH (B,a,rS,rT,rN)
      | Short ->  I_CASBH (H,a,rS,rT,rN)
      | Word ->  I_CAS (V32,a,rS,rT,rN)
      | Quad ->  I_CAS (V64,a,rS,rT,rN)
      | S128 ->  I_CAS (V128,a,rS,rT,rN)

    let cas a rS rT rN =  I_CAS (vloc,a,rS,rT,rN)

    let ldop_mixed op sz a rS rT rN =
      let open MachSize in
      match sz with
      | Byte -> I_LDOPBH (op,B,a,rS,rT,rN)
      | Short ->  I_LDOPBH (op,H,a,rS,rT,rN)
      | Word ->  I_LDOP (op,V32,a,rS,rT,rN)
      | Quad ->  I_LDOP (op,V64,a,rS,rT,rN)
      | S128 ->  I_LDOP (op,V128,a,rS,rT,rN)

    let ldop op a rS rT rN =  I_LDOP (op,vloc,a,rS,rT,rN)

    let stop_mixed op sz a rS rN =
      let open MachSize in
      match sz with
      | Byte -> I_STOPBH (op,B,a,rS,rN)
      | Short ->  I_STOPBH (op,H,a,rS,rN)
      | Word ->  I_STOP (op,V32,a,rS,rN)
      | Quad ->  I_STOP (op,V64,a,rS,rN)
      | S128 ->  I_STOP (op,V128,a,rS,rN)

    let stop op a rS rN =  I_STOP (op,vloc,a,rS,rN)

    let do_sum_addr v st rA idx =
      let r,st = tempo1 st in
      if do_morello then
        r,[do_addcapa r rA idx],st
      else
        r,[do_add64 v r rA idx],st

    let sum_addr = do_sum_addr vloc

    let stlr_of_sz sz r1 r2 =
      let open MachSize in
      match sz with
      | Byte  -> I_STLRBH (B,r1,r2)
      | Short -> I_STLRBH (H,r1,r2)
      | Word -> I_STLR (V32,r1,r2)
      | Quad -> I_STLR (V64,r1,r2)
      | S128 -> I_STLR (V128,r1,r2)

    let stlr_mixed sz o st r1 r2 =
      let rA,cs_sum,st = sumi_addr st r2 o in
      cs_sum@[stlr_of_sz sz r1 rA],st

    let stlr_mixed_idx sz st r1 r2 idx  =
      let rA,cs_sum,st = sum_addr st r2 idx in
      cs_sum@[stlr_of_sz sz r1 rA],st

    let ldar_mixed t sz o st r1 r2 =
      let rA,cs,st = sumi_addr st r2 o in
      let ld =
        let open MachSize in
        match sz  with
        | Byte -> I_LDARBH (B,t,r1,rA)
        | Short -> I_LDARBH (H,t,r1,rA)
        | Word -> I_LDAR (V32,t,r1,rA)
        | Quad -> I_LDAR (V64,t,r1,rA)
        | S128 -> I_LDAR (V128,t,r1,rA) in
      cs@[ld],st

    let do_ldar_mixed_idx v t sz o st r1 r2 idx =
      let rA,cs1,st = sumi_addr st r2 o in
      let rA,cs2,st = do_sum_addr v st rA idx in
      let ld =
        let open MachSize in
        match sz  with
        | Byte -> I_LDARBH (B,t,r1,rA)
        | Short -> I_LDARBH (H,t,r1,rA)
        | Word -> I_LDAR (V32,t,r1,rA)
        | Quad -> I_LDAR (V64,t,r1,rA)
        | S128 -> I_LDAR (V128,t,r1,rA) in
      cs1@cs2@[ld],st

    let ldar_mixed_idx = do_ldar_mixed_idx vloc

    let do_ldp opt r1 r2 rA = I_LDP (opt,vloc,r1,r2,rA,(0,Idx))
    and do_ldxp opt r1 r2 rA = I_LDXP (vloc,opt,r1,r2,rA)
    let do_stp opt r1 r2 rA = I_STP (opt,vloc,r1,r2,rA,(0,Idx))
    and do_stxp opt r r1 r2 rA = I_STXP (vloc,opt,r,r1,r2,rA)

(*********)
(* loads *)
(*********)

    module type L = sig
      type sz
      val sz0 : sz
      val load : sz -> A.st -> reg -> reg -> instruction list * A.st
      val load_idx : sz -> sz -> A.st -> reg -> reg -> reg -> instruction list * A.st
      val next_reg : A.st -> Code.proc -> sz -> reg * A.st
    end

    let type_of_sz sz =
      let open TypBase in
      Std (Unsigned,MachSize.at_least_word sz)

    let next_reg_sz st p sz =
      let r,st = next_reg st in
      let loc = A.Reg (p,r) in
      let st = A.add_type loc (type_of_sz sz) st in
      r,st

    let next_reg_var st p var = next_reg_sz st p (v2sz var)

    let emit_load_mixed sz o st p init x =
      let rA,st = next_reg_sz st p sz in
      let rB,init,st = U.next_init st p init x in
      rA,init,lift_code [ldr_mixed rA rB sz o],st

    let _emit_load_int_idx o st  _p init rA =
      let r1,st = next_reg st in
      r1,init,lift_code [ldr_mixed r1 rA szloc o],st

    let emit_ldr_addon a r = match a with
    | Some Capability -> assert do_morello ; [gcvalue r r]
    | None -> []

    let do_emit_load_idx_var next_reg_loc load_idx v1 v2  st p init x idx =
      let rA,st =
        if do_mixed then next_reg_loc st p v1
        else next_reg st in
      let rB,init,st = U.next_init st p init x in
      let ins,st = load_idx v1 v2 st rA rB idx in
      rA,init,pseudo ins ,st

    module LOAD(L:L) =
      struct

        let emit_load_var_reg vr st p init rB =
          let rA,st =
            if do_mixed then L.next_reg st p vr
            else next_reg st in
          let ld,st = L.load vr st rA rB in
          rA,init,lift_code ld,st

        let emit_load_var vr st p init x =
          let rB,init,st = U.next_init st p init x in
          emit_load_var_reg vr st p init rB

        let emit_load =  emit_load_var L.sz0

        let emit_fetch st _p init lab =
          let rA,st = next_reg st in
          let lab0 = ".+12" in
          let lab1 = ".+8" in
          let cs =
            Label (lab,Instruction (b lab0))::
            Instruction (mov rA 2)::
            Instruction (b lab1)::
            Instruction (mov rA 1)::
            [] in
          rA,init,cs,st

        let emit_load_not_zero st p init x =
          let rA,st = next_reg st in
          let rB,init,st = U.next_init st p init x in
          let ld,st = L.load L.sz0 st rA rB in
          let lab = Label.next_label "L" in
          rA,init,
          Label (lab,Nop)::
          lift_code (ld@[cbz rA lab]),
          st

        let emit_load_one st p init x =
          let rA,st = next_reg st in
          let rB,init,st = U.next_init st p init x in
          let ld,st = L.load L.sz0 st rA rB in
          let lab = Label.next_label "L" in
          rA,init,
          Label (lab,Nop)::
          pseudo (ld@[cmpi rA 1; bne lab]),
          st

        let emit_load_not st p init x cmp =
          let rA,st = next_reg st in
          let rC,st = tempo4 st in
          let rB,init,st = U.next_init st p init x in
          let ld,st = L.load L.sz0 st rA rB in
          let lab = Label.next_label "L" in
          let out = Label.next_label "L" in
          rA,init,
          Instruction (mov rC 200)::
          (* 200 X about 5 ins looks for a typical memory delay *)
          Label (lab,Nop)::
          pseudo
            (ld@
             [cmp rA;
              bne out; I_OP3 (vloc,SUBS,rC,rC,OpExt.Imm (1,0));
              cbnz rC lab ;
            ])@
          [Label (out,Nop)],st

        let emit_load_not_eq st p init x rP =
          emit_load_not st p init x (fun r -> cmp r rP)

        let emit_load_not_value st p init x v =
          emit_load_not st p init x (fun r -> cmpi r v)

        let emit_load_idx_var = do_emit_load_idx_var L.next_reg L.load_idx

      end

    let wrap_st emit st r1 r2 =
      let c = emit r1 r2 in
      [c],st

    module LDR =
      LOAD
        (struct
          type sz = A64.variant
          let sz0 = vloc
          let load vr = wrap_st (do_ldr vr)
          let load_idx v1 v2 st rA rB idx = [do_ldr_idx v1 v2 rA rB idx],st
          let next_reg = next_reg_var
        end)

    module LDN = struct

      let emit_load_reg n st init rA =
        let (r,rs),st = emit_vregs n st in
        let adds = List.map (fun v -> add_simd r v) rs in
        let rS,st = next_scalar_reg st in
        let addv = [I_ADDV(A64.VSIMD32,rS,r)] in
        let rX,st = next_reg st in
        let fmov = [I_FMOV_TG(A64.V32,rX,A64.VSIMD32,rS)] in
        rX,init,lift_code ([ldn n (r::rs) rA]@adds@addv@fmov),st

      let emit_load n st p init loc =
        let rA,init,st = U.next_init st p init loc in
        emit_load_reg n st init rA

      let emit_load_idx n v st p init loc ridx =
        let rA,init,st = U.next_init st p init loc in
        let rA,csA,st = do_sum_addr v st rA ridx in
        let r,init,cs,st = emit_load_reg n st init rA in
        r,init,pseudo csA@cs,st
    end


    module LDP = struct

      let emit_load_reg temporal st init rA =
        let r1,st = next_vreg st in
        let r2,st = next_vreg st in
        let ldp = [I_LDP_SIMD(temporal,A64.VSIMD32,to_scalar r1,to_scalar r2,rA,(0,A64.Idx))] in
        let r3,st = next_vreg st in
        let add = [I_ADD_SIMD (r3,r1,r2)] in
        let rX,st = next_reg st in
        let fmov = [I_FMOV_TG(A64.V32,rX,A64.VSIMD32,to_scalar r3)] in
        rX,init,lift_code (ldp@add@fmov),st

      let emit_load t st p init loc =
        let rA,init,st = U.next_init st p init loc in
        emit_load_reg t st init rA

      let emit_load_idx t v st p init loc ridx =
        let rA,init,st = U.next_init st p init loc in
        let rA,csA,st = do_sum_addr v st rA ridx in
        let r,init,cs,st = emit_load_reg t st init rA in
        r,init,pseudo csA@cs,st
    end

    module LDUR = struct
      let emit_load_reg st init rA =
        let r,st = next_scalar_reg st in
        let ldur = [I_LDUR_SIMD(A64.VSIMD32,r,rA,0)] in
        let rX,st = next_reg st in
        let fmov = [I_FMOV_TG(A64.V32,rX,A64.VSIMD32,r)] in
        rX,init,lift_code (ldur@fmov),st

      let emit_load st p init loc =
        let rA,init,st = U.next_init st p init loc in
        emit_load_reg st init rA

      let emit_load_idx v st p init loc ridx =
        let rA,init,st = U.next_init st p init loc in
        let rA,csA,st = do_sum_addr v st rA ridx in
        let r,init,cs,st = emit_load_reg st init rA in
        r,init,pseudo csA@cs,st
    end

    module LDAPUR = struct
      let emit_load_reg st init rA =
        let r,st = next_scalar_reg st in
        let ldur = [I_LDAPUR_SIMD(A64.VSIMD32,r,rA,0)] in
        let rX,st = next_reg st in
        let fmov = [I_FMOV_TG(A64.V32,rX,A64.VSIMD32,r)] in
        rX,init,lift_code (ldur@fmov),st

      let emit_load st p init loc =
        let rA,init,st = U.next_init st p init loc in
        emit_load_reg st init rA

      let emit_load_idx v st p init loc ridx =
        let rA,init,st = U.next_init st p init loc in
        let rA,csA,st = do_sum_addr v st rA ridx in
        let r,init,cs,st = emit_load_reg st init rA in
        r,init,pseudo csA@cs,st
    end

    module LDNW = struct

      let emit_load_reg n st init rA idx =
        let pred,st = next_preg st in
        let acc,st = next_vreg st in
        let nelem = SIMD.nelements n in
        let ptrue = [I_PTRUE (pred,pattern nelem)] in
        let (r,rs),st = emit_zregs n st in
        let load = [ldnsv n (r::rs) pred rA idx] in
        let reduce = (List.map (fun v -> I_ADD_SV (r,r,v)) rs)@[I_UADDV (VSIMD64,to_scalar acc,pred,r)] in
        let rX,st = next_reg st in
        let fmov = [I_FMOV_TG(V32,rX,VSIMD32,to_scalar acc)] in
        rX,init,lift_code (ptrue@load@reduce@fmov),st

      let emit_load n st p init loc =
        let open MemExt in
        let idx = Imm(0,Idx) in
        let rA,init,st = U.next_init st p init loc in
        emit_load_reg n st init rA idx

      let emit_load_idx n v st p init loc ridx =
        let open MemExt in
        let rA,init,st = U.next_init st p init loc in
        let rI,csI,st = match v with
        | V32 ->
          let r,st = next_reg st in
          r,[sxtw r ridx],st
        | _ -> ridx,[],st
        in
        let idx = Reg(V64,rI,LSL,2) in
        let r,init,cs,st = emit_load_reg n st init rA idx in
        r,init,pseudo csI@cs,st
    end

    module LD1G = struct

      let emit_load_reg n st init rA idx =
        let pred,st = next_preg st in
        let acc,st = next_vreg st in
        let nelem = SIMD.nelements n in
        let ptrue = [I_PTRUE (pred,pattern nelem)] in
        let r,st = next_zreg st in
        let load = [I_LD1SP (VSIMD32,[r],with_mode Zero pred,rA,idx)] in
        let reduce = [I_UADDV (VSIMD64,to_scalar acc,pred,r)] in
        let rX,st = next_reg st in
        let fmov = [I_FMOV_TG(V32,rX,VSIMD32,to_scalar acc)] in
        rX,init,lift_code (ptrue@load@reduce@fmov),st

      let emit_load n st p init loc =
        let open MemExt in
        let rA,init,st = U.next_init st p init loc in
        let rI,st = next_zreg st in
        let csI = [I_INDEX_II (rI,0,1)] in
        let idx = ZReg(rI,UXTW,2) in
        let r,init,cs,st = emit_load_reg n st init rA idx in
        r,init,pseudo csI@cs,st

      let emit_load_idx n v st p init loc ridx =
        let open MemExt in
        let rA,init,st = U.next_init st p init loc in
        let rI,st = next_zreg st in
        let csI = [I_INDEX_SI (rI,v,ridx,1)] in
        let idx = ZReg(rI,UXTW,2) in
        let r,init,cs,st = emit_load_reg n st init rA idx in
        r,init,pseudo csI@cs,st
    end

    module LD1T = struct

      let emit_load_reg n st init rA idx =
        let smstart = [I_SMSTART (None)] in
        let pred,st = next_preg st in
        let nelem = SIMD.nelements n in
        let ptrue = [I_PTRUE (pred,pattern nelem)] in
        let movx12 = [I_MOV (V32,x12,RV (V32,ZR))] in
        let tile,slice,st = next_zaslice n st in
        let load = [I_LD1SPT (VSIMD32,tile,x12,slice,with_mode Zero pred,rA,idx)] in
        let r,st = next_zreg st in
        let mova = [I_MOVA_TV (r,with_mode Merge pred,tile,x12,slice)] in
        let acc,st = next_vreg st in
        let reduce = [I_UADDV (VSIMD64,to_scalar acc,pred,r)] in
        let rX,st = next_reg st in
        let fmov = [I_FMOV_TG(V32,rX,VSIMD32,to_scalar acc)] in
        let smstop = [I_SMSTOP (None)] in
        rX,init,lift_code (smstart@ptrue@movx12@load@mova@reduce@fmov@smstop),st

        let emit_load n st p init loc =
          let open MemExt in
          let idx = Imm(0,Idx) in
          let rA,init,st = U.next_init st p init loc in
          emit_load_reg n st init rA idx

        let emit_load_idx n v st p init loc ridx =
          let open MemExt in
          let rA,init,st = U.next_init st p init loc in
          let rI,csI,st = match v with
          | V32 ->
            let r,st = next_reg st in
            r,[sxtw r ridx],st
          | _ -> ridx,[],st
          in
          let idx = Reg(V64,rI,LSL,2) in
          let r,init,cs,st = emit_load_reg n st init rA idx in
          r,init,pseudo csI@cs,st
    end

    module LDG = struct
      let emit_load st p init x =
        let rA,st = next_reg st in
        let rB,init,st = U.next_init st p init x in
        rA,init,lift_code [mov_reg_addr rA rB;ldg rA rB],st

      let emit_load_idx v st p init x idx =
        let rA,st = next_reg st in
        let rB,init,st = U.next_init st p init x in
        let rC,c,st = do_sum_addr v st rB idx in
        rA,init,lift_code (mov_reg_addr rA rB::c@[ldg rA rC]),st

    end

    module LDCT = struct
      let emit_load st p init x =
        let rA,st = next_reg st in
        let rB,init,st = U.next_init st p init x in
        rA,init,lift_code [ldct rA rB],st

      let emit_load_idx st p init x idx =
        let rA,init,st = U.next_init st p init x in
        let rB,st = next_reg st in
        let rC,st = next_reg st in
        rC,init,lift_code ([sctag rB rA idx; ldct rC rB]),st
    end

    module OBS =
      LOAD
        (struct
          type sz = MachSize.sz
          let sz0 = naturalsize
          let load vr st rA rB = [ldr_mixed rA rB vr 0],st
          let load_idx v1 _v2 st rA rB idx =
            [ldr_mixed_idx vloc rA rB idx v1],st
          let next_reg = next_reg_sz
        end)

(* Load pair *)


    module LDAR = LOAD
        (struct
          type sz = A64.variant
          let sz0 = vloc
          let load vr = wrap_st (do_ldar vr)
          let load_idx v1 v2 st rA rB idx =
            let r,ins,st = do_sum_addr v2 st rB idx in
            ins@[do_ldar v1 rA r],st
          let next_reg = next_reg_var
        end)

    module LDAPR = LOAD
        (struct
          type sz = A64.variant
          let sz0 = vloc
          let load vr = wrap_st (do_ldapr vr)
          let load_idx v1 v2 st rA rB idx =
            let r,ins,st = do_sum_addr v2 st rB idx in
            ins@[do_ldapr v1 rA r],st
          let next_reg = next_reg_var
        end)


(**********)
(* Stores *)
(**********)

    let seal_dp_addr init p loc st rd v =
      let rB,init,st = U.next_init st p init loc in
      let rC,st = next_reg st in
      let cs = lift_code [
        subi V64 rC rd v;
        andi V64 rC rC 0xfff;
        scvalue rC rB rC;
        cseal rC rB rC; ] in
      (rB,rC),init,cs,st

    module type S = sig
      val store : A.st -> reg -> reg -> instruction list * A.st
      val store_idx : A.st -> reg -> reg -> reg -> instruction list * A.st
      val emit_mov :
        A.st -> Code.proc -> A.init -> int ->
        reg * A.init * Extra.instruction list * A.st
    end

    let emit_str_addon st p init rA rB a e = match a with
    | Some Capability ->
      assert do_morello ;
      let init,cs,st = if e.C.cseal > 0 then
        let r,init,csi,st = U.emit_mov st p init e.C.cseal in
        init,csi@lift_code [scvalue rA rB rA; seal rA rA r],st
      else init,[],st in
      let init,cs,st = if e.C.ctag > 0 then
        let r,init,csi,st = U.emit_mov st p init e.C.ctag in
        init,cs@csi@lift_code [sctag rA rA r],st
      else init,cs,st in
      init,cs,st
    | None -> init,[],st

    let emit_store_reg_mixed sz o st p init x rA a e =
      let rB,init,st = U.next_init st p init x in
      let init,csi,st = emit_str_addon st p init rA rB a e in
      init,csi@[Instruction (str_mixed sz o rA rB)],st

    let do_emit_mov_sz emit_mov_sz sz st p init v =
      let rA,init,csi,st = emit_mov_sz sz st p init v in
      let st =
        let loc = A.Reg (p,rA) in
        let t = type_of_sz sz in
        A.add_type loc t st in
      rA,init,csi,st

      let emit_mov_sz = do_emit_mov_sz U.emit_mov_sz
      let emit_mov_sz_fresh = do_emit_mov_sz U.emit_mov_sz_fresh

      let emit_store_mixed sz o st p init x v a e =
        let rA,init,csi,st = emit_mov_sz sz st p init v in
        let init,cs,st = emit_store_reg_mixed sz o st p init x rA a e in
        init,csi@cs,st

    let _emit_store_int_idx o st p init rA v =
      let r1,init,csi,st = U.emit_mov st p init v in
      init,csi@pseudo [str_mixed szloc o r1 rA;],st

    module STORE(S:S) =
      struct

        let emit_store_reg st p init x rA a e =
          let rB,init,st = U.next_init st p init x in
          let init,csi,st = emit_str_addon st p init rA rB a e in
          let cs,st = S.store st rA rB in
          init,csi@pseudo cs,st

        let emit_store st p init x v a e =
          let rA,init,csi,st = S.emit_mov st p init v in
          let init,cs,st = emit_store_reg st p init x rA a e in
          init,csi@cs,st

        let emit_store_idx_reg st p init x idx rA a e =
          let rB,init,st = U.next_init st p init x in
          let init,csi,st = emit_str_addon st p init rA rB a e in
          let ins,st = S.store_idx st rA rB idx in
          init,csi@pseudo ins,st

        let emit_store_idx st p init x idx v a e =
          let rA,init,csi,st = S.emit_mov st p init v in
          let init,cs,st = emit_store_idx_reg st p init x idx rA a e in
          init,csi@cs,st

        let emit_store_nop st p init lab =
          let rA,init,st = U.emit_nop st p init nop in
          let rB,init,st = U.next_init st p init lab in
          let cs,st = S.store st rA rB in
          init,pseudo cs,st
      end

    module STR =
      STORE
        (struct
          let store = wrap_st str
          let store_idx st rA rB idx = [str_idx rA rB idx],st
          let emit_mov = U.emit_mov
        end)

    module STN = struct
      let emit_store_reg n st init rA v =
        let (r,rs),st = emit_vregs n st in
        let movi = List.mapi (fun i r -> movi_reg r (v+i)) (r::rs) in
        init,pseudo movi@pseudo [stn n (r::rs) rA],st

      let emit_store n st p init loc v =
        let rA,init,st = U.next_init st p init loc in
        emit_store_reg n st init rA v

      let emit_store_idx n vdep st p init loc ridx v =
        let rA,init,st = U.next_init st p init loc in
        let rA,csA,st = do_sum_addr vdep st rA ridx in
        let init,cs,st = emit_store_reg n st init rA v in
        init,pseudo csA@cs,st

      let emit_store_dep n vdep st init rA v =
        let (r,rs),st = emit_vregs n st in
        let rB,st = next_vreg st in
        let dup = [I_DUP (rB,A64.V32,vdep)] in
        let movi = List.mapi (fun i r -> movi_reg r (v+i)) (r::rs) in
        let adds = List.map (fun v -> add_simd v rB) (r::rs) in
        let stn = [stn n (r::rs) rA] in
        init,lift_code(dup@movi@adds@stn),st
    end

    module STP = struct
      let emit_store_reg temporal st init rA v =
        let r1,st = next_vreg st in
        let r2,st = next_vreg st in
        let movi = List.mapi (fun i r -> movi_reg r (v+i)) [r1;r2] in
        let stp = [I_STP_SIMD(temporal,A64.VSIMD32,to_scalar r1,to_scalar r2,rA,(0,A64.Idx))] in
        init,pseudo movi@pseudo stp,st

      let emit_store n st p init loc v =
        let rA,init,st = U.next_init st p init loc in
        emit_store_reg n st init rA v

      let emit_store_idx n vdep st p init loc ridx v =
        let rA,init,st = U.next_init st p init loc in
        let rA,csA,st = do_sum_addr vdep st rA ridx in
        let init,cs,st = emit_store_reg n st init rA v in
        init,pseudo csA@cs,st

      let emit_store_dep temporal vdep st init rA v =
        let rB,st = next_vreg st in
        let dup = [I_DUP (rB,A64.V32,vdep)] in
        let r1,st = next_vreg st in
        let r2,st = next_vreg st in
        let movi = List.mapi (fun i r -> movi_reg r (v+i)) [r1;r2] in
        let adds = List.map (fun v -> add_simd v rB) [r1;r2] in
        let stp = [I_STP_SIMD(temporal,A64.VSIMD32,to_scalar r1,to_scalar r2,rA,(0,A64.Idx))] in
        init,lift_code(dup@movi@adds@stp),st
    end

    module STUR = struct
      let emit_store_reg st init rA v =
        let r,st = next_vreg st in
        let movi = [movi_reg r v] in
        let stur = [I_STUR_SIMD(A64.VSIMD32,to_scalar r,rA,0)] in
        init,lift_code(movi@stur),st

      let emit_store st p init loc v =
        let rA,init,st = U.next_init st p init loc in
        emit_store_reg st init rA v

      let emit_store_idx vdep st p init loc ridx v =
        let rA,init,st = U.next_init st p init loc in
        let rA,csA,st = do_sum_addr vdep st rA ridx in
        let init,cs,st = emit_store_reg st init rA v in
        init,pseudo csA@cs,st

      let emit_store_dep vdep st init rA v =
        let rB,st = next_vreg st in
        let dup = [I_DUP (rB,A64.V32,vdep)] in
        let r1,st = next_vreg st in
        let movi = [movi_reg r1 v] in
        let adds = [add_simd r1 rB]in
        let stur = [I_STUR_SIMD(A64.VSIMD32,to_scalar r1,rA,0)] in
        init,lift_code(dup@movi@adds@stur),st
    end

    module STLUR = struct
      let emit_store_reg st init rA v =
        let r,st = next_vreg st in
        let movi = [movi_reg r v] in
        let stlur = [I_STLUR_SIMD(A64.VSIMD32,to_scalar r,rA,0)] in
        init,lift_code(movi@stlur),st

      let emit_store st p init loc v =
        let rA,init,st = U.next_init st p init loc in
        emit_store_reg st init rA v

      let emit_store_idx vdep st p init loc ridx v =
        let rA,init,st = U.next_init st p init loc in
        let rA,csA,st = do_sum_addr vdep st rA ridx in
        let init,cs,st = emit_store_reg st init rA v in
        init,pseudo csA@cs,st

      let emit_store_dep vdep st init rA v =
        let rB,st = next_vreg st in
        let dup = [I_DUP (rB,A64.V32,vdep)] in
        let r1,st = next_vreg st in
        let movi = [movi_reg r1 v] in
        let adds = [add_simd r1 rB]in
        let stlur = [I_STLUR_SIMD(A64.VSIMD32,to_scalar r1,rA,0)] in
        init,lift_code(dup@movi@adds@stlur),st
    end

    module STNW = struct
      let emit_store_reg n st init rA v idx =
        let pred,st = next_preg st in
        let nelem = SIMD.nelements n in
        let ptrue = [I_PTRUE (pred,pattern nelem)] in
        let (r,rs),st = emit_zregs n st in
        let mov_sv = List.mapi (fun i r -> I_MOV_SV (r,(v+i),S_NOEXT)) (r::rs) in
        let setup = mov_sv@ptrue in
        let store = [stnsv n (r::rs) pred rA idx] in
        init,lift_code (setup@store),st

      let emit_store n st p init loc v =
        let open MemExt in
        let idx = Imm(0,Idx) in
        let rA,init,st = U.next_init st p init loc in
        emit_store_reg n st init rA v idx

      let emit_store_idx n vdep st p init loc ridx v =
        let open MemExt in
        let rA,init,st = U.next_init st p init loc in
        let rI,csI,st = match vdep with
        | V32 ->
          let r,st = next_reg st in
          r,[sxtw r ridx],st
        | _ -> ridx,[],st
        in
        let idx = Reg(V64,rI,LSL,2) in
        let init,cs,st = emit_store_reg n st init rA v idx in
        init,pseudo csI@cs,st

      let emit_store_dep n vdep st init rA v =
        let open MemExt in
        let idx = Imm(0,Idx) in
        let rB,st = next_zreg st in
        let dup_sv = [I_DUP_SV (rB,V32,vdep)] in
        let (r,rs),st = emit_zregs n st in
        let mov_sv = List.mapi (fun i r -> I_MOV_SV (r,(v+i),S_NOEXT)) (r::rs) in
        let add_sv = List.map (fun v -> I_ADD_SV(v,v,rB)) (r::rs) in
        let pred,st = next_preg st in
        let nelem = SIMD.nelements n in
        let ptrue = [I_PTRUE (pred,pattern nelem)] in
        let setup = dup_sv@mov_sv@add_sv@ptrue in
        let store = [stnsv n (r::rs) pred rA idx] in
        init,lift_code (setup@store),st
    end

    module ST1S = struct
      let emit_store_reg n st init rA v idx=
        let pred,st = next_preg st in
        let nelem = SIMD.nelements n in
        let ptrue = [I_PTRUE (pred,pattern nelem)] in
        let r,st = next_zreg st in
        let mov_sv = [I_MOV_SV (r,v,S_NOEXT)] in
        let setup = mov_sv@ptrue in
        let store = [I_ST1SP (VSIMD32,[r],pred,rA,idx)] in
        init,lift_code (setup@store),st

      let emit_store n st p init loc v =
        let open MemExt in
        let rA,init,st = U.next_init st p init loc in
        let rI,st = next_zreg st in
        let csI = [I_INDEX_II (rI,0,1)] in
        let idx = ZReg(rI,UXTW,2) in
        let init,cs,st = emit_store_reg n st init rA v idx in
        init,pseudo csI@cs,st

      let emit_store_idx n vdep st p init loc ridx v =
        let open MemExt in
        let rA,init,st = U.next_init st p init loc in
        let rI,st = next_zreg st in
        let csI = [I_INDEX_SI (rI,vdep,ridx,1)] in
        let idx = ZReg(rI,UXTW,2) in
        let init,cs,st = emit_store_reg n st init rA v idx in
        init,pseudo csI@cs,st

      let emit_store_dep n vdep st init rA v =
        let open MemExt in
        let rB,st = next_zreg st in
        let dup_sv = [I_DUP_SV (rB,V32,vdep)] in
        let r,st = next_zreg st in
        let mov_sv = [I_MOV_SV (r,v,S_NOEXT)] in
        let add_sv = [I_ADD_SV (r,r,rB)] in
        let pred,st = next_preg st in
        let rI,st = next_zreg st in
        let nelem = SIMD.nelements n in
        let ptrue = [I_PTRUE (pred,pattern nelem)] in
        let index = [I_INDEX_II (rI,0,1)] in
        let setup = dup_sv@mov_sv@add_sv@ptrue in
        let store = [I_ST1SP (VSIMD32,[r],pred,rA,ZReg(rI,UXTW,2))] in
        init,lift_code (index@setup@store),st
    end

    module ST1T = struct
      let emit_store_reg n st init rA v idx=
        let smstart = [I_SMSTART (None)] in
        let pred,st = next_preg st in
        let nelem = SIMD.nelements n in
        let ptrue = [I_PTRUE (pred,pattern nelem)] in
        let movx12 = [I_MOV (V32,x12,RV (V32,ZR))] in
        let r,st = next_zreg st in
        let mov_sv = [I_MOV_SV (r,v,S_NOEXT)] in
        let tile,slice,st = next_zaslice n st in
        let mova = [I_MOVA_VT (tile,x12,slice,with_mode Merge pred,r)] in
        let setup = mov_sv@ptrue@movx12@mova in
        let store = [I_ST1SPT (VSIMD32,tile,x12,slice,pred,rA,idx)] in
        let smstop = [I_SMSTOP (None)] in
        init,lift_code (smstart@setup@store@smstop),st

      let emit_store n st p init loc v =
        let open MemExt in
        let idx = Imm(0,Idx) in
        let rA,init,st = U.next_init st p init loc in
        emit_store_reg n st init rA v idx

      let emit_store_idx n vdep st p init loc ridx v =
        let open MemExt in
        let rA,init,st = U.next_init st p init loc in
        let rI,csI,st = match vdep with
        | V32 ->
          let r,st = next_reg st in
          r,[sxtw r ridx],st
        | _ -> ridx,[],st
        in
        let idx = Reg(V64,rI,LSL,2) in
        let init,cs,st = emit_store_reg n st init rA v idx in
        init,pseudo csI@cs,st

      let emit_store_dep n vdep st init rA v =
        let open MemExt in
        let idx = Imm(0,Idx) in
        let smstart = [I_SMSTART (None)] in
        let rB,st = next_zreg st in
        let dup_sv = [I_DUP_SV (rB,V32,vdep)] in
        let r,st = next_zreg st in
        let mov_sv = [I_MOV_SV (r,v,S_NOEXT)] in
        let add_sv = [I_ADD_SV (r,r,rB)] in
        let tile,slice,st = next_zaslice n st in
        let pred,st = next_preg st in
        let nelem = SIMD.nelements n in
        let ptrue = [I_PTRUE (pred,pattern nelem)] in
        let movx12 = [I_MOV (V32,x12,RV (V32,ZR))] in
        let mova = [I_MOVA_VT (tile,x12,slice,with_mode Merge pred,r)] in
        let setup = dup_sv@mov_sv@add_sv@ptrue@movx12@mova in
        let store = [I_ST1SPT (VSIMD32,tile,x12,slice,pred,rA,idx)] in
        let smstop = [I_SMSTOP (None)] in
        init,lift_code (smstart@setup@store@smstop),st
    end

    module STG = struct

      let emit_store_reg st p init x rA =
        let rB,init,st = U.next_init st p init x in
        init,pseudo [stg rA rB],st

      let emit_store st p init e =
        let loc = Code.as_data e.C.loc in
        let x = Code.add_tag loc e.C.tag
        and v = Code.add_tag loc (Code.value_to_int e.C.v) in
        let rA,init,st = U.next_init st p init v in
        emit_store_reg st p init x rA

      let emit_store_idx vaddr st p init e idx =
        let loc = Code.as_data e.C.loc in
        let x = Code.add_tag loc e.C.tag
        and v = Code.add_tag loc (Code.value_to_int e.C.v) in
        let rA,init,st = U.next_init st p init v in
        let rB,init,st = U.next_init st p init x in
        let rC,c,st = do_sum_addr vaddr st rB idx in
        init,pseudo (c@[stg rA rC]),st

    end

    module STCT = struct
      let emit_store_reg st p init x rA =
        let rB,init,st = U.next_init st p init x in
        init,pseudo [stct rA rB],st

      let emit_store st p init x v =
        if v > 1 then Warn.fatal "Capability tags can't be incremented above 1";
        let rA,init,csi,st = U.emit_mov st p init v in
        let init,cs,st = emit_store_reg st p init x rA in
        init,csi@cs,st

      let emit_store_idx st p init x idx v =
        if v > 1 then Warn.fatal "Capability tags can't be incremented above 1";
        let rA,init,csi,st = U.emit_mov st p init v in
        let rB,init,st = U.next_init st p init x in
        let rC,st = next_reg st in
        init,csi@pseudo ([sctag rC rB idx; stct rA rC]),st
    end

    module STLR =
      STORE
        (struct
          let store = wrap_st stlr
          let store_idx st rA rB idx =
            let r,ins,st = sum_addr st rB idx in
            ins@[stlr rA r],st
          let emit_mov = U.emit_mov
        end)

(***************************)
(* Atomic loads and stores *)
(***************************)

    let get_xload_addon (a,_m) r1 = match a with
      | Plain a
      | Acq a -> emit_ldr_addon a r1
      | _ -> []

    let get_xload = function
      | (Plain None,None) ->ldxr
      | (Plain Some Capability,None) -> ldxr_sz XX MachSize.S128
      | (Plain None,Some (sz,_)) -> ldxr_sz XX sz
      | (Acq None,None)   -> ldaxr
      | (Acq Some Capability,None) -> ldxr_sz AX MachSize.S128
      | (Acq None,Some (sz,_)) -> ldxr_sz AX sz
      | (AcqPc _,_) -> Warn.fatal "AcqPC annotation on xload"
      | (Tag,_)|(CapaTag,_)|(CapaSeal,_) -> Warn.fatal "variant annotation on xload"
      | a ->
         Warn.fatal "Bad annotation for Lx: %s\n" (A.pp_atom a)


    and get_xstore = function
      | (Plain None,None) -> stxr
      | (Plain Some Capability,None) -> stxr_sz YY MachSize.S128
      | (Plain None,Some (sz,_)) -> stxr_sz YY sz
      | (Rel None,None) -> stlxr
      | (Rel Some Capability,None) -> stxr_sz LY MachSize.S128
      | (Rel None,Some (sz,_)) -> stxr_sz LY sz
      | (Tag,_)|(CapaTag,_)|(CapaSeal,_) -> Warn.fatal "variant annotation on xstore"
      | a ->
         Warn.fatal "Bad annotation for Sx: %s\n" (A.pp_atom a)

    let get_xstore_addon (a,_m) r2 r3 e init st p = match a with
    | Plain a
    | Rel a -> emit_str_addon st p init r2 r3 a e
    | _ -> init,[],st

    let get_rmw_addrs arw st rA = match arw with
    | (_,(None|Some (_,0))),(_,(None|Some (_,0)))
      -> rA,rA,[],st
    | (_,Some (_,o1)),(_,Some (_,o2)) when o1=o2 ->
        let r,cs,st = sumi_addr st rA o1 in
        r,r,cs,st
    |  (_,(None|Some (_,0))),(_,Some (_,o)) ->
        let  r,cs,st = sumi_addr st rA o in
        rA,r,cs,st
    |  (_,Some (_,o)),(_,(None|Some (_,0))) ->
        let  r,cs,st = sumi_addr st rA o in
        r,rA,cs,st
    | (_,Some (_,o1)),(_,Some (_,o2)) ->
        let  r1,cs1,st = sumi_addr_gen tempo1 st rA o1 in
        let  r2,cs2,st = sumi_addr_gen tempo2 st rA o2 in
        r1,r2,cs1@cs2,st

    module
      ExclusivePair
        (Load:
           sig

             type load_regs
             val load : A.atom -> load_regs -> A.reg -> A.ins list
             val load_addon : A.atom -> load_regs -> A.ins list
           end)
        (Store:
         sig
           type store_regs
           val store : A.atom -> A.reg -> store_regs -> A.reg -> A.ins list
           val store_addon :
             A.atom -> store_regs -> A.reg ->
                 C.event -> A.init -> A.st -> Proc.t ->
                   A.init * A.pseudo list * A.st
         end) =
      struct

        let emit_xpair ar rR rAR aw r rW rAW =
          let cs_load = Load.load ar rR rAR
          and cs_store = Store.store aw r rW rAW in
          cs_load@Load.load_addon ar rR@cs_store

        let emit_loop_pair (ar,aw as arw) p st init rR rW rA e =
          let rAR,rAW,cs0,st = get_rmw_addrs arw st rA in
          let init,cs1,st = Store.store_addon ar rW rAW e init st p in
          let lbl = Label.next_label "Loop" in
          let r,st = tempo3 st in
          let cs =
            Label (lbl,Nop)
            ::lift_code (emit_xpair ar rR rAR aw r rW rAW)
            @[Instruction (cbnz r lbl)] in
          init,pseudo cs0@cs1@cs,st

        let emit_one_pair (ar, aw) p st init r rR rW rAR rAW k e =
          let ok,st = A.ok_reg st in
          let init,cs1,st = Store.store_addon ar rW rAW e init st p in
          init,
          cs1
          @lift_code (emit_xpair ar rR rAR aw r rW rAW)
          @[Instruction (cbnz r (Label.last p))]
          @(k (Instruction (incr ok))),
          A.next_ok st

        let emit_unroll_pair u (ar, aw as arw) p st init rR rW rA e =
          let rAR,rAW,cs0,st = get_rmw_addrs arw st rA in
          let cs0 = pseudo cs0 in
          if u <= 0 then
            let r,st = next_reg st in
            let init,cs1,st = Store.store_addon ar rW rA e init st p in
            init,
            cs0@cs1
            @pseudo  (emit_xpair ar rR rAR aw r rW rAW),
            st
          else if u = 1 then
            let r,st = tempo3 st in
            let init,cs,st =
              emit_one_pair arw p st init r rR rW rAR rAW (fun i -> [i]) e in
            init,cs0@cs,st
          else
            let r,st = tempo3 st in
            let init,cs1,st = Store.store_addon ar rW rA e init st p in
            let out = Label.next_label "Go" in
            let rec do_rec = function
              | 1 ->
                 emit_one_pair
                   arw p st init r rR rW rAR rAW
                   (fun i ->  [Label (out,Nop);i]) e
              | u ->
                 let init,cs,st = do_rec (u-1) in
                 init,
                 pseudo (emit_xpair ar rR rAR aw r rW rAW)
                 @(Instruction (cbz r out)::cs1@cs),
                 st in
            let init,cs,st = do_rec u in
            init,cs0@cs,st

        let emit_pair = match Cfg.unrollatomic with
          | None -> emit_loop_pair
          | Some u -> emit_unroll_pair u

      end

    module XLoad = struct
      type load_regs = A.reg
      let  load ar rR rAR = [get_xload ar rR rAR]
      let load_addon = get_xload_addon
    end

    module XStore = struct
      type store_regs = A.reg
      let store  aw r rW rAW = [get_xstore aw r rW rAW]
      let store_addon = get_xstore_addon
    end

    module XSingle = ExclusivePair(XLoad)(XStore)


    module XLoadPair = struct
      type load_regs  = A.reg * A.reg
      let load ar (r1,r2) rA =
        let a =
          match ar with
          | Pair (Pa,_),None -> XP
          | Pair (PaI,_),None -> AXP
          | _ ->
             Warn.fatal
               "Illegal %s annotaton on load exclusive pair" (pp_atom ar)  in
        [do_ldxp a r1 r2 rA; add vloc r1 r2 r1;]
      let load_addon _ _ =
        assert (not (do_morello)); []
    end

    module XStorePair = struct
      type store_regs =  A.reg * A.reg
      let store aw r (r1,r2) rA =
        let a =
          match aw with
          | Pair (Pa,_),_ -> YY
          | Pair (PaI,_),_ -> LY
          | _ ->
             Warn.fatal
               "Illegal %s annotaton on store exclusive pair" (pp_atom aw)  in
        [dec r2 r1; do_stxp a r r2 r1 rA;]
      let store_addon _ _ _ _ init st _ =
        assert (not (do_morello)); init,[],st
    end

    module XPair = ExclusivePair(XLoadPair)(XStorePair)

(* Translate annotations *)

    let tr_rw = function
      | PP -> (Plain None,None),(Plain None,None)
      | PL -> (Plain None,None),(Rel None,None)
      | AP -> (Acq None,None),(Plain None,None)
      | AL -> (Acq None,None),(Rel None,None)

    let tr_none = function
      | None -> Plain None,None
      | Some p -> p


(********************)
(* Mixed size pairs *)
(********************)

    let emit_pair_mixed sz o rw =
      let arw = match tr_rw rw with
      | (a1,_),(a2,_) -> (a1,Some (sz,o)),(a2,Some (sz,o)) in
      XSingle.emit_pair arw

(********************************)
(* Individual loads and strores *)
(********************************)

    let emit_lda_reg rw st init p rA =
      let rR,st = next_reg st in
      let _,cs,st = XSingle.emit_pair rw p st init rR rR rA C.evt_null in
      rR,cs,st

    let emit_lda rw st p init loc =
      let rA,init,st = U.next_init st p init loc in
      let r,cs,st =  emit_lda_reg rw st init p rA in
      r,init,cs,st

    let do_emit_lda_idx v rw st p init loc idx =
      let rA,init,st = U.next_init st p init loc in
      let rA,cs1,st = do_sum_addr v st rA idx in
      let r,cs2,st =  emit_lda_reg rw st init p rA in
      r,init,pseudo cs1@cs2,st

    let emit_lda_mixed_reg sz o rw st p init rA =
      let rR,st = next_reg_sz st p sz in
      let _,cs,st = emit_pair_mixed sz o  rw p st init rR rR rA C.evt_null in
      rR,cs,st

    let emit_lda_mixed sz o rw st p init loc =
      let rA,init,st = U.next_init st p init loc in
      let r,cs,st =  emit_lda_mixed_reg sz o rw st p init rA in
      r,init,cs,st

    let do_emit_lda_mixed_idx v sz o rw st p init loc idx =
      let rA,init,st = U.next_init st p init loc in
      let rA,cs1,st = do_sum_addr v st rA idx in
      let r,cs2,st =  emit_lda_mixed_reg sz o rw st p init rA in
      r,init,pseudo cs1@cs2,st

    let do_emit_sta rw st p init rW rA =
      let rR,st = next_reg st in
      let init,cs,st = XSingle.emit_pair rw p st init rR rW rA C.evt_null in
      rR,init,cs,st

    let emit_sta rw st p init loc v =
      let rA,init,st = U.next_init st p init loc in
      let rW,init,csi,st = U.emit_mov st p init v in
      let rR,init,cs,st = do_emit_sta rw st p init rW rA in
      rR,init,csi@cs,st

    let emit_sta_reg rw st p init loc rW =
      let rA,init,st = U.next_init st p init loc in
      let rR,init,cs,st = do_emit_sta rw st p init rW rA in
      rR,init,cs,st

    let emit_sta_idx rw st p init loc idx v =
      let rA,init,st = U.next_init st p init loc in
      let rA,cs1,st = sum_addr st rA idx in
      let rW,init,csi,st = U.emit_mov st p init v in
      let rR,init,cs2,st = do_emit_sta rw st p init rW rA in
      rR,init,csi@pseudo cs1@cs2,st

    let do_emit_sta_mixed sz o rw st p init rW rA =
      let rR,st = next_reg st in
      let init,cs,st = emit_pair_mixed sz o rw p st init rR rW rA C.evt_null in
      rR,init,cs,st


    let emit_sta_mixed sz o rw st p init loc v =
      let rA,init,st = U.next_init st p init loc in
      let rW,init,csi,st = U.emit_mov st p init v in
      let rR,init,cs,st = do_emit_sta_mixed sz o rw st p init rW rA in
      rR,init,csi@cs,st

    let emit_sta_mixed_reg sz o rw st p init loc rW =
      let rA,init,st = U.next_init st p init loc in
      let rR,init,cs,st = do_emit_sta_mixed sz o rw st p init rW rA in
      rR,init,cs,st

    let emit_sta_mixed_idx sz o rw st p init loc idx v =
      let rA,init,st = U.next_init st p init loc in
      let rA,cs1,st = sum_addr st rA idx in
      let rW,init,csi,st = U.emit_mov st p init v in
      let rR,init,cs2,st = do_emit_sta_mixed sz o rw st p init rW rA in
      rR,init,csi@pseudo cs1@cs2,st

    let do_emit_set_pteval rel st p init v rA =
      let rB,init,st = U.emit_pteval st p init v in
      let do_str = if rel then do_stlr else do_str in
      init,pseudo [do_str A64.V64 rB rA],st

    let emit_set_pteval rel st p init v loc =
      let rA,init,st = U.next_init st p init loc in
      do_emit_set_pteval rel st p init v rA

    let emit_set_pteval_idx rel vdep idx st p init v loc =
      let rA,init,st = U.next_init st p init loc in
      let rA,cs1,st = do_sum_addr vdep st rA idx in
      let init,cs2,st = do_emit_set_pteval rel st p init v rA in
      init,pseudo cs1@cs2,st

    let emit_set_pteval_reg rel st p init rB loc =
      let rA,init,st = U.next_init st p init loc in
      let do_str = if rel then do_stlr else do_str in
      init,pseudo [do_str A64.V64 rB rA],st

(********)
(* Pair *)
(********)

    let emit_ldp_reg opt st init rA =
      let r1,r2,st = next_reg2 st in
      r1,init,pseudo [do_ldp opt r1 r2 rA;add vloc r1 r1 r2;],st

    let emit_ldp_reg opt idx st _p init rA =
      match opt,idx with
      |  _,Both ->
          emit_ldp_reg opt st init rA

    let emit_ldp opt idx st p init loc =
      let rA,init,st =  U.next_init st p init loc in
      emit_ldp_reg opt idx st p init rA

    let emit_ldp_idx_var opt idx vdep st p init loc ridx =
      let rA,init,st =  U.next_init st p init loc in
      let rA,csA,st = do_sum_addr vdep st rA ridx in
      let r,init,cs,st = emit_ldp_reg opt idx st p init rA in
      r,init,pseudo csA@cs,st

    let do_emit_stp_reg opt st p init v rA =
      let r1,init,csi1,st = U.emit_mov st p init v in
      let r2,st = next_reg st in
      init,csi1@pseudo [dec r2 r1; do_stp opt r2 r1 rA;],st

    let emit_stp_reg opt idx st p init rA e =
      match opt,idx with
      | _,Both ->
      do_emit_stp_reg opt st p init (Code.value_to_int e.C.v) rA

    let emit_stp opt idx st p init loc e =
      let rA,init,st =  U.next_init st p init loc in
      emit_stp_reg opt idx st p init rA e

    let emit_stp_idx_var opt idx vdep st p init loc e ridx =
      let rA,init,st =  U.next_init st p init loc in
      let rA,csA,st = do_sum_addr vdep st rA ridx in
      let init,cs,st = emit_stp_reg opt idx st p init rA e in
      init,pseudo csA@cs,st

    let stp_emit_store_reg opt idx st p init loc r1 =
      let rA,init,st =  U.next_init st p init loc in
      let r2,st = next_reg st in
      match idx with
      | Both ->
         init,pseudo [dec r2 r1; do_stp opt r2 r1 rA;],st

(**************)
(* For export *)
(**************)

    let emit_load_one = LDR.emit_load_one
    let emit_load = LDR.emit_load


    let emit_obs t = match t with
    | Code.Ord | Code.Instr-> emit_load_mixed naturalsize 0
    | Code.Pte->
        fun st p init loc ->
        let r,init,cs,st = LDR.emit_load_var A64.V64 st p init (Misc.add_pte loc) in
        r,init,cs,st
    | Code.Tag -> LDG.emit_load
    | Code.CapaTag -> LDCT.emit_load
    | Code.CapaSeal -> fun st p init x ->
      let r,init,cs,st = emit_load_mixed MachSize.S128 0 st p init x in
      let cs2 = lift_code [gctype r r] in
      r,init,cs@cs2,st
    | Code.VecReg n ->
       let emit_load = match n with
         | SIMD.NeAcqPc -> LDAPUR.emit_load
         | SIMD.NeP -> LDUR.emit_load
         | SIMD.NePa  -> LDP.emit_load A64.TT
         | SIMD.NePaN -> LDP.emit_load A64.NT
         | SIMD.Sv1 | SIMD.Sv2i | SIMD.Sv3i | SIMD.Sv4i-> LDNW.emit_load n
         | SIMD.SvV -> LD1G.emit_load n
         | SIMD.SmV | SIMD.SmH -> LD1T.emit_load n
         | _ -> LDN.emit_load n
       in
       emit_load
    | Code.Pair -> emit_ldp Pa Both


    let emit_obs_not_value = OBS.emit_load_not_value
    let emit_obs_not_eq = OBS.emit_load_not_eq
    let emit_obs_not_zero = OBS.emit_load_not_zero


(**********)
(* Access *)
(**********)

    let add_tag =
      if do_memtag then
        fun loc tag -> Code.add_tag loc tag
      else if do_morello then
        fun loc _ -> Code.add_capability loc 0
      else fun loc _ -> loc

    let get_tagged_loc e = add_tag (as_data e.C.loc) e.C.tag

    let add_label_to_last_instructions e cs =
      match e.C.check_fault with
      | Some (label_name, _) ->
        let rec do_rec = function
          | [] -> assert false (* the `cs` should not be empty *)
          | [instr] -> [Label(label_name, instr)]
          | [instr;branch_instr] when is_branch branch_instr ->
              [Label(label_name, instr);branch_instr]
          | instr::rem -> instr::do_rec rem in
        do_rec cs
      | None -> cs

    (* If there is a fault label in `e`, add the `index`-th label
       to the first instruction in `cs` *)
    let add_label_to_first_instructions e cs =
      match e.C.check_fault with
      | Some (label_name, _) ->
        (* find the first non-label instruction *)
        let rec do_rec cs = match cs with
          | [] -> assert false (* the `cs` should not be empty *)
          | instr::rem -> begin match instr with
            (* skip label or instruction that is already labelled *)
            | Label(_) -> instr::do_rec rem
            |_ -> Label(label_name, instr)::rem
        end in
        do_rec cs
      | None -> cs

    let emit_access st p init e =
    (* collapse the value `v` in event `e` to integer *)
    let value = Code.value_to_int e.C.v in
    match e.C.dir,e.C.loc with
    | None,_ -> Warn.fatal "AArchCompile.emit_access"
    | Some d,Code lab ->
        begin match d,e.C.atom with
        | R,Some (Instr, None) ->
            let r,init,cs,st = LDR.emit_fetch st p init lab in
            Some r,init,cs,st
        (* Plain read from an instruction label is currently not supported,
           but will be implemented in a future patch
        | R, None ->
            let r,init,cs,st = LDR.emit_load st p init lab in
            Some r,init,cs,st
        | W, Some(Instr, None)  *)
        | W, None ->
            let init,cs,st = STR.emit_store_nop st p init lab in
            None,init,cs,st
        | _,_ -> Warn.fatal "Not Yet (%s,%s)!!!"
              (pp_dir d) (C.debug_evt e)
        end
    | Some d,Data loc ->
        let loc = add_tag loc e.C.tag in
        let atom = match e.C.atom with
        | None -> None
        | Some (a,m) -> begin match a with
          | Plain Some Capability
          | Acq Some Capability
          | AcqPc Some Capability
          | Rel Some Capability ->
            assert (Misc.is_none m) ;
            Some (a,Some (MachSize.S128,0))
          | _ -> Some (a,m) end in
        let regs,inits,cs,st = begin match d,atom with
        | R,None ->
            let r,init,cs,st = LDR.emit_load st p init loc in
            Some r,init,cs,st
        | R,Some (Acq _,None) ->
            let r,init,cs,st = LDAR.emit_load st p init loc in
            Some r,init,cs,st
        | R,Some (Acq a,Some (sz,o)) ->
            let module L =
              LOAD
                (struct
                  type sz = MachSize.sz
                  let sz0 = sz
                  let load sz = ldar_mixed AA sz o
                  let load_idx sz _ = ldar_mixed_idx AA sz o
                  let next_reg = next_reg_sz
                end) in
            let r,init,cs,st = L.emit_load st p init loc in
            let cs2 = emit_ldr_addon a r in
            Some r,init,cs@pseudo cs2,st
        | R,Some (AcqPc _,None) ->
            let r,init,cs,st = LDAPR.emit_load st p init loc in
            Some r,init,cs,st
        | R,Some (AcqPc a,Some (sz,o)) ->
            let module L =
              LOAD
                (struct
                  type sz = MachSize.sz
                  let sz0 = sz
                  let load sz = ldar_mixed AQ sz o
                  let load_idx sz _ = ldar_mixed_idx AQ sz o
                  let next_reg = next_reg_sz
                end) in
            let r,init,cs,st = L.emit_load st p init loc in
            let cs2 = emit_ldr_addon a r in
            Some r,init,cs@pseudo cs2,st
        | R,Some (Rel _,_) ->
            Warn.fatal "No load release"
        | R,Some (Atomic rw,None) ->
            let r,init,cs,st = emit_lda (tr_rw rw) st p init loc  in
            Some r,init,cs,st
        | R,Some (Atomic rw,Some (sz,o)) ->
            let r,init,cs,st = emit_lda_mixed sz o rw st p init loc  in
            Some r,init,cs,st
        | R,Some (Plain a,Some (sz,o)) ->
            let r,init,cs,st = emit_load_mixed sz o st p init loc in
            let cs2 = emit_ldr_addon a r in
            Some r,init,cs@pseudo cs2,st
        | R,Some (Tag,None) ->
            let r,init,cs,st = LDG.emit_load st p init loc  in
            Some r,init,cs,st
        | R,Some (CapaTag,None) ->
            let r,init,cs,st = LDCT.emit_load st p init loc in
            Some r,init,cs,st
        | R,Some (CapaTag,Some _) -> assert false
        | R,Some (CapaSeal,None) ->
            let r,init,cs,st = emit_load_mixed MachSize.S128 0 st p init loc in
            Some r,init,cs@lift_code [gctype r r],st
        | R,Some (CapaSeal,Some _) -> assert false
        | R,Some (Neon n, None) ->
           let emit_load = match n with
             | SIMD.NeRel -> Warn.fatal "No laod release"
             | SIMD.NeAcqPc -> LDAPUR.emit_load
             | SIMD.NeP -> LDUR.emit_load
             | SIMD.NePa  -> LDP.emit_load A64.TT
             | SIMD.NePaN -> LDP.emit_load A64.NT
             | SIMD.Sv1 | SIMD.Sv2i | SIMD.Sv3i | SIMD.Sv4i -> LDNW.emit_load n
             | SIMD.SvV -> LD1G.emit_load n
             | SIMD.SmV | SIMD.SmH -> LD1T.emit_load n
             | _ -> LDN.emit_load n
           in
           let r,init,cs,st = emit_load st p init loc in
            Some r,init,cs,st
        | R,Some (Neon _,Some _) -> assert false
        | R,Some (Pair (opt,idx),None) ->
          let r,init,cs,st = emit_ldp opt idx st p init loc in
          Some r,init,cs,st
        | R,Some (Pair _,Some _) -> assert false
        | W,None ->
            let init,cs,st =
              STR.emit_store st p init loc value None C.evt_null in
            None,init,cs,st
        | W,Some (Rel _,None) ->
            let init,cs,st =
              STLR.emit_store st p init loc value None C.evt_null in
            None,init,cs,st
        | W,Some (Acq _,_) -> Warn.fatal "No store acquire"
        | W,Some (AcqPc _,_) -> Warn.fatal "No store acquirePc"
        | W,Some (Atomic rw,None) ->
            let r,init,cs,st = emit_sta (tr_rw rw) st p init loc value in
            Some r,init,cs,st
        | W,Some (Atomic rw,Some (sz,o)) ->
            let r,init,cs,st = emit_sta_mixed sz o rw st p init loc value in
            Some r,init,cs,st
        | W,Some (Plain a,Some (sz,o)) ->
            let init,cs,st = emit_store_mixed sz o st p init loc value a e in
            None,init,cs,st
        | W,Some (Rel a,Some (sz,o)) ->
            let module S =
              STORE
                (struct
                  let store = stlr_mixed sz o
                  let store_idx st r1 r2 idx =
                    let cs,st = stlr_mixed_idx sz st r1 r2 idx in
                    let cs = match o with
                    | 0 -> cs
                    | _ -> addi idx idx o::cs in
                    cs,st
                  let emit_mov = emit_mov_sz sz
                end) in
            let init,cs,st = S.emit_store st p init loc value a e in
            None,init,cs,st
        | W,Some (Tag,None) ->
            let init,cs,st = STG.emit_store st p init e in
            None,init,cs,st
        | W,Some (Pair (opt,idx),None) ->
            let init,cs,st = emit_stp opt idx st p init loc e in
            None,init,cs,st
        | W,Some (Pair _,Some _) -> assert false
        | (R|W), Some (Instr, _) -> Warn.fatal "Instr annotation did not create code location %s" (C.debug_evt e)
        | R,Some (Pte (Read|ReadAcq|ReadAcqPc as rk),None) ->
            let emit = match rk with
            | Read -> LDR.emit_load_var
            | ReadAcq -> LDAR.emit_load_var
            | ReadAcqPc -> LDAPR.emit_load_var
            | _ -> assert false in
            let r,init,cs,st = emit A64.V64 st p init (Misc.add_pte loc) in
            Some r,init,cs,st
        | W,Some (Pte (Set _),None) ->
            let init,cs,st =
              emit_set_pteval false st p init e.C.pte (Misc.add_pte loc) in
            None,init,cs,st
        | W,Some (Pte (SetRel _),None) ->
            let init,cs,st =
              emit_set_pteval true st p init e.C.pte (Misc.add_pte loc) in
            None,init,cs,st
        | d,Some (Pte _,_ as a) ->
            Warn.fatal
              "Atom %s does not apply to direction %s"
              (A.pp_atom a) (Code.pp_dir d)
        | _,Some (Plain _,None) -> assert false
        | _,Some (Tag,_) -> assert false
        | W,Some (CapaTag,None) ->
            let init,cs,st = STCT.emit_store st p init loc value in
            None,init,cs,st
        | W,Some (CapaTag,Some _) -> assert false
        | W,Some (CapaSeal,None) ->
            let rA,init,st = U.next_init st p init loc in
            let rB,init,csi,st = U.emit_mov st p init e.C.ord in
            let init,cs,st =
              emit_str_addon
                st p init rB rA (Some Capability) {e with C.cseal = value} in
            None,init,csi@cs@lift_code [str_mixed MachSize.S128 0 rB rA],st
        | W,Some (CapaSeal,Some _) -> assert false
        | W,Some (Neon n, None) ->
           let emit_store = match n with
             | SIMD.NeAcqPc -> Warn.fatal "No store acquirePc"
             | SIMD.NeRel -> STLUR.emit_store
             | SIMD.NeP -> STUR.emit_store
             | SIMD.NePa  -> STP.emit_store A64.TT
             | SIMD.NePaN -> STP.emit_store A64.NT
             | SIMD.Sv1 | SIMD.Sv2i | SIMD.Sv3i | SIMD.Sv4i-> STNW.emit_store n
             | SIMD.SvV -> ST1S.emit_store n
             | SIMD.SmV | SIMD.SmH -> ST1T.emit_store n
             | _ -> STN.emit_store n
           in
           let init,cs,st = emit_store st p init loc value in
           None,init,cs,st
        | W,Some (Neon _,Some _) -> assert false
        end in
        (* Add a label to instructions `cs`, when a fault check is required. *)
          (* Add a label to instructions `cs`, when a fault check is required. *)
        let cs = add_label_to_last_instructions e cs in
        regs,inits,cs,st
    (* END of emit_access *)

    let same_sz sz1 sz2 = match sz1,sz2 with
      | None,None -> true
      | Some s1,Some s2 ->  MachMixed.equal s1 s2
      | (None,Some _)|(Some _,None) -> false

    let check_cu b =
      if b then
        Warn.fatal
          "Refuse to generate constrained unpredictable, use -variant CU to accept"

    let check_arw_lxsx er ew =
      let _,szr as ar = tr_none er.C.atom
      and _,szw as aw = tr_none ew.C.atom in
      check_cu (not (A64.do_cu || same_sz szr szw)) ;
      ar,aw

    let emit_addr_simple  st p init er =
      let rA,init,st = U.next_init st p init (get_tagged_loc er) in
      rA,init,[],st

    let do_emit_exch1 emit_addr st p init er ew =
      let rA,init,caddr,st = emit_addr st p init er in
      let rR,st = next_reg st in
      let rW,init,csi,st = U.emit_mov st p init (Code.value_to_int ew.C.v) in
      let arw = check_arw_lxsx er ew in
      let init,cs,st = XSingle.emit_pair arw p st init rR rW rA ew in
      let cs = add_label_to_first_instructions er cs in
      rR,init,csi@caddr@cs,st

    let emit_exch1 = do_emit_exch1 emit_addr_simple

    let do_emit_exch22 emit_addr st p init er ew =
      let rA,init,caddr,st = emit_addr st p init er in
      let rR1,rR2,st = next_reg2 st in
      let rW1,init,csi,st = U.emit_mov st p init (Code.value_to_int ew.C.v) in
      let rW2,st = next_reg st in
      let arw = check_arw_lxsx er ew in
      let init,cs,st =
        XPair.emit_pair arw p st init (rR1,rR2) (rW1,rW2) rA ew in
      let cs = add_label_to_first_instructions er cs in
      rR1,init,csi@caddr@cs,st

    let emit_exch22 = do_emit_exch22 emit_addr_simple

    let do_emit_exch21 emit_addr st p init er ew =
      let rA,init,caddr,st = emit_addr st p init er in
      let rR1,rR2,st = next_reg2 st in
      let rW,init,csi,st = U.emit_mov st p init (Code.value_to_int ew.C.v) in
      let arw = check_arw_lxsx er ew in
      let module X = ExclusivePair(XLoadPair)(XStore) in
      let init,cs,st =
        X.emit_pair arw p st init (rR1,rR2) rW rA ew in
      let cs = add_label_to_first_instructions er cs in
      rR1,init,csi@caddr@cs,st

    let emit_exch21 = do_emit_exch21 emit_addr_simple

    let do_emit_exch12 emit_addr st p init er ew =
      let rA,init,caddr,st = emit_addr st p init er in
      let rR,st = next_reg st in
      let rW1,init,csi,st = U.emit_mov st p init (Code.value_to_int ew.C.v) in
      let rW2,st = next_reg st in
      let arw = check_arw_lxsx er ew in
      let module X = ExclusivePair(XLoad)(XStorePair) in
      let init,cs,st =
        X.emit_pair arw p st init rR (rW1,rW2) rA ew in
      let cs = add_label_to_first_instructions er cs in
      rR,init,csi@caddr@cs,st

    let emit_exch12 = do_emit_exch12 emit_addr_simple

    let emit_exch st p init er ew =
      let ar,_ = tr_none er.C.atom
      and aw,_ = tr_none ew.C.atom in
      match ar,aw with
      | (Pair _,Pair _) ->
         emit_exch22 st p init er ew
      | (Pair _,_) ->
         check_cu (not A64.do_cu) ;
         emit_exch21 st p init er ew
      | (_,Pair _) ->
         check_cu (not A64.do_cu) ;
         emit_exch12 st p init er ew
      | _,_ ->
         emit_exch1 st p init er ew

    let do_sz sz1 sz2 =
      if same_sz sz1 sz2 then sz1
      else
        Warn.fatal "Amo instructions with different sizes or offsets"

    let do_rmw_type a1 a2 = match a1,a2 with
    | Plain o1,Plain o2 when o1 = o2 -> RMW_P,o1
    | Acq o1,Plain o2 when o1 = o2   -> RMW_A,o1
    | Plain o1,Rel o2 when o1 = o2   -> RMW_L,o1
    | Acq o1,Rel o2 when o1 = o2     -> RMW_AL,o1
    | _,_ ->
        Warn.fatal "Bad annotation for Amo: R=%s, W=%s"
          (pp_atom_acc a1) (pp_atom_acc a2)

    let do_rmw_annot (ar,szr) (aw,szw) =
      let sz =  do_sz szr szw in
      let a,opt = do_rmw_type ar aw in
      sz,a,opt

    let mk_emit_mov sz = match sz with
    | None ->  U.emit_mov
    | Some (sz,_) ->  emit_mov_sz sz

    let mk_emit_mov_fresh sz = match sz with
    | None ->  U.emit_mov_fresh
    | Some (sz,_) ->  emit_mov_sz_fresh sz

    let do_emit_ldop_rA  ins ins_mixed st p init er ew rA =
      assert (er.C.ctag = ew.C.ctag && er.C.cseal = ew.C.cseal) ;
      let sz,a,opt = do_rmw_annot (tr_none er.C.atom) (tr_none ew.C.atom) in
      let rR,st = next_reg st in
      let rW,init,csi,st = mk_emit_mov sz st p init (Code.value_to_int ew.C.v) in
      let sz = match opt with
      | None -> sz
      | Some Capability -> assert (Misc.is_none sz) ; Some (MachSize.S128, 0) in
      let init,csi2,st = emit_str_addon st p init rW rA opt ew in
      let cs,st = match sz with
      | None -> [ins a rW rR rA],st
      | Some (sz,o) ->
          let rA,cs,st = sumi_addr st rA o in
          cs@[ins_mixed sz a rW rR rA],st in
      let cs = add_label_to_last_instructions er (pseudo cs) in
      let cs2 = emit_ldr_addon opt rR in
      rR,init,(csi@csi2@cs@pseudo cs2),st

    let do_emit_ldop ins ins_mixed st p init er ew =
      let rA,init,st =
        U.next_init st p init (get_tagged_loc er) in
      do_emit_ldop_rA ins ins_mixed st p init er ew rA

    let emit_swp =  do_emit_ldop swp swp_mixed
    and emit_ldop op = do_emit_ldop (ldop op) (ldop_mixed op)

    let emit_cas_rA st p init er ew rA =
      assert (er.C.ctag = ew.C.ctag && er.C.cseal = ew.C.cseal) ;
      let sz,a,opt = do_rmw_annot (tr_none er.C.atom) (tr_none ew.C.atom) in
      let rS,init,csS,st = mk_emit_mov_fresh sz st p init (Code.value_to_int er.C.v) in
      let rT,init,csT,st = mk_emit_mov sz st p init (Code.value_to_int ew.C.v) in
      let sz = match opt with
      | None -> sz
      | Some Capability -> assert (Misc.is_none sz) ; Some (MachSize.S128, 0) in
      let init,csS2,st = emit_str_addon st p init rS rA opt er in
      let init,csT2,st = emit_str_addon st p init rT rA opt ew in
      let cs,st = match sz with
      | None -> [cas a rS rT rA],st
      | Some (sz,o) ->
          let rA,cs,st = sumi_addr st rA o in
          cs@[cas_mixed sz a rS rT rA],st in
      let cs = add_label_to_last_instructions er (pseudo cs) in
      let cs2 = emit_ldr_addon opt rS in
      rS,init,csS@csS2@csT@csT2@cs@pseudo cs2,st

    let emit_cas  st p init er ew =
      let rA,init,st =
        U.next_init st p init (get_tagged_loc er) in
      emit_cas_rA st p init er ew rA

    let emit_stop_rA op st p init er ew rA =
      let a,sz1 = tr_none ew.C.atom
      and b,sz2 = tr_none er.C.atom in
      let sz = do_sz sz1 sz2 in
      let a = match b,a with
      | Plain _,Plain _-> W_P
      | Plain _,Rel _ -> W_L
      | _ ->
          Warn.fatal "Unexpected atoms in STOP instruction: %s,%s"
            (pp_atom_acc b)  (pp_atom_acc a) in
      let rW,init,csi,st = mk_emit_mov sz st p init (Code.value_to_int ew.C.v) in
      let cs,st = match sz with
      | None -> [stop op a rW rA],st
      | Some (sz,o) ->
          let rA,cs,st = sumi_addr st rA o in
          cs@[stop_mixed op sz a rW rA],st in
      let cs = add_label_to_last_instructions er (pseudo cs) in
      None,init,csi@cs,st

    let emit_stop  op st p init er ew =
      let rA,init,st =
        U.next_init st p init (get_tagged_loc er) in
      emit_stop_rA op st p init er ew rA

    let map_some f st p init er ew =
      let r,init,cs,st = f  st p init er ew in
      Some r,init,cs,st

    let emit_rmw rmw = match rmw with
    | LrSc -> map_some emit_exch
    | Swp -> map_some emit_swp
    | Cas -> map_some emit_cas
    | LdOp op -> map_some (emit_ldop op)
    | StOp op -> emit_stop op

(* Fences *)
    let emit_cachesync s isb r =
      pseudo
        (I_DC ((match s with Strong -> DC.civac | Weak -> DC.cvau),r)::
         I_FENCE (DSB (ISH,FULL))::
         I_IC (IC.ivau,r)::
         I_FENCE (DSB (ISH,FULL))::
         (if isb then [I_FENCE ISB] else []))

    let emit_shootdown dom op sync r =
      match sync with
      | Sync ->
         pseudo
           (I_FENCE (DSB(dom,FULL))::
            I_TLBI(op,r)::I_FENCE (DSB(dom,FULL))::[])
      | NoSync ->
         pseudo (I_TLBI(op,r)::[])

    let emit_CMO t r = match t with
      | DC_CVAU -> pseudo ([I_DC (DC.cvau, r)])
      | IC_IVAU -> pseudo ([I_IC (IC.ivau, r)])

    let emit_fence st p init n f = match f with
    | Barrier f -> init,[Instruction (I_FENCE f)],st
    | Shootdown(dom,op,sync) ->
        let loc = match n.C.evt.C.loc with
        | Data loc -> loc
        | Code _ -> Warn.user_error "TLBI/CacheSync" in
        let open TLBI in
        let r,init,csr,st =  match op.TLBI.typ with
        | ALL|VMALL|VMALLS12
            ->
              ZR,init,[],st
        | ASID|VA|VAL|VAA|VAAL|IPAS2|IPAS2L
            ->
              let r,init,st = U.next_init st p init loc in
              let r1,st = tempo1 st in
              let cs = [Instruction (lsri64 r1 r 12)] in
              r1,init,cs,st in
        let cs = emit_shootdown dom op sync r in
        init,csr@cs,st
    | CacheSync (s,isb) -> begin
        try
          let lab = C.find_prev_code_write n in
          let r,init,st = U.next_init st p init lab in
          init,emit_cachesync s isb r,st
        with Not_found ->
          Warn.user_error "No code write before CacheSync"
        end
    | CMO (t,dirloc) ->
      let loc =
        let n0 = match dirloc with
        | Next -> C.find_non_pseudo n
        | Prev -> C.find_non_pseudo_prev n in
        match n0.C.evt.C.loc with
        | Data loc -> loc
        | Code lab -> lab
      in
      let r,init,st = U.next_init st p init loc in
      init,emit_CMO t r,st

    let stronger_fence = strong


(* Dependencies *)
    let calc0  =
      if Cfg.realdep then
        fun vdep dst src -> andi vdep dst src 128
      else
        fun vdep dst src -> eor vdep dst src src

    let calc0_gen csel st vdep = match csel with
      | NoCsel -> fun src dst -> [calc0 vdep src dst],st
      | OkCsel ->
         fun dst src ->
           let r3,st = next_reg st in
           let r4,st = next_reg st in
           [do_movi vdep r3 1; do_cmpi vdep src 0;
            do_csel vdep dst r3 r4; andi vdep dst dst 2;],st

    let emit_access_dep_addr csel vdep st p init e rd =
      let r2,st = next_reg st in
      let cs0,st =  calc0_gen csel st vdep r2 rd in
      (* collapse the value `v` in event `e` to integer *)
      let value = Code.value_to_int e.C.v in
      match e.C.dir,e.C.loc with
      | None,_ -> Warn.fatal "TODO"
      | Some d,Data loc ->
          let loc = add_tag loc e.C.tag in
          let atom = match e.C.atom with
          | None -> None
          | Some (a,m) -> begin match a with
            | Plain Some Capability
            | Acq Some Capability
            | AcqPc Some Capability
            | Rel Some Capability ->
              assert (Misc.is_none m) ;
              Some (a,Some (MachSize.S128,0))
            | _ -> Some (a,m) end in
          let regs,inits,cs,st = begin match d,atom with
          | R,None ->
              let r,init,cs,st =
                LDR.emit_load_idx_var vloc vdep st p init loc r2 in
              Some r,init, pseudo cs0@cs,st
          | R,Some (Acq _,None) ->
              let r,init,cs,st =
                LDAR.emit_load_idx_var vloc vdep st p init loc r2 in
              Some r,init, pseudo cs0@cs,st
          | R,Some (Acq a,Some (sz,o)) ->
             let load =
               do_emit_load_idx_var
                 next_reg_sz
                 (fun sz _ ->  do_ldar_mixed_idx vdep AA sz o)
                 sz sz in
              let r,init,cs,st = load st p init loc r2 in
              let cs2 = emit_ldr_addon a r in
              Some r,init,pseudo cs0@cs@pseudo cs2,st
          | R,Some (AcqPc _,None) ->
              let r,init,cs,st =
                LDAPR.emit_load_idx_var vloc vdep st p init loc r2 in
              Some r,init, pseudo cs0@cs,st
          | R,Some (AcqPc a,Some (sz,o)) ->
             let load =
               do_emit_load_idx_var
                 next_reg_sz
                 (fun sz _ ->  do_ldar_mixed_idx vdep AQ sz o)
                 sz sz in
              let r,init,cs,st = load st p init loc r2 in
              let cs2 = emit_ldr_addon a r in
              Some r,init,pseudo cs0@cs@pseudo cs2,st
          | R,Some (Rel _,_) ->
              Warn.fatal "No load release"
          | R,Some (Atomic rw,None) ->
              let r,init,cs,st =
                do_emit_lda_idx vdep (tr_rw rw) st p init loc r2 in
              Some r,init, pseudo cs0@cs,st
          | R,Some (Atomic rw,Some (sz,o)) ->
              let r,init,cs,st =
                do_emit_lda_mixed_idx vdep sz o rw st p init loc r2 in
              Some r,init, pseudo cs0@cs,st
          | R,Some (Tag,None) ->
              let r,init,cs,st = LDG.emit_load_idx vdep st p init loc r2 in
              Some r,init, pseudo cs0@cs,st
          | R,Some (Tag,Some _) -> assert false
          | R,Some (CapaTag,None) ->
              (* TODO: don't waste r2 *)
              let r,init,cs,st = LDCT.emit_load_idx st p init loc rd in
              Some r,init,cs,st
          | R,Some (CapaTag,Some _) -> assert false
          | R,Some (CapaSeal,None) ->
              (* TODO: don't waste r2 *)
              let (_,rA),init,cs,st = seal_dp_addr init p loc st rd e.C.dep in
              let rB,st = next_reg st in
              Some rB,init,cs@lift_code [ldr_mixed rB rA MachSize.S128 0; gctype rB rB],st
          | R,Some (CapaSeal,Some _) -> assert false
          | R,Some (Neon n,None) ->
              let emit_load_idx = match n with
                | SIMD.NeRel -> Warn.fatal "No laod release"
                | SIMD.NeAcqPc -> LDAPUR.emit_load_idx
                | SIMD.NeP -> LDUR.emit_load_idx
                | SIMD.NePa -> LDP.emit_load_idx A64.TT
                | SIMD.NePaN -> LDP.emit_load_idx A64.NT
                | SIMD.Sv1 | SIMD.Sv2i | SIMD.Sv3i | SIMD.Sv4i -> LDNW.emit_load_idx n
                | SIMD.SvV -> LD1G.emit_load_idx n
                | SIMD.SmV | SIMD.SmH -> LD1T.emit_load_idx n
                | _ -> LDN.emit_load_idx n
              in
              let rB,init,cs,st = emit_load_idx vdep st p init loc r2 in
              Some rB,init,pseudo cs0@cs,st
          | R,Some (Pair (opt,idx),None) ->
              let r,init,cs,st =
                emit_ldp_idx_var opt idx vdep st p init loc r2 in
              Some r,init, pseudo cs0@cs,st
          | R,Some ((Neon _|Pair _),Some _) -> assert false
          | W,None ->
              let module STR =
                STORE
                  (struct
                    let store = wrap_st str
                    let store_idx st rA rB idx =
                      [do_str_idx vdep rA rB idx],st
                    let emit_mov = U.emit_mov
                  end) in
              let init,cs,st = STR.emit_store_idx st p init loc r2 value None C.evt_null in
              None,init,pseudo cs0@cs,st
          | W,Some (Rel _,None) ->
              let module STLR =
                STORE
                  (struct
                    let store = wrap_st stlr
                    let store_idx st rA rB idx =
                      let r,ins,st = do_sum_addr vdep st rB idx in
                      ins@[stlr rA r],st
                      let emit_mov = U.emit_mov
                  end) in
              let init,cs,st = STLR.emit_store_idx st p init loc r2 value None C.evt_null in
              None,init,pseudo cs0@cs,st
          | W,Some (Rel a,Some (sz,o)) ->
              let module S =
                STORE
                  (struct
                    let store = stlr_mixed sz o
                    let store_idx st r1 r2 idx =
                      let cs,st = stlr_mixed_idx sz st r1 r2 idx in
                      let cs = match o with
                      | 0 -> cs
                      | _ -> addi idx idx o::cs in
                      cs,st
                      let emit_mov = emit_mov_sz sz
                  end) in
              let init,cs,st = S.emit_store_idx st p init loc r2 value a e in
              None,init,pseudo cs0@cs,st
          | W,Some (Acq _,_) -> Warn.fatal "No store acquire"
          | W,Some (AcqPc _,_) -> Warn.fatal "No store acquirePc"
          | (R|W), Some (Instr, _) -> Warn.fatal "No dependency to code location"
          | W,Some (Atomic rw,None) ->
              let r,init,cs,st =
                emit_sta_idx (tr_rw rw) st p init loc r2 value in
              Some r,init,pseudo cs0@cs,st
          | W,Some (Atomic rw,Some (sz,o)) ->
              let r,init,cs,st = emit_sta_mixed_idx sz o rw st p init loc r2 value in
              Some r,init,pseudo cs0@cs,st
          | R,Some (Plain a,Some (sz,o)) ->
             let load_idx sz _ st r1 r2 idx =
               let cs = [ldr_mixed_idx vdep r1 r2 idx sz] in
               let cs = match o with
                 | 0 -> cs
                 | _ -> do_addi vdep idx idx o::cs in
               cs,st in
             let load =
               do_emit_load_idx_var next_reg_sz load_idx sz sz in
             let r,init,cs,st = load st p init loc r2 in
             let cs2 = emit_ldr_addon a r in
             Some r,init,pseudo cs0@cs@pseudo cs2,st
          | W,Some (Plain a,Some (sz,o)) ->
              let module S =
                STORE
                  (struct
                    let store = wrap_st (str_mixed sz o)
                    let store_idx st r1 r2 idx =
                      let cs = [str_mixed_idx sz vdep r1 r2 idx] in
                      let cs = match o with
                      | 0 -> cs
                      | _ -> do_addi vdep idx idx o::cs in
                      cs,st
                    let emit_mov = emit_mov_sz sz
                  end) in
              let init,cs,st = S.emit_store_idx st p init loc r2 value a e in
              None,init,pseudo cs0@cs,st
          | W,Some (Tag, None) ->
              let init,cs,st = STG.emit_store_idx vdep st p init e r2 in
              None,init,pseudo cs0@cs,st
          | W,Some (Pair (opt,idx),None) ->
              let init,cs,st =
                emit_stp_idx_var opt idx vdep st p init loc e r2 in
              None,init, pseudo cs0@cs,st
          | W,Some (Pair _,_) -> assert false
          | (W,(Some (Pte (Set _),None))) ->
              let init,cs,st =
                emit_set_pteval_idx false vdep r2 st p init e.C.pte (Misc.add_pte loc) in
              None,init,pseudo cs0@cs,st
          | (W,(Some (Pte (SetRel _),None))) ->
              let init,cs,st =
                emit_set_pteval_idx true vdep r2 st p init e.C.pte (Misc.add_pte loc) in
              None,init,pseudo cs0@cs,st
          | (R,(Some (Pte (Read|ReadAcq|ReadAcqPc as rk),None)))
            ->
              let emit = match rk with
              | Read -> LDR.emit_load_var_reg
              | ReadAcq -> LDAR.emit_load_var_reg
              | ReadAcqPc -> LDAPR.emit_load_var_reg
              | _ -> assert false in
              let loc = Misc.add_pte loc in
              let rA,init,st = U.next_init st p init loc in
              let rA,cs1,st = do_sum_addr vdep st rA r2 in
              let r,init,cs,st = emit A64.V64 st p init rA in
              Some r,init,pseudo cs0@pseudo cs1@cs,st
         | (W|R) as d,Some (Pte _,_ as a) ->
             Warn.fatal
               "Annotation %s does not apply to direction %s"
               (A64.pp_atom a) (Code.pp_dir d)
          | W,Some (Tag,Some _) -> assert false
          | W,Some (CapaTag,None) ->
              (* TODO: don't waste r2 *)
              let init,cs,st = STCT.emit_store_idx st p init loc rd value in
              None,init,cs,st
          | W,Some (CapaTag,Some _) -> assert false
          | W,Some (CapaSeal,None) ->
              (* TODO: don't waste r2 *)
              let (rA,rB),init,csi,st =
                seal_dp_addr init p loc st rd e.C.dep in
              let rC,init,csi2,st = U.emit_mov st p init e.C.ord in
              let init,cs,st = emit_str_addon st p init rC rA (Some Capability)
                {e with C.cseal = value} in
              None,init,
              csi@csi2@cs@lift_code [str_mixed MachSize.S128 0 rC rB],st
          | W,Some (CapaSeal,Some _) -> assert false
          | W,Some (Neon n,None) ->
             let emit_store_idx = match n with
               | SIMD.NeAcqPc -> Warn.fatal "No store acquirePc"
               | SIMD.NeRel -> STLUR.emit_store_idx
               | SIMD.NeP -> STUR.emit_store_idx
               | SIMD.NePa ->  STP.emit_store_idx A64.TT
               | SIMD.NePaN -> STP.emit_store_idx A64.NT
               | SIMD.Sv1 | SIMD.Sv2i | SIMD.Sv3i | SIMD.Sv4i -> STNW.emit_store_idx n
               | SIMD.SvV -> ST1S.emit_store_idx n
               | SIMD.SmV | SIMD.SmH -> ST1T.emit_store_idx n
               | _ -> STN.emit_store_idx n
             in
             let init,cs,st = emit_store_idx vdep st p init loc r2 value in
              None,init,pseudo cs0@cs,st
          | W,Some (Neon _,Some _) -> assert false
          | _,Some (Plain _,None) -> assert false
          end in
          (* Add a label to instructions `cs`, when a fault check is required. *)
          regs,inits,(add_label_to_last_instructions e cs),st
        | _,Code _ -> Warn.fatal "No dependency to code location"
      (* END of emit_access_dep_addr *)

    let emit_addr_dep csel vdep st p init loc rd =
      let r2,st = next_reg st in
      let cs0,st = calc0_gen csel st vdep r2 rd in
      let rA,init,st = U.next_init st p init loc in
      let rA,csum,st = do_sum_addr vdep st rA r2 in
      rA,init,pseudo (cs0@csum),st

    let emit_exch_dep_addr1 csel vdep st p init er ew rd =
      do_emit_exch1
        (fun st p init er ->
          emit_addr_dep csel vdep st p init (get_tagged_loc er) rd)
        st p init er ew

    let emit_exch_dep_addr22 csel vdep st p init er ew rd =
      do_emit_exch22
        (fun st p init er ->
          emit_addr_dep csel vdep st p init (get_tagged_loc er) rd)
        st p init er ew

    let emit_exch_dep_addr21 csel vdep st p init er ew rd =
      do_emit_exch21
        (fun st p init er ->
          emit_addr_dep csel vdep st p init (get_tagged_loc er) rd)
        st p init er ew

    let emit_exch_dep_addr12 csel vdep st p init er ew rd =
      do_emit_exch12
        (fun st p init er ->
          emit_addr_dep csel vdep st p init (get_tagged_loc er) rd)
        st p init er ew

    let emit_exch_dep_addr csel vdep st p init er ew rd =
      let ar,_ = tr_none er.C.atom
      and aw,_ = tr_none ew.C.atom in
      match ar,aw with
      | (Pair _,Pair _)->
         emit_exch_dep_addr22 csel vdep st p init er ew rd
      | (Pair _,_) ->
         check_cu (not A64.do_cu);
         emit_exch_dep_addr21 csel vdep st p init er ew rd
      | (_,Pair _) ->
         check_cu (not A64.do_cu);
         emit_exch_dep_addr12 csel vdep st p init er ew rd
      | _,_ ->
         emit_exch_dep_addr1 csel vdep st p init er ew rd

    let emit_access_dep_data csel vdep st p init e  r1 =
      let atom = match e.C.atom with
      | None -> None
      | Some (a,m) -> begin match a with
        | Plain Some Capability
        | Acq Some Capability
        | AcqPc Some Capability
        | Rel Some Capability ->
          assert (Misc.is_none m) ;
          Some (a,Some (MachSize.S128,0))
        | _ -> Some (a,m) end in
      (* collapse the value `v` in event `e` to integer *)
      let value = Code.value_to_int e.C.v in
      let regs,inits,cs,st = match e.C.dir,e.C.loc with
      | None,_ -> Warn.fatal "TODO"
      | Some R,_ -> Warn.fatal "data dependency to load"
      | Some W,Data loc ->

          let r2,cs2,init,st,addi =
            let r2,st = next_reg st in
            match atom with
            | Some (Tag,None) ->
                let cs0,st = calc0_gen csel st vdep r2 r1 in
                let rA,init,st = U.next_init st p init (add_tag loc value) in
                let rB,cB,st = sum_addr st rA r2 in
                rB,pseudo (cs0@cB),init,st,[]
            | Some (_,Some (sz,_)) ->
                let cs0,st = calc0_gen csel st vdep r2 r1 in
                let rA,init,csA,st = emit_mov_sz sz st p init value in
                let cs2 = pseudo cs0 in
                let addi = [add (sz2v sz) r2 r2 rA] in
                r2,csA@cs2,init,st,addi
            | Some (CapaSeal,None) ->
                let cs0,st = calc0_gen csel st vdep r2 r1 in
                let cs2 = pseudo cs0 in
                let addi = [addi r2 r2 e.C.ord] in
                r2,cs2,init,st,addi
            | Some (Pte _,None) ->
                let rA,init,st = U.emit_pteval st p init e.C.pte in
                let cs,st =
                  match vdep with
                  | A64.V128 ->
                      Warn.fatal "128 bit dependency to pte access"
                  | A64.V64 ->
                      calc0_gen csel st A64.V64 r2 r1
                  | A64.V32 ->
                      let r3,st = tempo1 st in
                      let cs0,st = calc0_gen csel st A64.V64 r2 r3 in
                      sxtw r3 r1::cs0,st in
                let addi = [add A64.V64 r2 r2 rA] in
                let cs2 = pseudo cs in
                r2,cs2,init,st,addi
            | _ ->
                let cs2,st =
                  match vdep,vloc with
                  | (V128,_)|(_,V128) ->
                      Warn.fatal "dependance from 128 bits access"
                  | (V32,V32)|(V64,V64)|(V64,V32) ->
                     calc0_gen csel st vdep r2 r1
                  | (V32,V64) ->
                      let r3,st = tempo1 st in
                      let cs,st = calc0_gen csel st vdep r3 r1 in
                      sxtw r2 r3::cs,st in
                let addi = [addi r2 r2 value] in
                let cs2 = pseudo cs2 in
                r2,cs2,init,st,addi in
          let r2,cs2,init,st = match atom with
            | Some(Neon _, None) -> r2,cs2,init,st
            | _ -> r2,cs2@pseudo addi,init,st in
          let loc = add_tag loc e.C.tag in
          begin match atom with
          | None ->
              let init,cs,st =
                STR.emit_store_reg st p init loc r2 None C.evt_null in
              None,init,cs2@cs,st
          | Some (Rel _,None) ->
              let init,cs,st =
                STLR.emit_store_reg st p init loc r2 None C.evt_null in
              None,init,cs2@cs,st
          | Some (Rel a,Some (sz,o)) ->
              let module S =
                STORE
                  (struct
                    let store = stlr_mixed sz o
                    let store_idx _st _r1 _r2 _idx = assert false
                    let emit_mov = emit_mov_sz sz
                  end) in
              let init,cs,st = S.emit_store_reg st p init loc r2 a e in
              None,init,cs2@cs,st
          | Some (Atomic rw,None) ->
              let r,init,cs,st = emit_sta_reg (tr_rw rw) st p init loc r2 in
              Some r,init,cs2@cs,st
          | Some (Atomic rw,Some (sz,o)) ->
              let r,init,cs,st = emit_sta_mixed_reg sz o rw st p init loc r2 in
              Some r,init,cs2@cs,st
          | Some (Acq _,_) ->
              Warn.fatal "No store acquire"
          | Some (AcqPc _,_) ->
              Warn.fatal "No store acquirePc"
          | Some (Instr, _) -> Warn.fatal "No Plain Write to label (code location)"
          | Some (Plain a,Some (sz,o)) ->
              let module S =
                STORE
                  (struct
                    let store  = wrap_st (str_mixed sz o)
                    let store_idx st r1 r2 idx =
                      let cs = [str_mixed_idx sz V64 r1 r2 idx] in
                      let cs = match o with
                      | 0 -> cs
                      | _ -> addi_64 idx idx o::cs in
                      cs,st
                    let emit_mov = emit_mov_sz sz
                  end) in
              let init,cs,st = S.emit_store_reg st p init loc r2 a e in
              None,init,cs2@cs,st
          | Some (Tag, None) ->
              let init,cs,st = STG.emit_store_reg st p init loc r2 in
              None,init,cs2@cs,st
          | Some (Pte (Set _),None) ->
              let init,cs,st = emit_set_pteval_reg false st p init r2 (Misc.add_pte loc) in
              None,init,cs2@cs,st
          | Some (Pte (SetRel _),None) ->
              let init,cs,st = emit_set_pteval_reg true st p init r2 (Misc.add_pte loc) in
              None,init,cs2@cs,st
          | Some ((Pte _,Some _)|(Pte (Read|ReadAcq|ReadAcqPc),_))
            -> assert false
          | Some (Plain _,None) -> assert false
          | Some (Tag,Some _) -> assert false
          | Some (CapaTag,None) ->
              if value > 1 then Warn.fatal "Capability tags can't be incremented above 1";
              let init,cs,st = STCT.emit_store_reg st p init loc r2 in
              None,init,cs2@cs,st
          | Some (CapaTag,Some _) -> assert false
          | Some (CapaSeal,None) ->
              let rA,init,st = U.next_init st p init loc in
              let init,cs,st =
                emit_str_addon
                  st p init r2 rA (Some Capability) {e with C.cseal = value} in
              None,init,cs2@cs@lift_code [str_mixed MachSize.S128 0 r2 rA],st
          | Some (CapaSeal,Some _) -> assert false
          | Some (Neon n,None) ->
             let rA,init,st = U.next_init st p init loc in
             let emit_store_dep = match n with
               | SIMD.NeAcqPc -> Warn.fatal "No store acquirePc"
               | SIMD.NeRel -> STLUR.emit_store_dep
               | SIMD.NeP -> STUR.emit_store_dep
               | SIMD.NePa ->  STP.emit_store_dep A64.TT
               | SIMD.NePaN -> STP.emit_store_dep A64.NT
               | SIMD.Sv1 | SIMD.Sv2i | SIMD.Sv3i | SIMD.Sv4i -> STNW.emit_store_dep n
               | SIMD.SvV -> ST1S.emit_store_dep n
               | SIMD.SmV | SIMD.SmH -> ST1T.emit_store_dep n
               | _ -> STN.emit_store_dep n
             in
             let init,cs,st = emit_store_dep r2 st init rA value in
             None,init,cs2@cs,st
          | Some (Neon _,Some _) -> assert false
          | Some (Pair (opt,idx),None) ->
             let init,cs,st = stp_emit_store_reg opt idx st p init loc r2 in
             None,init,cs2@cs,st
          | Some (Pair _,Some _) -> assert false
          end
      (* END of `Some W` *)
      | _,Code _ -> Warn.fatal "Not Yet (%s,dep_data)" (C.debug_evt e) in
    regs,inits,(add_label_to_last_instructions e cs),st
    (* END of emit_access_dep_data *)

    let is_ctrlisync = function
      | D.CTRLISYNC -> true
      | _ -> false

    let insert_isb isb cs1 cs2 =
      if isb then cs1@[Instruction (I_FENCE ISB)]@cs2
      else cs1@cs2

    let emit_ctrl vdep r =
      let lab = Label.next_label "LC" in
      let c = [Instruction (do_cbnz vdep r lab); Label (lab,Nop);] in c

    let emit_ctrl_gen csel st vdep r = match csel with
      | NoCsel -> emit_ctrl vdep r,st
      | OkCsel ->
         let r2,st = next_reg st in
         let r3,st = next_reg st in
         pseudo
           [do_cmpi vdep r 0; do_cinc vdep r2 r3 r2;]@
           emit_ctrl vdep r2,
         st

    let emit_access_ctrl csel vdep isb st p init e r1 =
      let c,st = emit_ctrl_gen csel st vdep r1 in
      let ropt,init,cs,st = emit_access st p init e in
      ropt,init,insert_isb isb c cs,st

    let emit_exch_ctrl csel vdep isb st p init er ew r1 =
      let c,st = emit_ctrl_gen csel st vdep r1 in
      let ropt,init,cs,st = emit_exch st p init er ew in
      ropt,init,insert_isb isb c cs,st

    let tr_atom = function
      | Some ((Tag|Pte _),_) -> V64
      | at ->
         begin match A64.get_access_atom at with
         | Some (sz,_) -> sz2v sz
         | None -> vloc
         end

    let node2vdep n =
      let e = n.C.evt in
      let at = e.C.atom in
      tr_atom at

    let emit_access_dep st p init e (dp,csel) r1 n1 =
      let vdep = node2vdep n1 in
      match dp with
      | D.ADDR -> emit_access_dep_addr csel vdep st p init e r1
      | D.DATA -> emit_access_dep_data csel vdep st p init e r1
      | D.CTRL -> emit_access_ctrl csel vdep false st p init e r1
      | D.CTRLISYNC -> emit_access_ctrl csel vdep true st p init e r1

    let emit_exch_dep st p init er ew (dp,csel) vdep rd = match dp with
    | D.ADDR -> emit_exch_dep_addr csel vdep st p init er ew rd
    | D.DATA -> Warn.fatal "not data dependency to RMW"
    | D.CTRL -> emit_exch_ctrl csel vdep false st p init er ew rd
    | D.CTRLISYNC -> emit_exch_ctrl csel vdep true st p init er ew rd

    let emit_ldop_dep ins ins_mixed  st p init er ew (dp,csel) vdep rd =
      match dp with
    | D.ADDR ->
       let rA,init,caddr,st =
         emit_addr_dep csel vdep st p init
           (get_tagged_loc er) rd in
        let rR,init,cs,st = do_emit_ldop_rA ins ins_mixed st p init er ew rA in
        rR,init,caddr@cs,st
    | D.CTRL|D.CTRLISYNC ->
        let c = emit_ctrl vdep rd in
        let rR,init,cs,st = do_emit_ldop ins ins_mixed st p init er ew in
        rR,init,insert_isb (is_ctrlisync dp) c cs,st
    | D.DATA -> Warn.fatal "Data dependency to LDOP"

    let emit_cas_dep  st p init er ew (dp,csel) vdep rd = match dp with
    | D.ADDR ->
       let rA,init,caddr,st =
         emit_addr_dep csel vdep st p init (get_tagged_loc er) rd in
        let rR,init,cs,st = emit_cas_rA st p init er ew rA in
        rR,init,caddr@cs,st
    | D.CTRL|D.CTRLISYNC ->
        let c,st = emit_ctrl_gen csel st vdep rd in
        let rR,init,cs,st = emit_cas st p init er ew in
        rR,init,insert_isb (is_ctrlisync dp) c cs,st
    | D.DATA -> Warn.fatal "Data dependency to CAS"

    let emit_stop_dep  op st p init er ew (dp,csel) rd n =
      let vdep = node2vdep n in
      match dp with
      | D.ADDR ->
         let rA,init,caddr,st =
           emit_addr_dep csel vdep st p init (get_tagged_loc er) rd in
         let rR,init,cs,st = emit_stop_rA op st p init er ew rA in
         rR,init,caddr@cs,st
      | D.CTRL|D.CTRLISYNC ->
         let c,st = emit_ctrl_gen csel st vdep rd in
         let rR,init,cs,st = emit_stop op st p init er ew in
         rR,init,insert_isb (is_ctrlisync dp) c cs,st
      | D.DATA -> Warn.fatal "Data dependency to STOP"


    let map_some_dp f st p init er ew dp rd n =
      let vdep = node2vdep n in
      let r,init,cs,st = f  st p init er ew dp vdep rd in
      Some r,init,cs,st

    let emit_rmw_dep rmw = match rmw with
    | LrSc -> map_some_dp emit_exch_dep
    | LdOp op -> map_some_dp (emit_ldop_dep (ldop op) (ldop_mixed op))
    | Swp ->  map_some_dp (emit_ldop_dep swp swp_mixed)
    | Cas -> map_some_dp emit_cas_dep
    | StOp op -> emit_stop_dep op

    let emit_fence_dp st p init n f (dp,csel) r1 n1 =
      let vdep = node2vdep n1 in
      match dp with
      | D.ADDR ->
        let n2 = C.find_non_pseudo n in
        let loc = match n2.C.evt.C.loc with
        | Data loc -> loc
        | Code lab -> match f with
          | CMO (_,Next) -> lab
          | _ -> Warn.fatal "Address dependency to code location must be directly followed by DC.CVAUn or IC.IVAUn" in
        (try ignore (C.find_node
          (fun m -> match m.C.edge.E.edge with
            | E.Insert (CMO (_,Prev)) when (loc_eq m.C.evt.C.loc n.C.evt.C.loc) ->
              Warn.fatal "Address dependency to code location cannot have IC.IVAUp or DC.CVAUp"
            | _ -> false )
          n)
        with Not_found -> ());
        let r2,st = next_reg st in
        let cs0,st =  calc0_gen csel st vdep r2 r1 in
        let rB,init,st = U.next_init st p init loc in
        let r,st = match f with
          | CMO _ -> rB,st
          | _ -> tempo1 st in
        let cs2 = (if do_morello then
          [do_addcapa rB r r2]
        else
          [do_add64 vloc rB r r2]) in
        let _,cs, st = emit_fence st p init n f in
        None, init, pseudo cs0@pseudo cs2@cs, st
      | D.DATA -> let init,cs, st = emit_fence st p init n f in
        Some r1, init, cs, st
      | D.CTRL | D.CTRLISYNC ->
        let c,st = emit_ctrl_gen csel st vdep r1 in
        let init,cs,st = emit_fence st p init n f in
        None,init,insert_isb (is_ctrlisync dp) c cs,st

    let do_check_load p st r e =
      let ok,st = A.ok_reg st in
      (fun k ->
        Instruction (cmpi r (Code.value_to_int e.C.v))::
        Instruction (bne (Label.last p))::
        Instruction (incr ok)::
        k),
      A.next_ok st

    let check_load  p r e init st =
      let cs,st = do_check_load p st r e in
      init,cs,st

(* Postlude *)

    let postlude =
      mk_postlude
        (fun st p init loc r ->
          STR.emit_store_reg st p init loc r None C.evt_null)


    let get_strx_result k = function
      | I_STXR (_,_,r,_,_)|I_STXRBH (_,_,r,_,_)
        -> r::k
      | _ -> k

    let get_strx_result_pseudo k = pseudo_fold  get_strx_result k

    let get_xstore_results = match Cfg.unrollatomic with
    | Some x when x <= 0 ->
        fun cs ->
          let rs = List.fold_left get_strx_result_pseudo [] cs in
          List.rev_map (fun r -> r,0) rs
    | Some _|None -> fun _ -> []

    include NoInfo
  end
