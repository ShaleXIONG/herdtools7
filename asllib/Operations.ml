open AST
open ASTUtils
open Error

let value_as_int pos = function
  | L_Int i -> (
      try Z.to_int i
      with Z.Overflow ->
        failwith "Cannot slice with an integer more than machine size.")
  | v ->
      fatal_from pos (Error.MismatchType (PP.literal_to_string v, [ integer' ]))

let is_positive z = Z.sign z != -1
let is_strict_positive z = Z.sign z = 1
let bv_same_length b1 b2 = Bitvector.(length b1 = length b2)

let exp_real q z =
  if Q.sign q = 0 then (
    assert (Z.sign z >= 0) (* Case handled earlier *);
    if Z.sign z = 0 then Q.one else Q.zero)
  else
    let q, z = if is_positive z then (q, z) else (Q.inv q, Z.neg z) in
    let num = Q.num q and den = Q.den q in
    let i = Z.to_int z in
    let res_num = Z.pow num i and res_den = Z.pow den i in
    Q.(res_num /// res_den)

let literal_to_string = function
  | L_Int i -> Z.to_string i
  | L_Bool true -> "TRUE"
  | L_Bool false -> "FALSE"
  | L_Real r -> Q.to_string r
  | L_BitVector bv -> Bitvector.to_string_hexa bv
  | L_String s -> s
  | L_Label l -> l

let binop_values pos t (op : binop) v1 v2 =
  match (op, v1, v2) with
  (* int -> int -> int *)
  | `ADD, L_Int v1, L_Int v2 -> L_Int (Z.add v1 v2)
  | `MUL, L_Int v1, L_Int v2 -> L_Int (Z.mul v1 v2)
  | `SUB, L_Int v1, L_Int v2 -> L_Int (Z.sub v1 v2)
  | `DIV, L_Int v1, L_Int v2 when is_strict_positive v2 && Z.divisible v1 v2 ->
      L_Int (Z.divexact v1 v2)
  | `DIVRM, L_Int v1, L_Int v2 when is_strict_positive v2 ->
      L_Int (Z.ediv v1 v2) (* Division rounded towards minus infinity. *)
  | `MOD, L_Int v1, L_Int v2 when is_strict_positive v2 -> L_Int Z.(erem v1 v2)
  | `POW, L_Int v1, L_Int v2 when is_positive v2 -> L_Int Z.(pow v1 (to_int v2))
  | `SHL, L_Int v1, L_Int v2 when is_positive v2 ->
      L_Int Z.(shift_left v1 (to_int v2))
  | `SHR, L_Int v1, L_Int v2 when is_positive v2 ->
      L_Int Z.(shift_right v1 (to_int v2))
  (* int -> int -> bool*)
  | `EQ, L_Int v1, L_Int v2 -> L_Bool (Z.equal v1 v2)
  | `NE, L_Int v1, L_Int v2 -> L_Bool (not (Z.equal v1 v2))
  | `LE, L_Int v1, L_Int v2 -> L_Bool (Z.leq v1 v2)
  | `LT, L_Int v1, L_Int v2 -> L_Bool (Z.lt v1 v2)
  | `GE, L_Int v1, L_Int v2 -> L_Bool (Z.geq v1 v2)
  | `GT, L_Int v1, L_Int v2 -> L_Bool (Z.gt v1 v2)
  (* bool -> bool -> bool *)
  | `BAND, L_Bool b1, L_Bool b2 -> L_Bool (b1 && b2)
  | `BOR, L_Bool b1, L_Bool b2 -> L_Bool (b1 || b2)
  | `BEQ, L_Bool b1, L_Bool b2 -> L_Bool (b1 == b2)
  | `IMPL, L_Bool b1, L_Bool b2 -> L_Bool ((not b1) || b2)
  | `EQ, L_Bool b1, L_Bool b2 -> L_Bool (b1 == b2)
  | `NE, L_Bool b1, L_Bool b2 -> L_Bool (b1 <> b2)
  (* int -> real -> real  or  real -> int -> real *)
  | `MUL, L_Int v1, L_Real v2 -> L_Real (Q.mul (Q.of_bigint v1) v2)
  | `MUL, L_Real v1, L_Int v2 -> L_Real (Q.mul v1 (Q.of_bigint v2))
  (* real -> real -> real *)
  | `ADD, L_Real v1, L_Real v2 -> L_Real (Q.add v1 v2)
  | `MUL, L_Real v1, L_Real v2 -> L_Real (Q.mul v1 v2)
  | `SUB, L_Real v1, L_Real v2 -> L_Real (Q.sub v1 v2)
  | `RDIV, L_Real v1, L_Real v2 when not (Q.sign v2 = 0) -> L_Real (Q.div v1 v2)
  | `POW, L_Real q1, L_Int z2 when not (Q.sign q1 = 0 && Z.sign z2 < 0) ->
      (* 0.0 ^ z is not defined for z < 0 *)
      L_Real (exp_real q1 z2)
  (* real -> real -> bool *)
  | `EQ, L_Real v1, L_Real v2 -> L_Bool (Q.equal v1 v2)
  | `NE, L_Real v1, L_Real v2 -> L_Bool (not (Q.equal v1 v2))
  | `LE, L_Real v1, L_Real v2 -> L_Bool (Q.leq v1 v2)
  | `LT, L_Real v1, L_Real v2 -> L_Bool (Q.lt v1 v2)
  | `GE, L_Real v1, L_Real v2 -> L_Bool (Q.geq v1 v2)
  | `GT, L_Real v1, L_Real v2 -> L_Bool (Q.gt v1 v2)
  (* bits -> bits -> bool *)
  | `EQ, L_BitVector b1, L_BitVector b2 when bv_same_length b1 b2 ->
      L_Bool (Bitvector.equal b1 b2)
  | `NE, L_BitVector b1, L_BitVector b2 when bv_same_length b1 b2 ->
      L_Bool (not @@ Bitvector.equal b1 b2)
  (* bits -> bits -> bits *)
  | `OR, L_BitVector b1, L_BitVector b2 when bv_same_length b1 b2 ->
      L_BitVector (Bitvector.logor b1 b2)
  | `AND, L_BitVector b1, L_BitVector b2 when bv_same_length b1 b2 ->
      L_BitVector (Bitvector.logand b1 b2)
  | `XOR, L_BitVector b1, L_BitVector b2 when bv_same_length b1 b2 ->
      L_BitVector (Bitvector.logxor b1 b2)
  | `BIC, L_BitVector b1, L_BitVector b2 when bv_same_length b1 b2 ->
      (* Internal usage of BIC operator *)
      L_BitVector (Bitvector.logand b1 (Bitvector.lognot b2))
  | `ADD, L_BitVector b1, L_BitVector b2 when bv_same_length b1 b2 ->
      L_BitVector
        Bitvector.(
          of_z (length b1) (Z.add (to_z_unsigned b1) (to_z_unsigned b2)))
  | `SUB, L_BitVector b1, L_BitVector b2 when bv_same_length b1 b2 ->
      L_BitVector
        Bitvector.(
          of_z (length b1) (Z.sub (to_z_unsigned b1) (to_z_unsigned b2)))
  | `BV_CONCAT, L_BitVector b1, L_BitVector b2 ->
      L_BitVector (Bitvector.concat [ b1; b2 ])
  (* bits -> integer -> bits *)
  | `ADD, L_BitVector b1, L_Int z2 ->
      L_BitVector Bitvector.(of_z (length b1) (Z.add (to_z_unsigned b1) z2))
  | `SUB, L_BitVector b1, L_Int z2 ->
      L_BitVector Bitvector.(of_z (length b1) (Z.sub (to_z_unsigned b1) z2))
  (* string -> string -> bool *)
  | `EQ, L_String s1, L_String s2 -> L_Bool (String.equal s1 s2)
  | `NE, L_String s1, L_String s2 -> L_Bool (not (String.equal s1 s2))
  (* enum -> enum -> bool *)
  | `EQ, L_Label s1, L_Label s2 -> L_Bool (String.equal s1 s2)
  | `NE, L_Label s1, L_Label s2 -> L_Bool (not (String.equal s1 s2))
  (* string concatenation *)
  | `STR_CONCAT, _, _ ->
      let str = literal_to_string v1 ^ literal_to_string v2 in
      L_String str
  (* Failure *)
  | _ -> fatal_from pos (Error.UnsupportedBinop (t, op, v1, v2))

let unop_values pos t op v =
  match (op, v) with
  | NEG, L_Int i -> L_Int (Z.neg i)
  | NEG, L_Real r -> L_Real (Q.neg r)
  | BNOT, L_Bool b -> L_Bool (not b)
  | NOT, L_BitVector bv -> L_BitVector (Bitvector.lognot bv)
  | _ -> fatal_from pos (Error.UnsupportedUnop (t, op, v))
