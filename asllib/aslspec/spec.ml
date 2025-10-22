(** A module for querying the AST (of the semantics-specification for ASL) and
    for checking its correctness. *)

open AST
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

(** [vars_of_type_term term] returns the list of term-naming variables that
    occur at any depth inside [term]. *)
let rec vars_of_type_term term =
  let listed_vars =
    match term with
    | Label _ -> []
    | Operator { term } -> opt_named_term_to_var_list term
    | LabelledTuple { components } -> vars_of_opt_named_type_terms components
    | LabelledRecord { fields } ->
        Utils.list_concat_map
          (fun (field_name, t) -> field_name :: vars_of_type_term t)
          fields
    | ConstantsSet _ -> []
    | Function { from_type; to_type } ->
        opt_named_term_to_var_list from_type
        @ opt_named_term_to_var_list to_type
  in
  List.sort_uniq String.compare listed_vars

(** [opt_named_term_to_var_list (var, t)] returns the list of term-naming
    variables that occur at any depth inside [t], plus [var] if it is [Some x].
*)
and opt_named_term_to_var_list (var, t) =
  Option.to_list var @ vars_of_type_term t

(** [vars_of_opt_named_type_terms named_terms] returns the list of term-naming
    variables that occur at any depth inside [opt_named_terms]. *)
and vars_of_opt_named_type_terms opt_named_terms =
  List.map opt_named_term_to_var_list opt_named_terms |> List.concat

(** Returns the label of a type variant, if it consists of a label. *)
let variant_to_label_opt { TypeVariant.term } =
  match term with
  | Label label -> Some label
  | LabelledTuple { label_opt } | LabelledRecord { label_opt } -> label_opt
  | _ -> None

(** Wraps AST nodes that define identifiers that may appear in type terms and
    expression terms: types, constants, relations, [Label]s, labelled tuples,
    and labelled records. *)
type definition_node =
  | Node_Type of Type.t
  | Node_Relation of Relation.t
  | Node_TypeVariant of TypeVariant.t
  | Node_Constant of Constant.t

let definition_node_name = function
  | Node_Type { Type.name }
  | Node_Relation { Relation.name }
  | Node_Constant { Constant.name } ->
      name
  | Node_TypeVariant def -> Option.get (variant_to_label_opt def)

let math_macro_opt_for_node = function
  | Node_Type def -> Type.math_macro def
  | Node_Relation def -> Relation.math_macro def
  | Node_TypeVariant def -> TypeVariant.math_macro def
  | Node_Constant def -> Constant.math_macro def

let prose_description_for_node = function
  | Node_Type def -> Type.prose_description def
  | Node_Relation def -> Relation.prose_description def
  | Node_TypeVariant def -> TypeVariant.prose_description def
  | Node_Constant def -> Constant.prose_description def

(** [vars_of_node node] returns the list of term-naming variables that occur at
    any depth inside the definition node [node]. *)
let vars_of_node = function
  | Node_Type { Type.variants; _ } ->
      Utils.list_concat_map
        (fun { TypeVariant.term } -> vars_of_type_term term)
        variants
  | Node_TypeVariant { TypeVariant.term } -> vars_of_type_term term
  | Node_Constant _ -> []
  | Node_Relation { Relation.input; output; _ } ->
      vars_of_opt_named_type_terms input
      @ Utils.list_concat_map vars_of_type_term output

(** Utility functions for handling layouts. *)
module Layout = struct
  (** For a relation like [ r(a,b,c) -> A|B|C ] generates a term
      [ (r(a,b,c), (A,B,C)) ]. That is, a pair where the first component is a
      labelled tuple with the relation name and arguments and the second
      component is a tuple with all output terms. *)
  let relation_to_tuple { Relation.name; input; output } =
    let opt_named_output_terms = List.map (fun term -> (None, term)) output in
    make_tuple
      [
        (None, make_labelled_tuple name input);
        (None, make_tuple opt_named_output_terms);
      ]

  (** Creates a horizontal layout for [term]. *)
  let rec horizontal_for_type_term term =
    match term with
    | Label _ -> Unspecified
    | Operator { term = _, t } -> horizontal_for_type_term t
    | LabelledTuple { components } ->
        if List.length components > 1 then
          Horizontal
            (List.map (fun (_, t) -> horizontal_for_type_term t) components)
        else Unspecified
    | LabelledRecord { fields; _ } ->
        if List.length fields > 1 then
          Horizontal
            (List.map (fun (_, t) -> horizontal_for_type_term t) fields)
        else Unspecified
    | ConstantsSet names ->
        Horizontal ((List.init (List.length names)) (fun _ -> Unspecified))
    | Function { from_type = _, from_term; to_type = _, to_term; _ } ->
        Horizontal
          [
            horizontal_for_type_term from_term; horizontal_for_type_term to_term;
          ]

  let rec default_for_type_term term =
    match term with
    | Label _ -> Unspecified
    | Operator { term = _, t } -> default_for_type_term t
    | LabelledTuple { components } ->
        if List.length components > 1 then
          Horizontal
            (List.map (fun (_, t) -> default_for_type_term t) components)
        else Unspecified
    | LabelledRecord { fields; _ } ->
        if List.length fields > 1 then
          Vertical (List.map (fun (_, t) -> default_for_type_term t) fields)
        else Unspecified
    | ConstantsSet names ->
        Horizontal ((List.init (List.length names)) (fun _ -> Unspecified))
    | Function { from_type = _, from_term; to_type = _, to_term; _ } ->
        Horizontal
          [ default_for_type_term from_term; default_for_type_term to_term ]

  let horizontal_if_unspecified layout terms =
    match layout with
    | Horizontal _ | Vertical _ -> layout
    | _ -> Horizontal (List.map (fun _ -> Unspecified) terms)

  let rec contains_vertical = function
    | Unspecified -> false
    | Horizontal shapes -> List.exists contains_vertical shapes
    | Vertical _ -> true

  let math_layout_for_node = function
    | Node_Type def -> (
        match Type.math_layout def with
        | Some layout -> layout
        | None -> Unspecified)
    | Node_Constant _ -> Unspecified
    | Node_Relation def -> (
        match Relation.math_layout def with
        | Some layout -> layout
        | None -> default_for_type_term (relation_to_tuple def))
    | Node_TypeVariant ({ TypeVariant.term } as def) -> (
        match TypeVariant.math_layout def with
        | Some layout -> layout
        | None -> default_for_type_term term)
end

let definition_node_attributes = function
  | Node_Type { Type.att }
  | Node_TypeVariant { TypeVariant.att }
  | Node_Relation { Relation.att }
  | Node_Constant { Constant.att } ->
      att

let elem_name = function
  | Elem_Type { Type.name }
  | Elem_Relation { Relation.name }
  | Elem_Constant { Constant.name } ->
      name
  | Elem_Render { name } -> name

(** Lists nodes that define identifiers and can be referenced. *)
let list_definition_nodes ast =
  List.fold_left
    (fun acc_nodes elem ->
      match elem with
      | Elem_Relation def -> Node_Relation def :: acc_nodes
      | Elem_Type ({ Type.variants } as def) ->
          let acc_nodes = Node_Type def :: acc_nodes in
          (* Labelled types directly under this type are considered defined here,
             but labelled types nested further in are considered as type references.
          *)
          List.fold_left
            (fun acc_nodes ({ TypeVariant.term } as variant) ->
              match term with
              | Label _
              | LabelledTuple { label_opt = Some _ }
              | LabelledRecord { label_opt = Some _ } ->
                  Node_TypeVariant variant :: acc_nodes
              | LabelledTuple { label_opt = None }
              | LabelledRecord { label_opt = None }
              | _ ->
                  acc_nodes)
            acc_nodes variants
      | Elem_Constant def -> Node_Constant def :: acc_nodes
      (* Although a render defines an identifier, it does not carry semantic
         meaning, and cannot be referenced elsewhere. *)
      | Elem_Render _ -> acc_nodes)
    [] ast

(** Creates a map from identifiers to the nodes where they are defined. If two
    nodes with the same identifier exist, a [SpecError] is raised. *)
let make_id_to_definition_node definition_nodes =
  List.fold_left
    (fun acc_map node ->
      let name = definition_node_name node in
      if StringMap.mem name acc_map then
        let msg = Format.sprintf "Duplicate definitions found for '%s'" name in
        raise (SpecError msg)
      else StringMap.add name node acc_map)
    StringMap.empty definition_nodes

type t = {
  ast : AST.t;  (** The original AST as parsed *)
  id_to_defining_node : definition_node StringMap.t;
      (** Associates identifiers with the AST nodes where they are defined. *)
  defined_ids : string list;
      (** The list of identifiers defined in the spec in order of appearance. *)
}

let ast spec = spec.ast

(** [defining_node_for_id self id] returns the defining node for the element
    with identifier [id] that is given in the specification. If no such element
    exists, a [SpecError] is raised. *)
let defining_node_for_id self id =
  try StringMap.find id self.id_to_defining_node
  with Not_found ->
    let msg = Format.sprintf "Encountered undefined element: %s" id in
    raise (SpecError msg)

module Check = struct
  let list_find_duplicate strs =
    let module StringSet = Set.Make (String) in
    let rec find_with_set set = function
      | [] -> None
      | str :: tail ->
          if StringSet.mem str set then Some str
          else find_with_set (StringSet.add str set) tail
    in
    find_with_set StringSet.empty strs

  (** Checks, for each definition node, that all mandatory attributes are
      present. The parser ensures only allowed attributes are added. *)
  let check_mandatory_attributes definition_nodes =
    let check_mandatory_attributes_for_definition_node defining_node =
      let attributes = definition_node_attributes defining_node in
      let mandatory_attrs =
        let open AttributeKey in
        match defining_node with
        | Node_Type _ | Node_TypeVariant _ | Node_Constant _ ->
            [ Prose_Description ]
        | Node_Relation _ -> [ Prose_Description; Prose_Application ]
      in
      List.iter
        (fun attr ->
          if not (Attributes.mem attr attributes) then
            let name = definition_node_name defining_node in
            let msg =
              Format.sprintf "element '%s' is missing mandatory attribute: '%s'"
                name (AttributeKey.to_str attr)
            in
            raise (SpecError msg))
        mandatory_attrs
    in
    List.iter check_mandatory_attributes_for_definition_node definition_nodes

  let rec check_layout term layout =
    let msg =
      Format.asprintf
        "layout %a is inconsistent with %a. Here's a consistent layout: %a"
        PP.pp_math_shape layout PP.pp_type_term term PP.pp_math_shape
        (Layout.default_for_type_term term)
    in
    let open Layout in
    match (term, layout) with
    | Label _, Unspecified -> ()
    | Label _, _ -> raise (SpecError msg)
    | Operator { term = _, t }, _ -> check_layout t layout
    | LabelledTuple { components }, Horizontal cells
    | LabelledTuple { components }, Vertical cells ->
        if List.length components <> List.length cells then
          raise (SpecError msg)
        else
          List.iter2
            (fun (_, term) cell -> check_layout term cell)
            components cells
    | LabelledRecord { fields }, Horizontal cells
    | LabelledRecord { fields }, Vertical cells ->
        if List.length fields <> List.length cells then raise (SpecError msg)
        else
          List.iter2 (fun (_, term) cell -> check_layout term cell) fields cells
    | ConstantsSet names, Horizontal cells ->
        if List.length names <> List.length cells then raise (SpecError msg)
        else List.iter2 (fun _ cell -> check_layout (Label "") cell) names cells
    | ( Function { from_type = _, from_term; to_type = _, to_term; _ },
        Horizontal cells ) ->
        if List.length cells <> 2 then raise (SpecError msg)
        else check_layout from_term (List.nth cells 0);
        check_layout to_term (List.nth cells 1)
    | _ -> ()

  let check_math_layout definition_nodes =
    let check_math_layout_for_definition_node node =
      let open Layout in
      match node with
      | Node_Type { Type.name } ->
          check_layout (Label name) (math_layout_for_node node)
      | Node_Constant { Constant.name } ->
          check_layout (Label name) (math_layout_for_node node)
      | Node_TypeVariant { TypeVariant.term } ->
          check_layout term (math_layout_for_node node)
      | Node_Relation def ->
          check_layout (relation_to_tuple def) (math_layout_for_node node)
    in
    List.iter check_math_layout_for_definition_node definition_nodes

  (** Returns all the identifiers referencing nodes that define identifiers. *)
  let rec referenced_ids = function
    | Label id -> [ id ]
    | Operator { term = _, t } -> referenced_ids t
    | LabelledTuple { label_opt; components } -> (
        let component_ids =
          List.map snd components |> Utils.list_concat_map referenced_ids
        in
        match label_opt with
        | None -> component_ids
        | Some label -> label :: component_ids)
    | LabelledRecord { label_opt; fields } -> (
        match label_opt with
        | None -> List.map snd fields |> Utils.list_concat_map referenced_ids
        | Some label ->
            label
            :: (List.map snd fields |> Utils.list_concat_map referenced_ids))
    | ConstantsSet constant_names -> constant_names
    | Function { from_type = _, from_term; to_type = _, to_term } ->
        referenced_ids from_term @ referenced_ids to_term

  (** [check_no_undefined_ids elements id_to_defining_node] checks that all
      identifiers referenced in [elements] are keys in [id_to_defining_node]. *)
  let check_no_undefined_ids elements id_to_defining_node =
    let check_no_undefined_ids_in_elem id_to_defining_node elem =
      let referenced_ids_for_list = Utils.list_concat_map referenced_ids in
      let ids_referenced_by_elem =
        match elem with
        | Elem_Constant _ -> []
        | Elem_Type { Type.variants } ->
            referenced_ids_for_list
              (List.map (fun { TypeVariant.term } -> term) variants)
        | Elem_Relation { Relation.input; output } ->
            let input_terms = List.map snd input in
            referenced_ids_for_list (input_terms @ output)
        | Elem_Render { pointers } ->
            Utils.list_concat_map
              (fun { TypesRender.type_name; variant_names } ->
                type_name :: variant_names)
              pointers
      in
      List.iter
        (fun id ->
          if not (StringMap.mem id id_to_defining_node) then
            let msg =
              Format.sprintf "Undefined reference to '%s' in '%s'" id
                (elem_name elem)
            in
            raise (SpecError msg))
        ids_referenced_by_elem
    in
    List.iter (check_no_undefined_ids_in_elem id_to_defining_node) elements

  (** A module for checking that each prose template string ([prose_description]
      and [prose_application] attributes) to ensure it does not contain a
      [{var}] where [var] does not name any type term. If it does, LaTeX will
      fail on [{var}], which would require debugging the generated code. This
      check catches such cases and generates an easy to understand explanation.
  *)
  module CheckProseTemplates : sig
    val check : definition_node list -> unit
    (** [check definition_nodes] checks all prose templates in
        [definition_nodes] *)
  end = struct
    (** [check_prose_template_for_vars template vars] checks that [template]
        does not contain a [{var}] where [var] is not in [vars]. Otherwise,
        raises a [SpecError] detailing the unmatched variables. *)
    let check_prose_template_for_vars template vars =
      let open Text in
      (* Populate with [{var}] for each [var]. *)
      let template_vars =
        List.fold_left
          (fun acc_map var_str ->
            let template_var = spec_var_to_template_var var_str in
            StringSet.add template_var acc_map)
          StringSet.empty vars
      in
      let template_var_regexp = Str.regexp "{[a-zA-Z0-9_']+}" in
      (* Remove things like [\texttt{a}], which do not (should not) reference variables. *)
      let reduce_template =
        Str.global_replace
          (Str.regexp
             {|\\\([a-zA-Z]+\){[a-zA-Z0-9_']+}\|\(\\hyperlink{[a-zA-Z_\-]*}{[a-zA-Z_\-]*}\)|})
          "" template
      in
      let blocks = Str.full_split template_var_regexp reduce_template in
      let unmatched_vars =
        List.fold_left
          (fun acc block ->
            match block with
            | Str.Text _ -> acc
            | Str.Delim var -> (
                match StringSet.find_opt var template_vars with
                | Some _ -> acc
                | None -> var :: acc))
          [] blocks
      in
      if Utils.list_is_empty unmatched_vars then ()
      else
        let msg =
          Format.sprintf
            "The prose template '%s' contains the following unmatched \
             variables: %s"
            template
            (String.concat ", " unmatched_vars)
        in
        raise (SpecError msg)

    let check_prose_template_for_definition_node defining_node =
      let prose_description = prose_description_for_node defining_node in
      let vars = vars_of_node defining_node in
      let () = check_prose_template_for_vars prose_description vars in
      match defining_node with
      | Node_Type _ | Node_TypeVariant _ | Node_Constant _ -> ()
      | Node_Relation def ->
          let prose_application = Relation.prose_application def in
          let () = check_prose_template_for_vars prose_application vars in
          ()

    let check defining_nodes =
      List.iter check_prose_template_for_definition_node defining_nodes
  end

  (** * A module for conservatively checking that all type terms are
      well-formed. That is each term that instantiates another defined type term
      is subsumed by it. *)
  module CheckTypeInstantiations : sig
    val check : definition_node StringMap.t -> elem list -> unit
    (** [check id_to_defining_node elems] conservatively checks that all type
        terms referenced in [elems] that instantiate type terms in the range of
        [id_to_defining_node] are also subsumed by them. The check assumes that
        [check_no_undefined_ids] has already been run. *)
  end = struct
    (** [subsumed sub_op super_op] is true if all values in the domain of
        [Operator { op = sub_op; term }] are also in the domain of
        [Operator { op = super_op; term }]. *)
    let operator_subsumed sub_op super_op =
      match (sub_op, super_op) with
      | Powerset, Powerset
      | Powerset_Finite, Powerset_Finite
      | Powerset_Finite, Powerset
      | List1, List1
      | List0, List0
      | List1, List0
      | Option, Option
      | Option, Powerset_Finite
      | Option, Powerset
      (* Option represents sets with 0 or 1 values while Powerset represents sets of _any_ number of values. *)
        ->
          true
      | _ -> false

    (** [subsumed id_to_defining_node sub super] conservatively tests whether
        all values in the domain of [sub] are also in the domain of [super]. *)
    let rec subsumed id_to_defining_node sub super =
      let equiv_singleton_tuple term =
        match term with
        | LabelledTuple
            { label_opt = None; components = [ (_, referenced_term) ] } ->
            referenced_term
        | _ -> term
      in
      let sub = equiv_singleton_tuple sub in
      let super = equiv_singleton_tuple super in
      match (sub, super) with
      | _, Label super_label ->
          let same_label =
            match sub with
            | Label sub_label -> String.equal sub_label super_label
            | _ -> false
          in
          same_label
          ||
          (* The case where [super_label] references a type. *)
          subsumed_term_type id_to_defining_node sub super_label
      | ( Operator { op = sub_op; term = _, sub_term },
          Operator { op = super_op; term = _, super_term } ) ->
          operator_subsumed sub_op super_op
          && subsumed id_to_defining_node sub_term super_term
      | ( LabelledTuple
            { label_opt = sub_label_opt; components = sub_components },
          LabelledTuple
            { label_opt = super_label_opt; components = super_components } ) ->
          Option.equal String.equal sub_label_opt super_label_opt
          && List.for_all2
               (fun (_, sub_term) (_, super_term) ->
                 subsumed id_to_defining_node sub_term super_term)
               sub_components super_components
      | ( LabelledRecord { label_opt = sub_label_opt; fields = sub_fields },
          LabelledRecord { label_opt = super_label_opt; fields = super_fields }
        ) ->
          Option.equal String.equal sub_label_opt super_label_opt
          && List.for_all2
               (fun (_, sub_term) (_, super_term) ->
                 subsumed id_to_defining_node sub_term super_term)
               sub_fields super_fields
      | ( Function { from_type = _, sub_from_term; to_type = _, sub_to_term },
          Function
            { from_type = _, super_from_term; to_type = _, super_to_term } ) ->
          subsumed id_to_defining_node sub_from_term super_from_term
          && subsumed id_to_defining_node sub_to_term super_to_term
      | ConstantsSet sub_names, ConstantsSet super_names ->
          List.for_all (fun name -> List.mem name super_names) sub_names
      | _ ->
          (* false is safely conservative. *)
          false

    (** [subsumed_term_type id_to_defining_node sub_term typename] checks if
        [sub_term] is subsumed by the type term defined by [typename] in
        [id_to_defining_node]. If [typename] is not defined in
        [id_to_defining_node], returns false. *)
    and subsumed_term_type id_to_defining_node sub_term typename =
      match StringMap.find_opt typename id_to_defining_node with
      | Some (Node_Type { Type.variants; _ }) ->
          List.exists
            (fun { TypeVariant.term = super_term } ->
              subsumed id_to_defining_node sub_term super_term)
            variants
      | _ -> false

    (** [check_subsumed id_to_defining_node sub super] checks that [sub] is
        subsumed by [super]. If not, a [SpecError] is raised. *)
    let check_subsumed id_to_defining_node sub super =
      if subsumed id_to_defining_node sub super then ()
      else
        let msg =
          Format.asprintf "The type term `%a` is not subsumed by `%a`"
            PP.pp_type_term sub PP.pp_type_term super
        in
        raise (SpecError msg)

    (** [check_subsumed_terms_lists id_to_defining_node term label sub_terms
         super_terms] checks that each term in [sub_terms] is subsumed by the
        corresponding term in [super_terms]. Both lists of terms comprise the
        labelled tuple/labelled record [term] with label [label]. If the lists
        have different lengths, or if any term in [sub_terms] is not subsumed by
        the corresponding term in [super_terms], a [SpecError] is raised. *)
    let check_subsumed_terms_lists id_to_defining_node term label sub_terms
        super_terms =
      if List.compare_lengths sub_terms super_terms = 0 then
        List.iter2 (check_subsumed id_to_defining_node) sub_terms super_terms
      else
        let msg =
          Format.asprintf
            "The type term `%a` cannot be instantiated since it has %i type \
             terms and `%s` requires %i type terms"
            PP.pp_type_term term (List.length sub_terms) label
            (List.length super_terms)
        in
        raise (SpecError msg)

    (** [is_constant id_to_defining_node id] checks if [id] is either defined as
        a constant directly or as a type variant with a label. *)
    let check_is_constant id_to_defining_node id =
      match StringMap.find_opt id id_to_defining_node with
      | Some (Node_Constant _) | Some (Node_TypeVariant { term = Label _ }) ->
          ()
      | _ ->
          let msg =
            Format.sprintf
              "%s is used as a constant even though it is not defined as one" id
          in
          raise (SpecError msg)

    (** [check_type_term id_to_defining_node term] checks that [term] is
        well-formed with respect to the type definitions in the range of
        [id_to_defining_node]. *)
    let rec check_type_term id_to_defining_node term =
      match term with
      | Operator { term = _, operator_term } ->
          check_type_term id_to_defining_node operator_term
      | LabelledTuple { label_opt; components } -> (
          let terms = List.map snd components in
          let () = List.iter (check_type_term id_to_defining_node) terms in
          match label_opt with
          | None -> ()
          | Some label -> (
              let variant_def = StringMap.find label id_to_defining_node in
              match variant_def with
              | Node_TypeVariant
                  {
                    TypeVariant.term =
                      LabelledTuple { components = def_opt_named_components };
                  } ->
                  let def_terms = List.map snd def_opt_named_components in
                  check_subsumed_terms_lists id_to_defining_node term label
                    terms def_terms
              | _ ->
                  let msg =
                    Format.asprintf
                      "The type term `%a` cannot be instantiated since '%s' is \
                       not a labelled tuple type"
                      PP.pp_type_term term label
                  in
                  raise (SpecError msg)))
      | LabelledRecord { label_opt; fields } -> (
          let terms = List.map snd fields in
          let () = List.iter (check_type_term id_to_defining_node) terms in
          match label_opt with
          | None -> ()
          | Some label -> (
              let variant_def = StringMap.find label id_to_defining_node in
              match variant_def with
              | Node_TypeVariant
                  {
                    TypeVariant.term = LabelledRecord { fields = def_fields; _ };
                  } ->
                  let def_terms = List.map snd def_fields in
                  check_subsumed_terms_lists id_to_defining_node term label
                    terms def_terms
              | _ ->
                  let msg =
                    Format.asprintf
                      "The type term `%a` cannot be instantiated since '%s' is \
                       not a labelled record type"
                      PP.pp_type_term term label
                  in
                  raise (SpecError msg)))
      | Function { from_type = _, from_term; to_type = _, to_term } ->
          check_type_term id_to_defining_node from_term;
          check_type_term id_to_defining_node to_term
      | ConstantsSet labels ->
          List.iter (check_is_constant id_to_defining_node) labels
      | Label label -> (
          let variant_def = StringMap.find label id_to_defining_node in
          match variant_def with
          | Node_TypeVariant _ | Node_Type _ -> ()
          | _ ->
              let msg =
                Format.asprintf
                  "The type term `%a` cannot be instantiated since '%s' is not \
                   a type or type variant"
                  PP.pp_type_term term label
              in
              raise (SpecError msg))

    let check id_to_defining_node elems =
      List.iter
        (function
          | Elem_Constant _ | Elem_Render _ -> ()
          | Elem_Relation { input; output } ->
              List.iter
                (fun (_, term) -> check_type_term id_to_defining_node term)
                input;
              List.iter (check_type_term id_to_defining_node) output
          | Elem_Type { Type.variants; _ } ->
              List.iter
                (fun { TypeVariant.term } ->
                  check_type_term id_to_defining_node term)
                variants)
        elems
  end
end

let from_ast ast =
  let definition_nodes = list_definition_nodes ast in
  let defined_ids = List.map definition_node_name definition_nodes in
  let id_to_defining_node = make_id_to_definition_node definition_nodes in
  let () = Check.check_no_undefined_ids ast id_to_defining_node in
  let () = Check.check_mandatory_attributes definition_nodes in
  let () = Check.check_math_layout definition_nodes in
  let () = Check.CheckProseTemplates.check definition_nodes in
  let defined_ids = List.rev defined_ids in
  { ast; id_to_defining_node; defined_ids }

let defined_ids self = self.defined_ids
