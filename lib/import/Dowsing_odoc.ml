(** Load code from Odoc, partially taken from sherlodoc *)

let index_file register filename =
  let open Odoc_index in
  let open Odoc_utils in
  (match Odoc_odoc.Odoc_file.load filename with
   | Ok { content = Page_content p; _ } ->
     Tree.iter ~f:register (Skeleton.from_page p)
   | Ok { content = Unit_content u; _ } ->
     Tree.iter ~f:register (Skeleton.from_unit u)
   | Ok _ -> ()
   | Error (`Msg _msg) ->
     ()
     (* Format.printf "Odoc warning or error %a: %s@." Fpath.pp filename msg *)
  )

module OM = Odoc_model
module P = OM.Paths

let rec oid_of_lid l =
  match l with
  | [] -> assert false
  (* Chop path starting with [Stdlib]
     TODO: handler opening properly
  *)
  | "Stdlib" :: t ->
    oid_of_lid t
  | h :: t ->
    List.fold_left (fun acc s -> Outcometree.Oide_dot (acc, s))
      (Outcometree.Oide_ident {printed_name = h})
      t

let rec lid_of_odoc_path : P.Path.Type.t -> _ = function
  | `Resolved t ->
    (* identifier now returns option: None for e.g. functor-application paths *)
    (match P.Path.Resolved.(identifier (t :> t)) with
     | Some id -> P.Identifier.fullname id
     | None -> failwith "TODO: fix me Dowsing_odoc.lid_of_odoc_path") (* TODO: I don't know what should be done here. *)
  | `Identifier (path, _hidden) ->
    P.Identifier.fullname path
  | `DotT (mdl, x) ->
    Odoc_document.Url.render_path (mdl :> P.Path.t) :: [OM.Names.TypeName.to_string x]
  | `SubstitutedT path ->
    (* Substituted path (e.g. from functor application): recurse on the inner path *)
    lid_of_odoc_path path

let rec odoc_to_outcometree
    (odoc : Odoc_model.Lang.TypeExpr.t) : Outcometree.out_type =
  match odoc with
   | Var v -> Otyp_var (false, v)
   | Any -> Otyp_abstract
   | Alias (ty, _) ->
     odoc_to_outcometree ty
   | Arrow (lbl, arg, res) ->
     let arg = odoc_to_outcometree arg in
     let res = odoc_to_outcometree res in
     let lbl = match lbl with
       | None -> Asttypes.Nolabel
       | Some (Label s) -> Labelled s
       | Some (Optional s) -> Optional s
       | Some (RawOptional s) -> Optional s (* TODO: is this correct? *)
     in
     Otyp_arrow (lbl, arg, res)
   | Tuple l ->
     (* OCaml 5.4 introduce labeled tuple. Tuple now carries optional per-element labels *)
     Otyp_tuple (List.map (fun (_lbl, ty) -> odoc_to_outcometree ty) l)
   | Constr (c, params) ->
     let c = lid_of_odoc_path c in
     let params = List.map odoc_to_outcometree params in
     Otyp_constr (oid_of_lid c, params)
   | Polymorphic_variant { kind; elements } ->
     let closed, bound = match kind with
         | Fixed -> true, None
         | Closed l -> true, Some l
         | Open -> false, None
     in
     let elements =
       match elements with
       | [ Type ty ] -> Outcometree.Ovar_typ (odoc_to_outcometree ty)
       | l ->
         let l =
           (* TODO This is incorrect *)
           List.filter_map (function
                 OM.Lang.TypeExpr.Polymorphic_variant.Constructor
                   { name; constant; arguments; _ } ->
                 Some (name, constant, List.map odoc_to_outcometree arguments)
               | _ -> None
             ) l
         in
         Outcometree.Ovar_fields l
     in 
     Otyp_variant (elements, closed, bound)
   | Object _ ->
     Otyp_stuff "object" (*TODO*)
   | Class (_, _) -> 
     Otyp_stuff "class" (*TODO*)
   | Poly (params, ty) ->
     Otyp_poly (params, odoc_to_outcometree ty)
   | Package { path=_; substitutions=_ } ->
     Otyp_stuff "package" (*TODO*)
     (* let substitutions = *)
     (*   List.map (fun ((field : P.Fragment.Type.t), ty) -> *)
     (*       let field = Odoc_document.Url.render_path *)
     (*           (field :> P.Fragment.leaf) *)
     (*       in *)
     (*       let ty = odoc_to_outcometree ty in *)
     (*       (field, ty) *)
     (*     ) substitutions *)
     (* in *)
     (* let lid = oid_of_lid [Odoc_document.Url.render_path (path :> P.Path.t)] in *)
     (* Otyp_module (lid, substitutions)  *)
   | OM.Lang.TypeExpr.Unboxed_tuple l ->
     (* OxCaml feature: unboxed tuple #(int * string): no separate Outcometree constructor,
        treat as a regular tuple for search purposes *)
     Otyp_tuple (List.map (fun (_lbl, ty) -> odoc_to_outcometree ty) l)
   | OM.Lang.TypeExpr.Quote t
   | OM.Lang.TypeExpr.Splice t ->
     (* OxCaml feature: recurse through *)
     odoc_to_outcometree t
   | OM.Lang.TypeExpr.Arrow_functor (_lbl, _mdl, _res) ->
     failwith "not implemented" (* TODO: what are arrow functor? New in OCaml 5.5? Modular explicit? *)

let of_entry_kind (kind : Odoc_index.Entry.kind) =
  let module O = Outcometree in
  let format_args args = match args with
    | Odoc_model.Lang.TypeDecl.Constructor.Tuple l ->
      List.map (fun ty -> Asttypes.Nolabel,odoc_to_outcometree ty) l
    | Record l ->
      List.map
        (fun (field : Odoc_model.Lang.TypeDecl.Field.t) ->
           Asttypes.Labelled (Odoc_model.Paths.Identifier.name field.id),
           odoc_to_outcometree field.type_)
        l
  in
  let mk_arrow args res = 
      List.fold_right
        (fun (n,a) acc -> O.Otyp_arrow (n, a, acc))
        args
        res
  in  
  match kind with
  | TypeDecl _ ->
    None
  | Value { value = _; type_ } ->
    Some (odoc_to_outcometree type_)
  | Constructor { args; res }
  | ExtensionConstructor { args; res }
  | Exception { args; res } ->
    let args = format_args args in
    let res = odoc_to_outcometree res in
    let ty = mk_arrow args res in
    Some ty
  | Field { mutable_ = _; parent_type; type_ }
  | UnboxedField { mutable_ = _; parent_type; type_ } ->
    let arg = odoc_to_outcometree parent_type in
    let res = odoc_to_outcometree type_ in
    let ty = mk_arrow [Nolabel, arg] res in
    Some ty
  | Doc
  | Class_type _
  | Method _
  | Class _
  | TypeExtension _
  | Module _
  | ModuleType _
  | Page _
  | Impl
  | Dir
    ->
    None

let of_entry pkg source_file (x : Odoc_index.Entry.t) =
  let open CCOption.Infix in
  let+ ty = of_entry_kind x.kind in
  let lid =
    LongIdent.of_list @@
    Odoc_model.Paths.Identifier.fullname x.id
  in
  {Db.Entry. lid ; ty ; pkg; source_file }

let iter pkgs k =
  let register ~pkg ~source_file entry =
    CCOption.iter k @@ of_entry pkg source_file entry
  in
  let loop (pkg, pkg_files) =
    List.iter
      (fun source_file -> index_file (register ~pkg ~source_file) source_file)
      pkg_files
  in
  List.iter loop pkgs
