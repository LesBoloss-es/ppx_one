open Parsetree
open Ast_helper

let loc = Obj.magic ()

let parse_extension ext =
  let open String in
  match index_opt ext '.' with
  | None -> (false, ext)
  | Some i ->
    if sub ext 0 i = "one" then
      (true, sub ext (i + 1) (length ext - i - 1))
    else
      (false, ext)

let should_handle _ext = true

type fct = Bind | Catch | Raise

let fct_to_string = function
  | Bind -> "bind"
  | Catch -> "catch"
  | Raise -> "raise"

let fct_of_string_exn = function
  | "bind" -> Bind
  | "catch" -> Catch
  | "raise" | "throw" | "fail" -> Raise
  | _ -> failwith "fct_of_string_exn"

let name ext fct =
  Format.sprintf "ppx_one_%s_%s" ext (fct_to_string fct)

let lid s = Location.mknoloc (Longident.Lident s)

let default_mapper = Ast_mapper.default_mapper

module Def = struct
  (* This module contains the replacement of definitions of syntax extensions *)

  let structure_item ext stri =
    match stri.pstr_desc with
    | Pstr_value (Nonrecursive, [vb]) ->
      (
        match vb.pvb_pat.ppat_desc with
        | Ppat_var {txt; _} ->
          (
            let fct = fct_of_string_exn txt in
            Str.value Nonrecursive [Vb.mk (Pat.var (Location.mknoloc (name ext fct))) vb.pvb_expr]
          )

        | _ -> failwith "Def.structure_item: pattern not supported"
      )

    | _ -> failwith "Def.structure_item: payload not supported"
end

module Use = struct
  (* This module contains the replacement of uses of syntax extensions. *)

  let expr ext expr =
    match expr.pexp_desc with
    | Pexp_let (Nonrecursive, [vb], e) ->
      (* let x = e1 in e2  =>  bind e1 (fun x -> e2) *)
      Exp.apply
        (Exp.ident (lid (name ext Bind)))
        [Nolabel, vb.pvb_expr;
         Nolabel, Exp.fun_ Nolabel None vb.pvb_pat e]

    | Pexp_try (e, cs) ->
      (* try t with cs  =>  catch (fun () -> t) (fun e -> match e with w | _ -> raise e)) *)
      Exp.apply
        (Exp.ident (lid (name ext Catch)))
        [
          Nolabel, [%expr fun () -> [%e e]];

          Nolabel,
          Exp.fun_ Nolabel None
            [%pat? e]
            (Exp.match_
               [%expr e]
               (cs @
                [Exp.case
                   (Pat.any ())
                   (Exp.apply
                      (Exp.ident (lid (name ext Raise)))
                      [Nolabel, [%expr e]])]))
        ]

    | _ -> failwith "Use.expr: payload not supported"
end

module Mapper = struct
  let expr mapper expr =
    match default_mapper.expr mapper expr with

    | {pexp_desc=Pexp_extension ({txt; _}, payload); _} ->
      let (is_definition, extension) = parse_extension txt in
      if should_handle extension then
        (
          match payload with
          | PStr [{pstr_desc=Pstr_eval (expr, _); _}] ->
            if is_definition then
              failwith "Mapper.expr: PStr payload with is_definition"
            else
              Use.expr extension expr

          | _ -> failwith "Mapper.expr: payload not supported"
        )
      else if is_definition then
        failwith "Mapper.expr: should not handle but is definition"
      else
        expr

    | expr -> expr

  let structure_item mapper stri =
    match default_mapper.structure_item mapper stri with

    | {pstr_desc=Pstr_extension (({txt; _}, payload), _); _} ->
      let (is_definition, extension) = parse_extension txt in
      if should_handle extension then
        (
          match payload with
          | PStr [stri] ->
            if is_definition then
              Def.structure_item extension stri
            else
              failwith "Mapper.structure_item: payload but is not definition"

          | _ -> failwith "Mapper.structure_item: payload not supported"
        )
      else if is_definition then
        failwith "Mapper.structure_item: should not handle, but is definition"
      else
        stri

    | stri -> stri

  let m _config _cookie =
    { default_mapper with expr; structure_item }
end

let () =
  Migrate_parsetree.(
    Driver.register
      ~name:"ppx_one"
      Versions.ocaml_407
      Mapper.m
  )
