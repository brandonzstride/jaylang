
open Lang
open Ast

module Fresh_names = struct
  module type S = sig
    val fresh_id : ?suffix:string -> unit -> Ident.t
    val fresh_poly_value : unit -> int
  end

  module Make () : S = struct
    (* suffixes are strictly for readability of target code *)
    let fresh_id : ?suffix : string -> unit -> Ident.t =
      let count = Utils.Counter.create () in
      fun ?(suffix : string = "") () ->
        let c = Utils.Counter.next count in
        Ident (Format.sprintf "~%d%s" c suffix)

    let fresh_poly_value : unit -> int =
      let count = Utils.Counter.create () in
      fun () -> Utils.Counter.next count
  end
end

module Let_builder (L : sig
  type a
  type t
  val t_to_expr : t -> body:a Expr.t -> a Expr.t
end) = struct
  type tape = L.t list

  (* for efficiency, use stateful "tape" *)
  type 'a m = tape -> 'a * tape

  let bind x f =
    fun s ->
      let a, s' = x s in
      f a s'

  let ( let* ) = bind

  let return a = fun s -> a, s

  let tell a = fun s -> (), a :: s

  let iter (ls : 'a list) ~(f : 'a -> unit m) : unit m =
    List.fold_left (fun acc_m a ->
      let* () = acc_m in f a
    ) (return ()) ls

  let build (m : L.a Expr.t m) : L.a Expr.t =
    let body, bindings = m [] in
    List.fold_left (fun body tape -> L.t_to_expr tape ~body) body bindings
end

open Ast_tools

module Desugared_functions = struct
  (*
    let filter_list x =
      match x with
      | `Nil _ -> x
      | `Cons _ -> x
      end
  *)
  let filter_list : Desugared.t =
    let x = Ident.Ident "x" in
    EFunction { param = x ; body =
      EMatch { subject = EVar x ; patterns =
        [ (PVariant
            { variant_label = Reserved.nil
            ; payload_id = Reserved.catchall }
          , EVar x)
        ; (PVariant
            { variant_label = Reserved.cons
            ; payload_id = Reserved.catchall }
          , EVar x)
        ]
      }
    }

  (*
    Generic Y-combinator for one function.

      fun f ->
        (fun s -> fun x -> f (s s) x)
        (fun s -> fun x -> f (s s) x)
  *)
  let y_1 =
    let open Ident in
    let open Expr in
    let open Ast_tools.Utils in
    let f = Ident "~f_y1" in
    let s = Ident "~s_y1" in
    let x = Ident "~x_y1" in
    let body =
      abstract_over_ids [ s ; x ] @@
        appl_list (EVar f) ([ apply (EVar s) (EVar s) ; EVar x ])
    in
    abstract_over_ids [ f ] @@
      apply body body

  (*
    Y-combinator for n functions identified by `names`.

      fun f1 ... fn ->
        Y (fun self f1 ... fn ->
          { l1 = fun x ->
            let r = self f1 ... fn in
            f1 r.f1 ... r.fn x
          ; ...
          ; ln = fun x ->
            let r = self f1 ... fn in
            fn r.f1 ... r.fn x
          }
        ) f1 ... fn
  *)
  let y_n = function
    | [] -> failwith "Invalid Y-combinator on 0 functions"
    | [ f ] ->
      (* only one function so can use the simple y_1 combinator *)
      let open Ast_tools.Utils in
      abstract_over_ids [ f ] @@
        let appl_y1 = apply y_1 (EVar f) in
        Ast.Expr.ERecord (RecordLabel.Map.singleton (RecordLabel.RecordLabel f) appl_y1)
    | ids ->
      let open Ident in
      let open Expr in
      let open Ast_tools.Utils in
      let self = Ident "~self_yn" in
      let x = Ident "~x_yn" in
      let r = Ident "~r_yn" in
      let e_ids = List.map (fun id -> EVar id) ids in
      let labels = List.map (fun id -> RecordLabel.RecordLabel id) ids in
      let projections = List.map (fun label -> proj (EVar r) label) labels in
      abstract_over_ids ids (
        appl_list (
          apply y_1 @@
            abstract_over_ids (self :: ids) @@
              ERecord (Ast.RecordLabel.Map.of_list @@
                let bodies =
                  List.map (fun f ->
                    abstract_over_ids [ x ] @@
                      ELet { var = r ; defn = appl_list (EVar self) e_ids ; body =
                        apply (appl_list (EVar f) projections) (EVar x)
                      }
                  ) ids
                in
                List.combine labels bodies
              )
        ) e_ids
      )

end

module Embedded_functions = struct
  (*
    Y-combinator for Mu types:

      fun f ->
        (fun x -> freeze (thaw (f (x x))))
        (fun x -> freeze (thaw (f (x x))))

    Notes:
    * f is a function, so it has be captured with a closure, so there is nothing
      wrong about using any names here. However, I use tildes to be safe and make
      sure they're fresh.
    * This y-combinator is unconventional in that it uses freeze and thaw instead of
      passing an argument. This is because we know the use case is for mu types.
  *)
  let y_freeze_thaw =
    let open Ident in
    let open Expr in
    let open Ast_tools.Utils in
    let f = Ident "~f_y_freeze_thaw" in
    let x = Ident "~x_y_freeze_thaw" in
    let body =
      abstract_over_ids [x] @@
        freeze (thaw (
          apply (EVar f) (apply (EVar x) (EVar x))
        ))
    in
    abstract_over_ids [ f ] @@
      apply body body

  (*
    Generic Y-combinator for one function.

      fun f ->
        (fun s -> fun x -> f (s s) x)
        (fun s -> fun x -> f (s s) x)
  *)
  let y_1 =
    let open Ident in
    let open Expr in
    let open Ast_tools.Utils in
    let f = Ident "~f_y1" in
    let s = Ident "~s_y1" in
    let x = Ident "~x_y1" in
    let body =
      abstract_over_ids [ s ; x ] @@
        appl_list (EVar f) ([ apply (EVar s) (EVar s) ; EVar x ])
    in
    abstract_over_ids [ f ] @@
      apply body body
end
