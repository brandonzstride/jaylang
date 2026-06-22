(**
  Module [Metadata].

  This is one module to contain the full test medatadata for
  a Bluejay test program.

  The metadata is stored in a comment at the top of the Bluejay
  file as follows

    (***
      (
        (features (<test feature list>))
        (reasons (<test reason list>))
        (speed <Fast or Slow>)
        (typing <Well_typed or Ill_typed or Exhausted>)
        (flags "<some string containing the argv flags to ceval>")
      )
    *)

  The triple asterisk in the comment opener is the key that the
  comment contains the tests metadata.

  All of these are optional. If absent, the test is assumed to be
  well-typed and run slow. The fields can be in any order.

  Note the reasons must be empty if the test is well-typed because
  they are the reasons for the error, which does not exist.
*)

open Sexplib.Conv

module Test_speed = struct
  type t = Fast | Slow [@@deriving sexp]
end

module Typing = struct
  type t = Well_typed | Ill_typed | Exhausted [@@deriving sexp]
  (*
    Well-typed : no error gets found
    Ill-typed  : some error gets found
    Exhausted  : the program is proven well-typed by exhausting all paths
  *)
end

module Flags = struct
  type t = string array

  (* We dont want to write flags as an array. Just a string is fine *)
  let sexp_of_t flags =
    sexp_of_string (String.concat " " (Array.to_list flags))

  let t_of_sexp sexp =
    let str = string_of_sexp sexp in
    let parts = String.split_on_char ' ' str |> List.filter (fun s -> not (String.is_empty s)) in
    Array.of_list parts
end

type t =
  { features : Ttag.t list  [@default []]
  ; reasons  : Ttag.t list  [@default []]
  ; speed    : Test_speed.t [@default Fast]
  ; typing   : Typing.t     [@default Exhausted]
  ; flags    : Flags.t      [@default [||]]
  } [@@deriving sexp]

let tags_of_t (r : t) : [ `Sorted_list of [ `Feature of Ttag.t | `Reason of Ttag.t | `Absent ] list ] =
  `Sorted_list (
    Ttag.all
    |> List.map (fun tag ->
      let mem ls = List.exists (Ttag.equal tag) ls in
      if mem r.reasons
      then begin
        (* first need to assert that features are a subset of reasons *)
        if not @@ mem r.features
        then failwith @@ Format.sprintf "Tag %s found in reasons but not features" (Ttag.to_name tag)
        else `Reason tag
      end
      else
        if mem r.features
        then `Feature tag
        else `Absent
    )
  )

let of_bjy_file (bjy_filename : string) : t =
  (* may consider reading only a short portion to make this next line faster *)
  let file_content = In_channel.with_open_bin bjy_filename In_channel.input_all in
  let s_opt =
    let (let*) x f = Option.bind x f in
    let* i0 = String.find_first file_content ~sub:"(***" in
    let* i1 = String.find_first file_content ~start:i0 ~sub:"*)" in
    let i = i0 + 4 in
    let n = i1 - i in
    Some (String.sub file_content i n)
  in
  Option.value s_opt ~default:"()"
  |> Sexplib.Sexp.of_string
  |> t_of_sexp
