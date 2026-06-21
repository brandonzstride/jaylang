
module type S = sig
  type t
  val initial : t
  val push : t -> t
  val increment : t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val uid : t -> int
end

module Simple : S = struct
  module T = struct
    type t =
      | Timestamp of int list [@@unboxed]

    let equal (Timestamp a) (Timestamp b) =
      List.equal Int.equal a b
  end

  include T
  let initial = Timestamp [1]
  let push (Timestamp xs) = Timestamp (1::xs)
  let increment = function
    | Timestamp (x::xs) -> Timestamp ((x+1)::xs)
    | Timestamp [] -> failwith "Invariant broken: empty timestamp"
  let compare (Timestamp xs1) (Timestamp xs2) =
    List.compare compare (List.rev xs1) (List.rev xs2)
  let to_string (Timestamp xs) =
    String.concat "." (List.rev_map string_of_int xs)

  let empty_hash = Hashtbl.hash []

  (* The standard library polymorphic hash has collisions after length 10, so we derive hash for uid *)
  let uid (Timestamp ls) =
    List.fold_left (fun acc a -> Hashtbl.hash (acc, a)) empty_hash ls
end

module PerfectHash : S = struct
  type t = int

  let equal = Int.equal
  let compare = Int.compare

  type entry = {
    mutable pushed : int;
    mutable incremented : int;
    time : int list;
  }

  let invalid = -1 (* sentinel for uninitialized indices *)

  let table : entry Vector.t =
    (* Create vector *)
    let v =
      Vector.create
        ~dummy:({pushed=invalid; incremented=invalid; time=[]})
    in
    (* Initialize first timestamp *)
    Vector.push v {pushed=invalid; incremented=invalid; time=[1]};
    v
  ;;

  let initial = 0 (* first timestamp is already initialized *)

  let push (time : t) : t =
    let entry = Vector.get table time in
    if entry.pushed <> invalid then entry.pushed else begin
      let entry' = {pushed=invalid; incremented=invalid; time=1::entry.time} in
      let entry'_idx = Vector.length table in
      Vector.push table entry';
      entry.pushed <- entry'_idx;
      entry'_idx
    end

  let increment (time : t) : t =
    let entry = Vector.get table time in
    if entry.incremented <> invalid then entry.incremented else begin
      let time' = match entry.time with
        | x::xs -> (x+1)::xs
        | [] -> failwith "Invariant broken: empty time list"
      in
      let entry' = {pushed=invalid; incremented=invalid; time=time'} in
      let entry'_idx = Vector.length table in
      Vector.push table entry';
      entry.incremented <- entry'_idx;
      entry'_idx
    end

  let to_string (time : t) : string =
    let entry = Vector.get table time in
    String.concat "." (List.rev_map string_of_int entry.time)

  let uid x = x
end

(* Select a default implementation *)
include Simple

module Map = Baby.W.Map.Make (Simple)
