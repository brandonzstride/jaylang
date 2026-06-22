
type 'k t = { value : 'a. ('a, 'k) Symbol.t -> 'a option }

let empty : 'k t = { value = fun _ -> None }

let merge (s1 : 'k t) (s2 : 'k t) : 'k t =
  let value : type a. (a, 'k) Symbol.t -> a option = fun sym ->
    match s1.value sym with
    | None -> s2.value sym
    | v -> v
  in
  { value }

let singleton (type a) (a : a) (s : (a, 'k) Symbol.t) : 'k t =
  let value (type b) (s' : (b, 'k) Symbol.t) : b option =
    match s, s' with
    | I uid, I uid' when uid = uid' -> Some a
    | B uid, B uid' when uid = uid' -> Some a
    | _ -> None
  in
  { value }
