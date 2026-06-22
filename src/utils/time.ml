
let time f x =
  let c = Mtime_clock.counter () in
  let res = f x in
  Mtime_clock.count c, res

let span_to_ms =
  let ms_over_ns = Mtime.Span.to_float_ns Mtime.Span.ms /. Mtime.Span.to_float_ns Mtime.Span.ns in
  fun span ->
    Mtime.Span.to_float_ns span /. ms_over_ns

let divide_span span n =
  Option.get @@
  Mtime.Span.of_float_ns (Mtime.Span.to_float_ns span /. Int.to_float n)

let of_float_sec float_sec =
  let s_per_ns = Mtime.Span.to_float_ns Mtime.Span.s /. Mtime.Span.to_float_ns Mtime.Span.ns in
  Mtime.Span.of_float_ns (float_sec *. s_per_ns)
let argv_span_conv =
  Cmdliner.Arg.Conv.make ()
    ~docv:"SPAN_SEC"
    ~parser:(fun s ->
        let span_opt = Option.bind (Float.of_string_opt s) of_float_sec in
        match span_opt with
        | Some span -> Ok span
        | None -> Error "Invalid time span as float seconds"
      )
    ~pp:Mtime.Span.pp

type _ Effect.t += Check_timeout : unit Effect.t

let yield_to_timer () = Effect.perform Check_timeout

let with_timeout span f a =
  let timer = Mtime_clock.counter () in
  try
    Ok (f a)
  with
  | effect Check_timeout, k ->
    let t = Mtime_clock.count timer in
    if Mtime.Span.is_longer t ~than:span then
      let exception Timeout in
      try Effect.Deep.discontinue k Timeout with
      | Timeout -> Error t
    else
      Effect.Deep.continue k ()
