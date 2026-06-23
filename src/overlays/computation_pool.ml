
open Moonpool

module type COMPUTE_RESULT = sig
  include Utils.Types.MONOID
  val is_signal_to_quit : t -> bool
end

let process_all (module C : COMPUTE_RESULT) (run : 'a -> C.t) (ls : 'a Nel.t) =
  (* We create one thread per work item, but it may be recommended to do fewer if this number is huge *)
  let pool = Ws_pool.create ~num_threads:(Nel.length ls) () in
  let futures =
    Nel.to_list ls
    |> List.map (fun item ->
      Fut.spawn ~on:pool (fun () -> run item)
    )
  in
  let rec go acc unfinished futures =
    match futures with
    | [] ->
      if List.is_empty unfinished
      then acc (* totally done with work *)
      else go acc [] unfinished (* done with this pass, so begin new pass *)
    | promise :: tl ->
      if Fut.is_done promise
      then begin
        let res = Fut.await promise in
        if C.is_signal_to_quit res
        then res
        else go (C.combine res acc) unfinished tl
      end
      else go acc (promise :: unfinished) tl
  in
  let res = go C.neutral [] futures in
  Ws_pool.shutdown_without_waiting pool;
  res
