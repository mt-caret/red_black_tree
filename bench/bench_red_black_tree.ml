open Core
open Core_bench

let size = 10_000
let compare = Int.compare

let create () =
  List.init size ~f:Fn.id |> Red_black_tree.of_list ~compare ~sexp_of_a:Int.sexp_of_t
;;

let core_set_create () = List.init size ~f:Fn.id |> Int.Set.of_list

let () =
  let t = create () in
  let set = core_set_create () in
  let to_check = List.init (size * 2) ~f:Fn.id in
  Command.run
    (Bench.make_command
       [ Bench.Test.create ~name:"Red_black_tree.of_list" create
       ; Bench.Test.create ~name:"Core.Set.of_list" core_set_create
       ; Bench.Test.create ~name:"Red_black_tree.mem" (fun () ->
             List.iter to_check ~f:(fun x -> Red_black_tree.mem t x ~compare |> ignore))
       ; Bench.Test.create ~name:"Core.Set.mem" (fun () ->
             List.iter to_check ~f:(fun x -> Set.mem set x |> ignore))
       ])
;;
