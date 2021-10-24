open Core
open Red_black_tree
open Quickcheck
open Base_quickcheck

module Op = struct
  type t = int * [ `Get | `Set of int ] [@@deriving sexp]

  let generator ~len =
    let open Generator.Let_syntax in
    let%map index = Generator.int_inclusive 0 (len - 1)
    and op =
      Generator.union [ return `Get; Generator.map Generator.int ~f:(fun x -> `Set x) ]
    in
    index, op
  ;;

  let ops_generator ~len = Generator.list (generator ~len)
end

let nonempty_int_array_gen =
  Generator.list Generator.int
  |> Generator.filter ~f:(Fn.non List.is_empty)
  |> Generator.map ~f:Array.of_list
;;

module Test (L : Persistent_array_intf.S) (R : Persistent_array_intf.S) = struct
  let%test_unit "ops match" =
    (let%bind.Generator arr = nonempty_int_array_gen in
     let%map.Generator ops = Op.ops_generator ~len:(Array.length arr) in
     ops, arr)
    |> Quickcheck.test ~sexp_of:[%sexp_of: Op.t list * int array] ~f:(fun (ops, arr) ->
           let l, r =
             List.fold
               ops
               ~init:(L.of_array arr, R.of_array arr)
               ~f:
                 (fun (l, r) -> function
                   | index, `Get ->
                     [%test_eq: int] (L.get l index) (R.get r index);
                     l, r
                   | index, `Set x -> L.set l index x, R.set r index x)
           in
           [%test_eq: int Array.t] (L.to_array l) (R.to_array r))
  ;;

  let%test_unit "(linear) persistence" =
    (let%bind.Generator arr = nonempty_int_array_gen in
     let%map.Generator ops = Op.ops_generator ~len:(Array.length arr) in
     ops, arr)
    |> Quickcheck.test ~sexp_of:[%sexp_of: Op.t list * int array] ~f:(fun (ops, arr) ->
           List.fold_map
             ops
             ~init:(L.of_array arr, R.of_array arr)
             ~f:
               (fun (l, r) -> function
                 | _, `Get -> (l, r), (l, r)
                 | index, `Set x ->
                   let l = L.set l index x in
                   let r = R.set r index x in
                   (l, r), (l, r))
           |> snd
           |> List.iter ~f:(fun (l, r) ->
                  [%test_eq: int array] (L.to_array l) (R.to_array r)))
  ;;
end

include Test (Persistent_array.Naive) (Persistent_array.Most_naive)
include Test (Persistent_array.Naive2) (Persistent_array.Most_naive)
include Test (Persistent_array.Naive3) (Persistent_array.Most_naive)
