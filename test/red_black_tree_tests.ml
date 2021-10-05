open Core
open Quickcheck

let compare = Int.compare
let sexp_of_a = Int.sexp_of_t

module Red_black_tree = struct
  let of_int_list = Red_black_tree.of_list ~compare ~sexp_of_a

  let generator =
    Generator.union
      [ Red_black_tree.int_generator
      ; List.quickcheck_generator Int.quickcheck_generator |> Generator.map ~f:of_int_list
      ]
  ;;

  include Red_black_tree
end

let%test_unit "trees are valid" =
  Quickcheck.test
    Red_black_tree.generator
    ~sexp_of:[%sexp_of: int Red_black_tree.t]
    ~f:(Red_black_tree.validate ~compare ~sexp_of_a)
;;

let insert = Red_black_tree.insert ~compare ~sexp_of_a
let mem = Red_black_tree.mem ~compare

let%test_unit "after insertion, the value exists" =
  Generator.both Red_black_tree.generator Int.quickcheck_generator
  |> Quickcheck.test
       ~sexp_of:[%sexp_of: int Red_black_tree.t * int]
       ~f:
         ([%test_pred: int Red_black_tree.t * int] (fun (tree, x) ->
              mem (insert tree x) x))
;;

let%test_unit "insertion doesn't remove elements" =
  Generator.both Red_black_tree.generator Int.quickcheck_generator
  |> Quickcheck.test
       ~sexp_of:[%sexp_of: int Red_black_tree.t * int]
       ~f:
         ([%test_pred: int Red_black_tree.t * int] (fun (tree, x) ->
              Red_black_tree.to_list tree
              |> List.for_all ~f:(fun y -> mem (insert tree x) y)))
;;

let%test_unit "insertion only adds elements" =
  Generator.both Red_black_tree.generator Int.quickcheck_generator
  |> Quickcheck.test
       ~sexp_of:[%sexp_of: int Red_black_tree.t * int]
       ~f:
         ([%test_pred: int Red_black_tree.t * int] (fun (tree, x) ->
              let length = Red_black_tree.length tree in
              let length' = Red_black_tree.length (insert tree x) in
              if mem tree x then length = length' else length + 1 = length'))
;;

let%test_unit "to_list results in a sorted list" =
  Quickcheck.test
    Red_black_tree.generator
    ~sexp_of:[%sexp_of: int Red_black_tree.t]
    ~f:
      ([%test_pred: int Red_black_tree.t] (fun tree ->
           Red_black_tree.to_list tree |> List.is_sorted_strictly ~compare))
;;

let%test_unit "min_elt" =
  Quickcheck.test
    Red_black_tree.generator
    ~sexp_of:[%sexp_of: int Red_black_tree.t]
    ~f:(fun t ->
      [%test_eq: int option]
        (Red_black_tree.to_list t |> List.min_elt ~compare)
        (Red_black_tree.min_elt t))
;;

let%test_unit "max_elt" =
  Quickcheck.test
    Red_black_tree.generator
    ~sexp_of:[%sexp_of: int Red_black_tree.t]
    ~f:(fun t ->
      [%test_eq: int option]
        (Red_black_tree.to_list t |> List.max_elt ~compare)
        (Red_black_tree.max_elt t))
;;
