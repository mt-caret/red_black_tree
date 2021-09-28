open Core
open Quickcheck

module Red_black_tree = struct
  include Red_black_tree

  let of_int_list =
    List.fold
      ~init:Red_black_tree.empty
      ~f:(insert ~compare:Int.compare ~sexp_of_a:Int.sexp_of_t)
  ;;

  let generator =
    List.quickcheck_generator Int.quickcheck_generator |> Generator.map ~f:of_int_list
  ;;
end

let%test_unit "trees are valid" =
  Quickcheck.test
    Red_black_tree.generator
    ~sexp_of:[%sexp_of: int Red_black_tree.t]
    ~f:(Red_black_tree.validate ~compare:Int.compare ~sexp_of_a:Int.sexp_of_t)
;;

let%test_unit "after insertion, the value exists" =
  Generator.both Red_black_tree.generator Int.quickcheck_generator
  |> Quickcheck.test
       ~sexp_of:[%sexp_of: int Red_black_tree.t * int]
       ~f:
         ([%test_pred: int Red_black_tree.t * int] (fun (tree, x) ->
              let tree' =
                Red_black_tree.insert tree x ~compare:Int.compare ~sexp_of_a:Int.sexp_of_t
              in
              Red_black_tree.mem tree' x ~compare:Int.compare))
;;

let%test_unit "to_list results in a sorted list" =
  Quickcheck.test
    Red_black_tree.generator
    ~sexp_of:[%sexp_of: int Red_black_tree.t]
    ~f:
      ([%test_pred: int Red_black_tree.t] (fun tree ->
           Red_black_tree.to_list tree |> List.is_sorted_strictly ~compare:Int.compare))
;;
