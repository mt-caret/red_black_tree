open Core

module Color = struct
  type t =
    | Red
    | Black
  [@@deriving sexp, compare, equal]
end

(* Invariants:
 * - No red node has a red child.
 * - Every path from the root to an empty node contains the same number of
 *   black nodes.
 *)
type 'a t =
  | Empty
  | Tree of Color.t * 'a t * 'a * 'a t
[@@deriving sexp, compare, equal]

let empty = Empty

let to_list t =
  let rec go t accum =
    match t with
    | Empty -> accum
    | Tree (_, l, x, r) -> go l (x :: go r accum)
  in
  go t []
;;

let rec mem t x ~compare =
  match t with
  | Empty -> false
  | Tree (_, l, y, r) ->
    (match Ordering.of_int (compare x y) with
    | Less -> mem l x ~compare
    | Equal -> true
    | Greater -> mem r x ~compare)
;;

let validate t ~compare ~sexp_of_a =
  let rec go = function
    | Empty -> 0, None
    | (Tree (Red, Tree (Red, _, _, _), _, _) | Tree (Red, _, _, Tree (Red, _, _, _))) as t
      -> raise_s [%message "subtree has red node with a red child" (t : a t)]
    | Tree (color, l, x, r) ->
      let l_num, l_minmax = go l in
      let r_num, r_minmax = go r in
      if l_num <> r_num
      then
        raise_s
          [%message
            "number of black nodes in subpaths differ"
              (l : a t)
              (l_num : int)
              (r : a t)
              (r_num : int)];
      let black_nodes =
        match color with
        | Black -> l_num + 1
        | Red -> l_num
      in
      let min_max =
        match l_minmax, r_minmax with
        | None, None -> x, x
        | Some (l_min, l_max), Some (r_min, r_max) ->
          let ( < ) a b = Ordering.equal Less (Ordering.of_int (compare a b)) in
          if not (l_max < x && x < r_min)
          then
            raise_s
              [%message
                "binary tree has inbalance"
                  (l : a t)
                  (l_max : a)
                  (x : a)
                  (r : a t)
                  (r_min : a)];
          l_min, r_max
        | Some min_max, None | None, Some min_max -> min_max
      in
      black_nodes, Some min_max
  in
  ignore (go t : _ * _ option)
;;

(*
let lbalance = function
  | Tree (Black, Tree (Red, Tree (Red, a, x, b), y, c), z, d)
  | Tree (Black, Tree (Red, a, x, Tree (Red, b, y, c)), z, d) ->
    Tree (Black, Tree (Black, a, x, b), y, Tree (Black, c, z, d))
  | t -> t
;;

let rbalance = function
  | Tree (Black, a, x, Tree (Red, Tree (Red, b, y, c), z, d))
  | Tree (Black, a, x, Tree (Red, b, y, Tree (Red, c, z, d))) ->
    Tree (Black, Tree (Black, a, x, b), y, Tree (Black, c, z, d))
  | t -> t
;;
*)

let balance = function
  | Tree (Black, Tree (Red, Tree (Red, a, x, b), y, c), z, d)
  | Tree (Black, Tree (Red, a, x, Tree (Red, b, y, c)), z, d)
  | Tree (Black, a, x, Tree (Red, Tree (Red, b, y, c), z, d))
  | Tree (Black, a, x, Tree (Red, b, y, Tree (Red, c, z, d))) ->
    Tree (Black, Tree (Black, a, x, b), y, Tree (Black, c, z, d))
  | t -> t
;;

let insert t x ~compare ~sexp_of_a =
  let rec go = function
    | Empty -> Tree (Red, Empty, x, Empty)
    | Tree (color, l, y, r) as t ->
      (match Ordering.of_int (compare x y) with
      | Less -> balance (Tree (color, go l, y, r))
      | Equal -> t
      | Greater -> balance (Tree (color, l, y, go r)))
  in
  match go t with
  | Empty -> raise_s [%message "should not happen" (t : a t) (x : a)]
  | Tree (_, l, y, r) -> Tree (Black, l, y, r)
;;

let%expect_test "test" =
  print_string "Hello, World";
  [%expect {| Hello, World |}]
;;
