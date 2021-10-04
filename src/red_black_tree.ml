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
    | Tree (_, l, x, r) -> (go [@tailcall]) l (x :: go r accum)
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

let rec min_elt t =
  match t with
  | Empty -> None
  | Tree (_, Empty, x, _) -> Some x
  | Tree (_, l, _, _) -> min_elt l
;;

let rec max_elt t =
  match t with
  | Empty -> None
  | Tree (_, _, x, Empty) -> Some x
  | Tree (_, _, _, r) -> max_elt r
;;

let validate t ~compare ~sexp_of_a =
  let rec go = function
    | Empty -> 1, None
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
                "binary tree has imbalance"
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

let lbalance = function
  | Tree (Black, Tree (Red, Tree (Red, a, x, b), y, c), z, d)
  | Tree (Black, Tree (Red, a, x, Tree (Red, b, y, c)), z, d) ->
    Tree (Red, Tree (Black, a, x, b), y, Tree (Black, c, z, d))
  | t -> t
;;

let rbalance = function
  | Tree (Black, a, x, Tree (Red, Tree (Red, b, y, c), z, d))
  | Tree (Black, a, x, Tree (Red, b, y, Tree (Red, c, z, d))) ->
    Tree (Red, Tree (Black, a, x, b), y, Tree (Black, c, z, d))
  | t -> t
;;

let insert t x ~compare ~sexp_of_a =
  let rec go = function
    | Empty -> Tree (Red, Empty, x, Empty)
    | Tree (color, l, y, r) as t ->
      (match Ordering.of_int (compare x y) with
      | Less -> lbalance (Tree (color, go l, y, r))
      | Equal -> t
      | Greater -> rbalance (Tree (color, l, y, go r)))
  in
  match go t with
  | Empty -> raise_s [%message "should not happen" (t : a t) (x : a)]
  | Tree (_, l, y, r) -> Tree (Black, l, y, r)
;;

let of_list xs ~compare ~sexp_of_a =
  List.fold xs ~init:empty ~f:(fun accum x -> insert accum x ~compare ~sexp_of_a)
;;

(* A generator for arbitrary red-black trees, based on
 * (https://matt.might.net/articles/quick-quickcheck) with deterministic keys
 * to guarantee that the generator won't "choose itsef into a corner". *)
let int_generator =
  let open Base_quickcheck in
  let open Generator.Let_syntax in
  (* We define height to be the number of black nodes from a path from the
   * root of a tree to an arbitrary leaf *minus one*. Why define it this way?
   * We want to say that the height of an empty tree is 0 (since we consider
   * [Empty] colored black). 
   *
   * [tree ~min ~max color] generates a tree of height [Generator.size] which
   * can be the child of a tree colored [color]. *)
  let rec tree ~min ~max (color : Color.t) =
    if max < min then raise_s [%message "max is smaller than min" (min : int) (max : int)];
    if (max - min) % 2 = 1
    then raise_s [%message "max - min is odd" (min : int) (max : int)];
    let mid = min + ((max - min) / 2) in
    let%bind height = Generator.size in
    match color, height with
    | Red, 0 -> return Empty
    | Black, 0 -> Generator.of_list [ Empty; Tree (Red, Empty, mid, Empty) ]
    | _, _ ->
      let black_subtree =
        let shrink = Generator.with_size ~size:(height - 1) in
        let%map l = tree ~min ~max:(mid - 1) Black |> shrink
        and r = tree ~min:(mid + 1) ~max Black |> shrink in
        Tree (Black, l, mid, r)
      in
      (match color with
      | Red -> black_subtree
      | Black ->
        let red_subtree =
          let%map l = tree ~min ~max:(mid - 1) Red
          and r = tree ~min:(mid + 1) ~max Red in
          Tree (Red, l, mid, r)
        in
        Generator.union [ red_subtree; black_subtree ])
  in
  let%bind size = Generator.size in
  let height = Int.floor_log2 (size + 1) in
  (* Given a red-black tree of height h, the maximum size of values in the tree
   * is 2^(2h + 1) - 1. The proof is left as an exercise to the reader.
   * (TODO: add proof) *)
  let max_value = Int.pow 2 ((2 * height) + 1) - 2 in
  tree ~min:0 ~max:max_value Black |> Generator.with_size ~size:height
;;
