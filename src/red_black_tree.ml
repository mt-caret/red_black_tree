open Core
module Persistent_array = Persistent_array
module Persistent_array_intf = Persistent_array_intf

(** Implementation of Okasaki's Red-Black trees *)
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

let is_empty = function
  | Empty -> true
  | Tree _ -> false
;;

let length t =
  let rec go t accum =
    match t with
    | Empty -> accum
    | Tree (_, l, _, r) -> (go [@tailcall]) l (go r (accum + 1))
  in
  go t 0
;;

let rec mem t x ~compare =
  match t with
  | Empty -> false
  | Tree (_, l, y, r) ->
    (match compare x y with
    | 0 -> true
    | n when n < 0 -> mem l x ~compare
    | _ -> mem r x ~compare)
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
          let ( <= ) a b = a < b || compare a b = 0 in
          if not (l_min <= l_max && l_max < x && x < r_min && r_min <= r_max)
          then
            raise_s
              [%message
                "binary tree has imbalance"
                  (l : a t)
                  (l_min : a)
                  (l_max : a)
                  (x : a)
                  (r : a t)
                  (r_min : a)
                  (r_max : a)];
          l_min, r_max
        | Some min_max, None | None, Some min_max -> min_max
      in
      black_nodes, Some min_max
  in
  ignore (go t : _ * _ option)
;;

(* We can construct minimum-height red-black trees rather easily by imposing
 * the following constraints:
 * (1) all nodes on each level have the same color
 * (2) the lowermost nodes are colored red
 *
 * Exercise 3.9 can be solved with something like this:
 * (fun l ->
 *   let a = Array.of_list l in
 *   of_increasing_iterator_unchecked
 *     ~len:(Array.length a)
 *     ~f:(Array.get a))
 * *)
let of_increasing_iterator_unchecked ~len ~f =
  let rec go color left right =
    if left = right
    then Empty
    else (
      let mid = (left + right) / 2 in
      let color' =
        match color with
        | Color.Red -> Color.Black
        | Black -> Red
      in
      Tree (color, go color' left mid, f mid, go color' (mid + 1) right))
  in
  let root_color = if len = 0 || Int.floor_log2 len % 2 = 1 then Color.Black else Red in
  go root_color 0 len
;;

let validate_optimal_height t ~sexp_of_a =
  let rec go = function
    | Empty -> 0
    | Tree (_, l, _, r) ->
      let l_height = go l in
      let r_height = go r in
      if abs (l_height - r_height) > 1
      then
        raise_s
          [%message
            "depth more than 1 different"
              (l_height : int)
              (l : a t)
              (r_height : int)
              (r : a t)
              (t : a t)];
      l_height + 1
  in
  ignore (go t : int)
;;

let lbalance l z d =
  match l with
  | Tree (Red, Tree (Red, a, x, b), y, c) | Tree (Red, a, x, Tree (Red, b, y, c)) ->
    Tree (Red, Tree (Black, a, x, b), y, Tree (Black, c, z, d))
  | _ -> Tree (Black, l, z, d)
;;

let rbalance a x r =
  match r with
  | Tree (Red, Tree (Red, b, y, c), z, d) | Tree (Red, b, y, Tree (Red, c, z, d)) ->
    Tree (Red, Tree (Black, a, x, b), y, Tree (Black, c, z, d))
  | _ -> Tree (Black, a, x, r)
;;

exception Element_exists

let add t x ~compare ~sexp_of_a =
  let rec go = function
    | Empty -> Tree (Red, Empty, x, Empty)
    | Tree (Red, l, y, r) ->
      (* Noting that Okasaki's [balance] implementation only rebalances when
       * the color is black, we skip the balance call here. *)
      (match compare x y with
      | 0 ->
        (* When an element already exists in the tree, we throw an exception to
         * escape out of recursion to avoid rebuilding the tree (Exercise 2.3).
         *)
        raise Element_exists
      | n when n < 0 -> Tree (Red, go l, y, r)
      | _ -> Tree (Red, l, y, go r))
    | Tree (Black, l, y, r) ->
      (match compare x y with
      | 0 -> raise Element_exists
      | n when n < 0 -> lbalance (go l) y r
      | _ -> rbalance l y (go r))
  in
  try
    match go t with
    | Empty -> raise_s [%message "should not happen" (t : a t) (x : a)]
    | Tree (_, l, y, r) -> Tree (Black, l, y, r)
  with
  | Element_exists -> t
;;

(*
(* The implementation for Exercise 3.10 (b) is as follows, but it actually
 * seems to degrade performance, so we don't use it.  *)
(* TODO: revisit *)
let llbalance color l x r =
  match color, l, x, r with
  | Color.Black, Tree (Red, Tree (Red, a, x, b), y, c), z, d ->
    Tree (Red, Tree (Black, a, x, b), y, Tree (Black, c, z, d))
  | _ -> Tree (color, l, x, r)
;;

let lrbalance color l x r =
  match color, l, x, r with
  | Color.Black, Tree (Red, a, x, Tree (Red, b, y, c)), z, d ->
    Tree (Red, Tree (Black, a, x, b), y, Tree (Black, c, z, d))
  | _ -> Tree (color, l, x, r)
;;

let rlbalance color l x r =
  match color, l, x, r with
  | Color.Black, a, x, Tree (Red, Tree (Red, b, y, c), z, d) ->
    Tree (Red, Tree (Black, a, x, b), y, Tree (Black, c, z, d))
  | _ -> Tree (color, l, x, r)
;;

let rrbalance color l x r =
  match color, l, x, r with
  | Color.Black, a, x, Tree (Red, b, y, Tree (Red, c, z, d)) ->
    Tree (Red, Tree (Black, a, x, b), y, Tree (Black, c, z, d))
  | _ -> Tree (color, l, x, r)
;;

let add t x ~compare ~sexp_of_a =
  let rec go = function
    | Empty -> Ordering.Equal, Tree (Red, Empty, x, Empty)
    | Tree (color, l, y, r) as t ->
      let ordering = Ordering.of_int (compare x y) in
      ( ordering
      , (match ordering with
        | Less ->
          (match go l with
          | Less, l' -> llbalance color l' y r
          | Equal, l' ->
            (* If [color = Red] and [l = Empty], I feel like this would result
             * in a red-red violation, but I can't figure out a way to
             * reproduce this case... *)
            Tree (color, l', y, r)
          | Greater, l' -> lrbalance color l' y r)
        | Equal -> t
        | Greater ->
          (match go r with
          | Less, r' -> rlbalance color l y r'
          | Equal, r' -> Tree (color, l, y, r')
          | Greater, r' -> rrbalance color l y r')) )
  in
  match go t with
  | _, Empty -> raise_s [%message "should not happen" (t : a t) (x : a)]
  | _, Tree (_, l, y, r) -> Tree (Black, l, y, r)
;;
 *)

let of_list xs ~compare ~sexp_of_a =
  List.fold xs ~init:empty ~f:(fun accum x -> add accum x ~compare ~sexp_of_a)
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
