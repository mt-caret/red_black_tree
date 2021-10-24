open Core

(*
 * TODO:
 * - path compression
 * - store more information at root (i.e. Array)
 * *)

module Most_naive = struct
  type 'a t = 'a array

  let create = Array.create
  let get t i = t.(i)

  let set t i x =
    let t' = Array.copy t in
    t'.(i) <- x;
    t'
  ;;

  let to_array = Fn.id
  let of_array = Fn.id
end

module Naive = struct
  type 'a inner =
    | Array of 'a Array.t
    | Diff of int * 'a * 'a t

  and 'a t = 'a inner ref

  let create ~len x = ref (Array (Array.create ~len x))

  let reroot t =
    let rec go t =
      match !t with
      | Array arr -> arr
      | Diff (i, x, t') ->
        let arr = go t' in
        let x' = arr.(i) in
        arr.(i) <- x;
        t := Array arr;
        t' := Diff (i, x', t);
        arr
    in
    go t, t
  ;;

  let get t i = (fst (reroot t)).(i)
  let set t i x = ref (Diff (i, x, snd (reroot t)))
  let of_array arr = ref (Array (Array.copy arr))
  let to_array t = fst (reroot t) |> Array.copy
end

module Naive2 = struct
  type 'a inner =
    | Array of 'a Array.t
    | Diff of 'a Int.Map.t * 'a t

  and 'a t = 'a inner ref

  let create ~len x = ref (Array (Array.create ~len x))

  let reroot t =
    let rec go t =
      match !t with
      | Array arr -> arr
      | Diff (diffs, t') ->
        let arr = go t' in
        let inv_diffs =
          Map.mapi diffs ~f:(fun ~key:i ~data:x ->
              let x' = arr.(i) in
              arr.(i) <- x;
              x')
        in
        t := Array arr;
        t' := Diff (inv_diffs, t);
        arr
    in
    go t, t
  ;;

  let get t i = (fst (reroot t)).(i)
  let set t i x = ref (Diff (Int.Map.singleton i x, snd (reroot t)))
  let of_array arr = ref (Array (Array.copy arr))
  let to_array t = fst (reroot t) |> Array.copy
end

module Naive3 = struct
  type 'a inner =
    | Array of 'a Array.t
    | Diff of 'a Int.Map.t * 'a t

  and 'a t = 'a inner ref

  let create ~len x = ref (Array (Array.create ~len x))

  let reroot t =
    let rec go t =
      match !t with
      | Array arr -> arr, t, Int.Map.empty
      | Diff (diffs, t') ->
        let arr, arr_t, diffs_to_root = go t' in
        let merged_diffs =
          Map.merge_skewed diffs diffs_to_root ~combine:(fun ~key:_ x _y -> x)
        in
        t := Diff (merged_diffs, arr_t);
        arr, arr_t, merged_diffs
    in
    let arr, arr_t, diffs = go t in
    match !t with
    | Array _ -> arr, t
    | Diff _ ->
      (* TODO: create new Array node when size of diffs is sufficiently large? *)
      let inv_diffs =
        Map.mapi diffs ~f:(fun ~key:i ~data:x ->
            (* TODO: use phys_equal to reduce entry when x = arr.(i) *)
            let x' = arr.(i) in
            arr.(i) <- x;
            x')
      in
      t := Array arr;
      arr_t := Diff (inv_diffs, t);
      arr, t
  ;;

  let get t i = (fst (reroot t)).(i)
  let set t i x = ref (Diff (Int.Map.singleton i x, snd (reroot t)))
  let of_array arr = ref (Array (Array.copy arr))
  let to_array t = fst (reroot t) |> Array.copy
end
