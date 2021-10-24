open! Core

module type S = sig
  type 'a t

  val create : len:int -> 'a -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> 'a t
  val of_array : 'a array -> 'a t
  val to_array : 'a t -> 'a array
end
