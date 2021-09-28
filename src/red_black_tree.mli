open! Core

type 'a t [@@deriving sexp, compare, equal]

val validate : 'a t -> compare:('a -> 'a -> int) -> sexp_of_a:('a -> Sexp.t) -> unit
val empty : 'a t
val to_list : 'a t -> 'a list
val mem : 'a t -> 'a -> compare:('a -> 'a -> int) -> bool
val insert : 'a t -> 'a -> compare:('a -> 'a -> int) -> sexp_of_a:('a -> Sexp.t) -> 'a t
