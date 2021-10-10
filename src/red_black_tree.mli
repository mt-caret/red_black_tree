open! Core

type 'a t [@@deriving sexp, compare, equal]

val validate : 'a t -> compare:('a -> 'a -> int) -> sexp_of_a:('a -> Sexp.t) -> unit
val validate_optimal_height : 'a t -> sexp_of_a:('a -> Sexp.t) -> unit
val empty : 'a t
val length : 'a t -> int
val to_list : 'a t -> 'a list
val is_empty : 'a t -> bool
val mem : 'a t -> 'a -> compare:('a -> 'a -> int) -> bool
val add : 'a t -> 'a -> compare:('a -> 'a -> int) -> sexp_of_a:('a -> Sexp.t) -> 'a t
val of_list : 'a list -> compare:('a -> 'a -> int) -> sexp_of_a:('a -> Sexp.t) -> 'a t
val of_increasing_iterator_unchecked : len:int -> f:(int -> 'a) -> 'a t
val min_elt : 'a t -> 'a option
val max_elt : 'a t -> 'a option
val int_generator : int t Base_quickcheck.Generator.t
