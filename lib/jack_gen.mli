module R = Jack_random
module T = Jack_tree

(*
  Generator for random values of 'a.
*)
type 'a gen =
  Gen of (R.size -> R.seed -> 'a T.tree)

val run_gen : R.size -> R.seed -> 'a gen -> 'a T.tree

val map_gen : ('a T.tree -> 'b T.tree) -> 'a gen -> 'b gen

val generate : (R.size -> R.seed -> 'a) -> 'a gen

val bind : 'a gen -> ('a -> 'b gen) -> 'b gen

val (>>=) : 'a gen -> ('a -> 'b gen) -> 'b gen

val return : 'a -> 'a gen
