module R = Jack_random
module T = Jack_tree

let id x = x

(*
  Generator for random values of 'a.
*)
type 'a gen =
  Gen of (R.size -> R.seed -> 'a T.tree)

(*
  Runs a generator, producing its shrink tree.
*)
let run_gen (s : R.size) (seed : R.seed) (Gen gen : 'a gen) : 'a T.tree  =
  gen s seed

(*
  Map over a generator's shrink tree.
*)
let map_gen (f : 'a T.tree -> 'b T.tree) (gen : 'a gen) : 'b gen =
  Gen (fun size seed ->
      f (run_gen size seed gen))

(*
  Generate a value with no shrinks from a 'Size' and a 'Seed'.
*)
let generate (f : R.size -> R.seed -> 'a) : 'a gen =
  Gen (fun size seed ->
     T.singleton (f size seed))

let shrink (f : 'a -> 'a T.Stream.stream) (g :'a gen) : 'a gen =
  map_gen (T.expand f g)
