module R = Jack_random
module T = Jack_tree
module LazyStream = T.Stream

let id x = x

type 'a gen =
  Gen of (R.size -> R.seed -> 'a T.tree)

let run_gen (s : R.size) seed (Gen gen : 'a gen) : 'a T.tree  =
  gen s seed

let map_gen (f : 'a T.tree -> 'b T.tree) (gen : 'a gen) : 'b gen =
  Gen (fun size seed ->
      f (run_gen size seed gen))
