module R = Jack_random
module T = Jack_tree

(*
  Generator for random values of 'a.
*)
type 'a gen =
  Gen of (R.size -> R.seed -> 'a T.tree)

(*
  Runs a generator, producing its shrink tree.
*)
let run_gen size seed (Gen gen) =
  gen size seed

(*
  Map over a generator's shrink tree.
*)
let map_gen f gen =
  Gen (fun size seed ->
      f (run_gen size seed gen))

(*
  Generate a value with no shrinks from a 'Size' and a 'Seed'.
*)
let generate f =
  Gen (fun size seed ->
     T.singleton (f size seed))

(*
 Apply a shrinking function to a generator.

 This will give the generator additional shrinking options, while keeping
 the existing shrinks intact.
*)
let shrinkLazy f g =
  map_gen (Jack_tree.expand f) g

let return a = Gen (fun _ _ -> T.singleton a)

let bind m k =
  Gen (fun size seed ->
      match Jack_random.split seed with
      | (sk, sm) ->
         T.bind (run_gen size sm m) (fun x ->
             run_gen size sk @@ k x
           )
    )

let (>>=) = bind
