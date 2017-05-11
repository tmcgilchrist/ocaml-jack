open Core_kernel.Std

module R = Core_kernel.Splittable_random


(* Tests are parameterized by the `Size` of the randomly-generated data, *)
(* the meaning of which depends on the particular generator used. *)
type size = int

type seed = R.State.t

let split seed =
  let seed1 = R.State.split seed in
  let seed2 = R.State.split seed1 in
  (seed1, seed2)

(* A generator for random values of type 'a *)
type 'a random =
  Random of (seed -> size -> 'a)

let unsafe_run (s : seed) (size : size) ((Random r) : 'a random) : 'a =
  r s size

let run (s : seed) (size : size) (r : 'a random) : 'a =
  unsafe_run s (max 1 size) r

let constant (x : 'a) : 'a random =
  Random (fun _ _ -> x)

let map (f : 'a -> 'b) (r : 'a random) : 'b random =
  Random (fun seed size -> r |> unsafe_run seed size |> f)

let bind (r : 'a random) (k : 'a -> 'b random) : 'b random =
  Random ( fun seed size ->
           (* TODO Not sure this is correct. *)
           let seed1, seed2 = split seed in
           r
           |> unsafe_run seed1 size
           |> k
           |> unsafe_run seed2 size
         )
let replicate (times : int) (r : 'a random): 'a list random =
  Random ( fun seed0 size ->
           let rec loop seed k acc =
             if k <= 0 then
               acc
             else
               let seed1, seed2 = split seed in
               let x = unsafe_run seed1 size r in
               loop seed2 (k - 1) (x :: acc) in
            loop seed0 times []

    )

let sized (f : size -> 'a random) : 'a random =
  Random (fun seed size ->
      unsafe_run seed size (f size))

let resize (newSize : size) (r : 'a random) : 'a random =
  Random (fun seed _ -> run seed newSize r)


(* TODO apply to Monad functor in Core *)
(* F# supplies these functions *)
(* let (|>) x f = f x *)
(* let (<|) f x = f x *)

(* TODO equivalent to F# Range type *)
