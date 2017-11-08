(*
  Stream module implementation taken from batteries/batLazyList.ml

  see https://github.com/ocaml-batteries-team/batteries-included/blob/453817c61f7faea0a44301b2febf3d45f48e61fd/src/batLazyList.ml
*)
module Stream = struct
  type 'a stream = Nil
                 | Cons of 'a * 'a t
   and 'a t =
     ('a stream) Lazy.t

  let empty_stream = Lazy.from_val Nil

  let next l = Lazy.force l

  let cons h t = Lazy.from_val (Cons(h, t))

  let ( ^:^ ) = cons

  let map f l =
    let rec aux rest =  match next rest with
      | Cons (x, (t : 'a t)) -> Cons (f x, lazy (aux t))
      | Nil                  -> Nil
    in lazy (aux l)

  let append (l1 : 'a t) (l2 : 'a t) =
    let rec aux list =  match next list with
      | Cons (x, (t : 'a t)) -> Cons (x, lazy (aux t))
      | _                    -> Lazy.force l2
    in lazy (aux l1)

  let filter f l =
    let rec next_true l = match next l with (* Compute the next accepted predicate without thunkification *)
      | Cons (x, l) when not (f x) -> next_true l
      | l                          -> l
    in
    let rec aux l = lazy(match next_true l with
                         | Cons (x, l) -> Cons (x, aux l)
                         | Nil         -> Nil)
    in aux l
end

(* A rose tree which represents a random generated outcome, and all the ways *)
(* in which it can be made smaller. *)
type 'a tree =
  | Node of 'a * 'a t
   and 'a t = (('a tree) Stream.stream Lazy.t)

(* The generated outcome. *)
let outcome (Node (x,_) : 'a tree) : 'a = x

(* All the possible shrinks of this outcome. This should be ordered *)
(* smallest to largest as if property still fails with the first shrink in *)
(* the list then we will commit to that path and none of the others will *)
(* be tried (i.e. there is no backtracking). *)
let shrinks (Node (_, xs) : 'a tree) : (('a tree) Stream.stream Lazy.t) = xs

 (* Create a tree with a single outcome and no shrinks. *)
let singleton (x : 'a) : 'a tree = Node (x, Stream.empty_stream)

(* Map over a tree. *)
let rec map (f : 'a -> 'b) (Node (x, xs) : 'a tree) =
  Node (f x, (Stream.map (map f) xs))

let rec bind (Node (x, xs0) : 'a tree) (k : 'a -> 'b tree) : 'b tree =
  match k x with
  | Node (y, ys) ->
     let xs = Stream.map (fun m -> bind m k) xs0 in
     Node (y, Stream.append xs ys)

let id (x : 'a) : 'a = x

let join (xss : ('a tree) tree) : 'a tree =
  bind xss id

(* Fold over a tree *)
let rec fold (f : 'a -> 'x -> 'b) (g : 'b Stream.stream Lazy.t -> 'x) (Node (x, xs) : 'a tree) : 'b =
  f x (foldForest f g xs)
(* Fold over a list of trees. *)
and foldForest (f : 'a -> 'x -> 'b) (g : 'b Stream.stream Lazy.t -> 'x) (xs : ('a tree) Stream.stream Lazy.t) : 'x =
  Stream.map (fold f g) xs |> g

(* Build a tree from an unfolding function and a seed value. *)
let rec unfold (f : 'b -> 'a) (g : 'b -> 'b Stream.stream Lazy.t) (x : 'b) : 'a tree =
  Node (f x, unfoldForest f g x)

(* Build a list of trees from an unfolding function and a seed value. *)
and unfoldForest (f : 'b -> 'a) (g : 'b -> 'b Stream.stream Lazy.t) (x : 'b) : ('a tree) Stream.stream Lazy.t =
  g x |> Stream.map (unfold f g)

(* Apply an additional unfolding function to an existing tree. *)
(*     The root outcome remains intact, only the shrinks are affected, this *)
(*     applies recursively, so shrinks can only ever be added using this *)
(*     function. *)
(*     If you want to replace the shrinks altogether, try: *)
(*     <c>Tree.unfold f (outcome oldTree)</c> *)
let rec expand (f : 'a -> 'a Stream.stream Lazy.t) (Node (x, xs) : 'a tree) : 'a tree =

  (* Ideally we could put the 'unfoldForest' nodes before the 'map expandTree' *)
  (* nodes, so that we're culling from the top down and we would be able to *)
  (* terminate our search faster, but this prevents minimal shrinking. *)

  (* We'd need some kind of tree transpose to do this properly. *)

  let ys = Stream.map (expand f) xs in
  let zs = unfoldForest id f x in
  Node (x, Stream.append ys zs)

(* Recursively discard any shrinks whose outcome does not pass the predicate. *)
(* <i>Note that the root outcome can never be discarded</i> *)
let rec filter (f : 'a -> bool) (Node (x, xs) : 'a tree) : 'a tree =
  Node (x, filterForest f xs)

(* Recursively discard any trees whose outcome does not pass the predicate. *)
and filterForest (f : 'a -> bool) (xs : ('a tree) Stream.stream Lazy.t) : ('a tree) Stream.stream Lazy.t =
  Stream.filter (fun x -> f (outcome x)) xs
  |> Stream.map (filter f)
