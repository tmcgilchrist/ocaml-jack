(*
  Stream module implementation taken from batteries/batLazyList.ml

  see https://github.com/ocaml-batteries-team/batteries-included/blob/453817c61f7faea0a44301b2febf3d45f48e61fd/src/batLazyList.ml
*)
module Stream : sig
  type 'a stream = Nil
                 | Cons of 'a * 'a t
   and 'a t =
     ('a stream) Lazy.t

  val empty_stream : 'a t
  val next : 'a Lazy.t -> 'a
  val cons : 'a -> 'a t -> 'a t
  val (^:^) : 'a -> 'a t -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val append : 'a t -> 'a t -> 'a t
  val filter : ('a -> bool) -> 'a t -> 'a t

end

type 'a tree =
  | Node of 'a * 'a t
   and 'a t = (('a tree) Stream.stream Lazy.t)

val outcome : 'a tree -> 'a
val shrinks : 'a tree -> ('a tree) Stream.stream Lazy.t

val singleton : 'a -> 'a tree
val map : ('a -> 'b) -> 'a tree -> 'b tree
val bind : 'a tree -> ('a -> 'b tree) -> 'b tree

val id : 'a -> 'a
val join : ('a tree) tree -> 'a tree

val fold : ('a -> 'x -> 'b) -> ('b Stream.t -> 'x) -> 'a tree -> 'b
val foldForest : ('a -> 'x -> 'b) -> ('b Stream.stream Lazy.t -> 'x) -> 'a t -> 'x

val unfold : ('b -> 'a) -> ('b -> 'b Stream.stream Lazy.t) -> 'b -> 'a tree
val unfoldForest : ('b -> 'a) -> ('b -> 'b Stream.stream Lazy.t) -> 'b -> 'a tree Stream.stream Lazy.t

val expand : ('a -> 'a Stream.stream Lazy.t) -> 'a tree -> 'a tree

val filter : ('a -> bool) -> 'a tree -> 'a tree
val filterForest : ('a -> bool) -> (('a tree) Stream.stream Lazy.t) -> ('a tree) Stream.stream Lazy.t
