type size

type seed

val split : seed -> (seed * seed)

type 'a random = Random of (seed -> size -> 'a)

val unsafe_run : seed -> size -> 'a random -> 'a

val run : seed -> size -> 'a random -> 'a

val constant : 'a -> 'a random

val map : ('a -> 'b) -> 'a random -> 'b random

val bind : 'a random -> ('a -> 'b random) -> 'b random

val replicate : int -> 'a random -> 'a list random

val sized : (size -> 'a random) -> 'a random

val resize : size -> 'a random -> 'a random
