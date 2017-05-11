type size

type seed

val split : seed -> (seed * seed)

type 'a random = Random of (seed -> size -> 'a)
