(* Tests are parameterized by the size of the randomly-generated data, the
 * meaning of which depends on the particular generator used. *)
type size = int

(* A range describes the bounds of a number to generate, which may or may not
 * be dependent on a 'Size'. *)
type 'a range =
  Range of 'a * (size -> ('a * 'a))

(* Get the origin of a range. This might be the mid-point or the lower bound,
 * depending on what the range represents.
 *
 * The 'bounds' of a range are scaled around this value when using the
 * 'linear' family of combinators.
 *
 * When using a 'Range' to generate numbers, the shrinking function will
 * shrink towards the origin. *)

val origin : 'a range -> 'a

(* Get the extents of a range, for a given size. *)
val bounds : size -> 'a range -> ('a * 'a)

(* val lower_bound : 'a ord -> size -> 'a range -> 'a *)
(* val upper_bound : 'a ord -> size -> 'a range -> 'a *)

(* Construct a range which represents a constant single value. *)
val singleton : 'a -> 'a range

(* Construct a range which is unaffected by the size parameter. *)
val constant : 'a -> 'a -> 'a range

(* Construct a range which is unaffected by the size parameter with a origin
   point which may differ from the bounds. *)
val constant_from : 'a -> 'a -> 'a -> 'a range

(* Construct a range which scales the second bound relative to the size
 * parameter. *)
val linear : int -> int -> int range

(* Construct a range which scales the bounds relative to the size parameter. *)
val linear_from  : int -> int -> int -> int range

(* Construct a range which scales the second bound exponentially relative to
 * the size parameter. *)
val exponential : int -> int -> int range

(* Construct a range which scales the bounds exponentially relative to the
 * size parameter. *)
val exponential_from : int -> int -> int -> int range
