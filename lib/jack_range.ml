type size = int

type 'a range =
  Range of 'a * (size -> ('a * 'a))

let origin (Range (a, _))  = a

let bounds sz (Range (_, f)) = f sz

let singleton x =
  Range (x, fun _ -> (x, x))

let constant_from z x y =
  Range (z, (fun _ -> (x, y)))

let constant x y =
  constant_from x x y

(* let scale_linear : size -> int -> int -> int *)
let scale_linear sz0 z n =
  let sz = max 0 (min 99 sz0) in
  let diff = ((n - z) * sz) / 99 in
  z + diff

(* Truncate a value so it stays within some range. *)
let clamp x y n =
  if x > y then
    min x (max y n)
  else
    min y (max x n)

let scale_exponential sz0 z0 n0 =
  let z = float_of_int z0 in
  let n = float_of_int n0 in
  let sz = clamp 0 99 sz0 in
  let diff = (((abs_float (n -. z) +. 1.0) ** (float_of_int sz /. 99.0)) -. 1.0) *. (n -. z) in
  int_of_float @@ z +. diff

let linear_from z x y =
  Range (z, fun sz ->
    let x_sized =
        clamp x y @@ scale_linear sz z x in

    let y_sized =
        clamp x y @@ scale_linear sz z y
    in
      (x_sized, y_sized))

let linear x y =
  linear_from x x y

let exponential_from z x y =
  Range (z, fun sz ->
            let sized_x = clamp x y @@ scale_exponential sz z x in
            let sized_y = clamp x y @@ scale_exponential sz z y in
            (sized_x, sized_y))

let exponential x y =
  exponential_from x x y
