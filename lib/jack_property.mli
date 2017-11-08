type result =
  | Success
  | Failure of (string list)

(* String formatted representation of a result *)
(* TODO use sexp to do this. *)
val show_result : result -> string

(* User friendly formatted representation of a result *)
val render_result : result -> string
