type result =
  | Success
  | Failure of (string list)

let show_result = function
  | Success -> "Success"
  | Failure xs ->
     let xb = String.concat " " xs in
     Format.sprintf "(Failure %s)" xb

let render_result = function
  | Success -> "Success"
  | Failure xs ->
     Format.sprintf "Failure: \n %s" (String.concat "\n " xs)
