open Core.Std

let list = [1;2;3]
;;

type t = {
  x: int;
} with sexp
;;

List.iter list ~f:(fun x ->
  printf "%s\n%!" (Sexp.to_string (sexp_of_t {x;}));
)
;;
