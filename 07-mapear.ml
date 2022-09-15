let rec mapear f = function
  | [] -> []
  | h::t -> (f h)::(mapear f t)
;;

(* Exemplo: string list -> float list *)

mapear float_of_string ["1.4"; "-2"; "27"; "42."] ;;