(* Ex. 01 *)

type expr =
  | Ctb of bool
  | Conj of expr * expr
  | Igual of expr * expr
  | Cti of int
  | Mais of expr * expr
  | Se of expr * expr * expr ;;

type valor =
  | Invalida
  | Inteiro of int
  | Booleano of bool ;;


let rec aval = function
  | Ctb b -> Booleano b
  | Conj (ei, ej) -> begin
    match aval ei, aval ej with
    | Booleano bi, Booleano bj -> Booleano (bj && bj)
    | _, _ -> Invalida
  end
  | Igual (ei, ej) -> begin
    match aval ei, aval ej with
    | Inteiro ii, Inteiro ij -> Booleano (ii = ij)
    | _, _ -> Invalida
  end
  | Cti i -> Inteiro i
  | Mais (ei, ej) -> begin
    match aval ei, aval ej with
    | Inteiro ii, Inteiro ij -> Inteiro (ii + ij)
    | _, _ -> Invalida
  end
  | Se (eb, ei, ej) -> begin
    match aval eb, aval ei, aval ej with
    | Booleano b, Inteiro ii, Inteiro ij -> Inteiro (if b then ii else ij)
    | _, _, _ -> Invalida
  end
;;

(* Ex. 02 *)

type imprimivel = Imp: ('a * ('a -> unit)) -> imprimivel ;;

let l: imprimivel list = [ Imp (3,    print_int);
                           Imp (3.14, print_float);
                           Imp ("Oi", print_string) ] ;;

let rec imprimir (l: imprimivel list) : unit =
  match l with
  | [] -> print_endline "[]"
  | (Imp (v, f))::t -> (f v ; print_string " :: " ; imprimir t)
;;

imprimir l ;;