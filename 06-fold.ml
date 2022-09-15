let rec fold (op: 'a -> 'b -> 'a) (base: 'a) (l: 'b list) =
  match l with
  | [] -> base
  | h::t -> fold op (op base h) t
;;

let soma = fold (+.) 0. ;;

let max l = 
  match l with
  | [] -> None
  | h::t -> Some (fold (fun base h -> if base > h then base else h) h l)
;;

let media (l: float list) =
  match l with
  | [] -> None
  | h::t -> Some (
    let (n, sum) = fold (fun (n, sum) h -> (n+1, sum +. h)) (1, h) t
    in sum /. (float_of_int n)
  )
;;