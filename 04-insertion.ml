let insertion_sort l =
  let rec insert x l =
    match l with
    | [] -> [x]
    | h::t -> if x <= h then x::l else h::(insert x t)
  in let rec move l r =
    match r with
    | [] -> l
    | rh::rt -> move (insert rh l) rt
  in move [] l
;;

insertion_sort [7; -1; 3; 0; 20] ;;