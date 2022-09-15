let processar_linha linha =
  let rec media l n soma =
    match l with
    | [] -> 
      if n = 0 
      then raise (Failure "Lista de notas vazia!")
      else soma /. (float_of_int n)
    | h::t -> media t (n+1) (soma +. (float_of_string h))
  in 
  match String.split_on_char ',' linha with
  | nome::notas -> (nome, media notas 0 0.)
  | _ -> raise (Failure "Linha mal formada!")
;;

let processar_arquivo input_path output_path =
  let input_file = open_in input_path in 
  let output_file = open_out output_path in
  let rec f () =
    try
      let linha = input_line input_file in
      let (nome, media) = processar_linha linha in
      output_string output_file (nome ^ "," ^ (string_of_float media) ^ "\n") ;
      f ()
    with
    | End_of_file -> ()
    | Failure s -> f ()
  in f ()
;;
    
processar_arquivo "in" "out" ;;