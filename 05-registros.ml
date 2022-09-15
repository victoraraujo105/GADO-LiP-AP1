type registro = {nome: string; nascimento: int * int * int} ;;

let main () =
  let rec exibir_menu l =
    print_endline "Menu:" ;
    print_endline "\t1) Listar Registros." ;
    print_endline "\t2) Inserir Registro." ;
    print_endline "\t3) Sair." ;
    let listar l =
      print_endline "Cadastro atual:" ;
      print_endline "----------------------------------------" ;
      let rec f = function
        | [] -> ()
        | {nome; nascimento = (dia, mes, ano)}::t ->
          Printf.printf "Nome: %s\n" nome ;
          Printf.printf "Nascimento: %d/%d/%d\n" dia mes ano ;
          print_endline "----------------------------------------" ;
          f t
      in f l
    in let ler_registro () =
      let rec ler_nome () =
        print_string "Nome: " ;
        let nome_candidato = read_line () in
        let rec checar_existencia = function
          | [] -> false
          | {nome; nascimento = _}::t -> nome = nome_candidato || (checar_existencia t)
        in 
        if checar_existencia l 
        then (print_endline "Nome já existe!" ; ler_nome ())
        else nome_candidato
      in let rec ler_data () =
        print_string "Nascimento: " ;
        let data = read_line () in
        try
          match String.split_on_char '/' data with
          | [d; m; a] -> 
              (int_of_string d, int_of_string m, int_of_string a)
          | _ -> raise (Failure "Data inválida.")
        with
        | _ -> (print_endline "Data inválida!" ; ler_data ())
      in {nome = ler_nome (); nascimento = ler_data ()}
    in match (print_string "Digite sua opção: "; read_line ()) with
    | "1" -> (listar l ; exibir_menu l)
    | "2" -> exibir_menu ((ler_registro ())::l) 
    | "3" -> ()
    | _ -> (print_endline "Opção inválida!" ; exibir_menu l)
  in exibir_menu []
;;

main ()