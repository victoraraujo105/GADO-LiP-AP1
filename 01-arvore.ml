type arvoreFloat = | Vazia | No of arvoreFloat * float * arvoreFloat ;;

(* Todos elementos à esquerda são menores ou iguais à raiz, e os à direita são maiores *)
let rec inserir_arv e raiz =
  match raiz with
  | Vazia -> No (Vazia, e, Vazia)
  | No (esq, v, dir) -> 
    if e <= v 
    then No (inserir_arv e esq, v, dir)
    else No (esq, v, inserir_arv e dir)
;;

let rec remover e raiz =
  let rec inserir esq dir = 
    match esq with
    | Vazia -> dir
    | No (esq_esq, v, esq_dir) -> No (esq_esq, v, inserir esq_dir dir)
  in
  match raiz with
  | Vazia -> Vazia
  | No (esq, v, dir) -> 
    if v = e 
    then inserir esq dir 
    else if e < v then No (remover e esq, v, dir) 
    else No (esq, v, remover e  dir)
;;