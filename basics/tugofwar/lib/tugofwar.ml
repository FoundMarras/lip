(* tokens *)
type token = A | B | X

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
             
let toklist_of_string s = 
  let list = explode s in
  let convert c = match c with
    | 'A' -> A 
    | 'B' -> B 
    | '=' -> X
    | _ -> failwith "Invalid character"
  in
  List.map convert list

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
    
let valid l = 
  
  let rec checkB = function
    | [] -> true
    | B :: t -> checkB t
    | _ -> false
  in

  let rec checkX = function
    | [] -> true
    | X :: t -> checkX t
    | B :: t -> checkB t
    | _ -> false
  in

  let rec checkA = function
    | [] -> true
    | A :: t -> checkA t
    | X :: t -> checkX t
    | B :: t -> checkB t
  in

  checkA l

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let win l = 
  let countA = List.fold_left(fun acc x -> if (x == A) then acc+1 else acc) 0 l in
  let countB = List.fold_left(fun acc x -> if (x == B) then acc+1 else acc) 0 l in
  if( countA > countB ) then A 
  else if (countA < countB) then B 
  else X 

(* val string_of_winner : token -> string *)
let string_of_winner w = 
  match w with 
  | A -> "A" 
  | X -> "=" 
  | B -> "B"


