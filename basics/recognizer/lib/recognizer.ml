let lang1 l = List.fold_left(fun acc x -> if (x = '0' || x = '1') then acc && true else acc && false) true l

let rec lang2 l = 
  match l with
  |[] -> true
  | '0' :: '1' :: t -> lang2 ('1'::t)
  | '1' :: '0' :: _ -> false
  | '1' :: '1' :: t -> lang2 ('1'::t)
  | ['1']-> true
  | _ -> false

let  lang3 l =
  let rec f = function
  |'0'::[]-> true
  |'1'::[]-> false
  | x::t when x = '1' || x = '0' -> f t
  | _ -> false
  in 
  match l with
  |'0'::t -> f t
  |_ -> false

let lang4 l =
  let rec f c = function
    |[]->true
    |'0'::t -> f c t
    |'1'::t when c<2 -> f (c+1) t
    |_-> false
  in
  f 0 l

let rec lang5  = function
|[]->true
|'1'::'1':: t -> lang5 t
|'0'::'0':: t-> lang5 t
|_ -> false

    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
