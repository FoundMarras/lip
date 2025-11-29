let lang1 l = List.fold_left(fun acc x -> if (x = "0" || x = "1") then acc && true else acc && false) true l

let rec lang2 l = 
  match l with
  |[] -> true
  | "0" :: "1" :: t -> lang2 ("1"::t)
  | "1" :: "0" :: _ -> false
  | "1" :: "1" :: t -> lang2 ("1"::t)
  | ["1"]-> true
  | _ -> false

let lang3 _ = failwith ""

let lang4 _ = failwith ""

let lang5 _ = failwith ""
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
