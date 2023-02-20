(*
In Fractran, a program consists of a finite list of positive, rational numbers. 
The input to a program is a positive integer n. 
The list is then searched in order, for a rational number p/q such that np/q is an integer. 
Then n is replaced by np/q and the search is restarted from the beginning of the list. 
The program halts when no such number p/q can be found, and the final n becomes the output from the program
*)

type fract = {
  num : int;
  den : int
}

(*let prgm = "17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23, 77/19, 1/17, 11/13, 13/11, 15/14, 15/2, 55/1"
*)

let prgm = "3/10, 4/3"
let rec pgcd a b = if b = 0 then a else pgcd b (a mod b)

let ($/) b a = {num = b.num/a;den=b.den/a}

let simplify f = f $/ (pgcd f.num f.den)  

let ($*) b a = simplify {num = a*b.num; den=b.den}

let frac_of_string size i =
  let f = (String.init (size-1) (fun j-> prgm.[i+j-size+1])) in
  let cut = String.index f '/' in  
  {num=int_of_string (String.sub f 0 cut);
   den=int_of_string (String.sub f (cut+1) (size-cut-2))}

let parse prgm =
  let len = String.length prgm  in 
  let rec aux size i l =
    if i = len then (frac_of_string size i)::l
    else if prgm.[i] = ',' then aux 0 (i+1) ((frac_of_string size i)::l)
    else aux (size+1) (i+1) l
  in aux 1 0 []

let rec exec l n iter =
  if iter=0 then () else
    let rec search l n =
      match l with
        [] -> -1
      | t::q -> let t' = t $* n in 
        if t'.den = 1 then t'.num 
        else search q n 
    in
    begin 
      let n' = search l n in 
      if n' = -1 then ()
      else (Printf.printf "%d " n'; exec l n' (iter-1) )
    end
(* 
let print_frac t =
	Printf.printf "(Frac : %d,%d)\n" t.num t.den

let rec print_ls l =
	match l with
	[] -> ()
	| t::q ->print_frac t; print_ls q
*)
let () =
  let l = List.rev (parse prgm) in
  print_ls l;
  exec l 15 10;
