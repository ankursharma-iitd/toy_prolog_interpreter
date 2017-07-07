
(* In this assignment, you will write a simplified version of a Prolog interpreter in OCaml.

You will first define an ML data type to represent the structure of a legitimate Prolog program.

A program is a set (list) of clauses. 
 A clause can either be a fact or a rule. A fact has a head but no body.  A rule has a head and a body.  
The head is a single atomic formula.  A body is a sequence of atomic formulas.
An atomic formula is a k-ary predicate symbol followed by k terms.
A term is either a variable, a constant, or a k-ary function symbol with k subterms.
A goal is a set (list) of atomic formulas.
You need to take your implementation of unification to use as the parameter-passing mechanism. (Note: by pretending the predicate symbol is a function symbol, you can perform resolution of goals and program clauses).

You also need to develop a back-tracking strategy to explore the resolution search space.   You need to be able to replace a goal by subgoals, as found by applying a unifier to the body of a program clause whose head unified with the chosen subgoal.

Check that your interpreter can execute the family relationship programs you have specified earlier *)
exception NOT_UNIFIABLE;;

open Types
open Printf

let var_table = Hashtbl.create 160 ;;

let elem1 (x,y) = x;;
let elem2 (x,y) = y;;

let check_int (x:(symbol * int)) = if((elem2 x) >= 0) then true else false;;

let check_symbol (l:symbol) = try (Hashtbl.find var_table l; false) with Not_found -> true ;;

let rec check_sig (x:sig_list) = match x with
	| Sig([]) -> (Hashtbl.reset var_table; true)
	| Sig(l::ls) -> if (check_symbol (elem1 l) && check_int l) then (Hashtbl.replace var_table (elem1 l) (elem2 l);check_sig (Sig ls))
					else (Hashtbl.reset var_table; false);;

let rec check_consistency (s:symbol) (x:int) (y:sig_list) = match y with
				|Sig([]) -> false
				|Sig(l::ls) -> if(((elem1 l) = s) && ((elem2 l) = x) && (check_sig y)) then true else (check_consistency s x (Sig ls));;

let rec wfterm (y:sig_list) (x:term)  = match x with
				|V(_) -> true
				|Const(a)-> (check_consistency a 0 y)
				|At(a,b) -> (check_consistency a (List.length b) y) && (List.for_all (wfterm y) b);;

(*return ht given a term*)
let maximum a b = if(b>=a) then b else a;;

let rec ht (x:term) = match x with
				|V(_) -> 0
				|Const(_)-> 0
				|At(a,b) -> 1 + (List.fold_left maximum 0 (List.map ht b));;

let sum a b = a + b ;;

let rec size (x:term) = match x with
				|V(_) -> 1 
				|Const(_)-> 1
				|At(a,b) -> 1 + (List.fold_left sum 0 (List.map size b));;

let rec union_append l1 l2 = match l1 with
				|[] -> l2
				|h::t -> if(List.mem h l2) then union_append t l2
						 else union_append t (h::l2);;

let rec vars (x:term) = match x with
				|V(a) -> [a] 
				|Const(a) -> [a]
				|At(a,b) -> (List.fold_left union_append [] (List.map vars b));;

(* ----------------------------------------------------------------------------------------------------------------------- *)
let get_symbol (x:substitution) = match x with
        |Subs(a,b) -> a;;

let get_term (x:substitution) = match x with
        |Subs(a,b) -> b;;

let rec sigma (v : variable) (s : substitution list) = match s with
 |[] -> V(v)
 |(x::xs) -> if (v=(get_symbol x)) then (get_term x) else (sigma v xs);;

let rec subst (s : substitution list) (t : term) = match t with
 |V(x) -> (sigma x s)
 |Const(x) -> Const(x)
 |At(a,b) -> At(a,(List.map (subst s) b));;

let rec occurs (x:variable) (y:term) = match y with
				| Const(_) -> false
				| V(t) -> (x=t)
				| At(_,rest) -> List.exists (occurs x) rest;;

(* ------------------------------------------------------------------------------------------------------------------------------ *)
(* let composition s1 s2 =
   let rec aux s1 s2 s = match (s1,s2) with
      | ([],[])   -> s
      | (l1, [])  -> l1 @ s
      | ([], l2)  -> l2 @ s
      | ((Subs(v1, t1) :: tl1), (Subs(v2, t2) :: tl2)) -> if (occurs v2 t1) then (Subs(v1, subst ([Subs(v2,t2)]) t1)::s) else (aux tl1 tl2 ([Subs(v1, t1)] @ [Subs(v2, t2)] @ s)) in
       aux s1 s2 [];; *)

let rec check_member (s:substitution) (sList:substitution list) = match (s,sList) with
	|(_,[]) -> false
	|Subs(x,_),Subs(y,_)::ys -> if(x=y) then true else (check_member s ys);; 

let rec append_substitution (sList1:substitution list) (sList2:substitution list) : substitution list = match (sList1,sList2) with
	|([],[]) -> []
	|(l1,[]) -> l1
	|([],l2) -> l2
	|(l,x::xs) -> if(check_member x l) then (append_substitution xs l) else x::(append_substitution xs l);; 

let funct (t:substitution list) (x:substitution) = match x with
			|(Subs(x,y)) -> Subs(x,(subst t y));;

let rec composition (sub1:substitution list) (sub2:substitution list) : substitution list = match (sub1,sub2) with
	|([],[]) -> []
	|([],a) -> a
	|(a,[]) -> a
	|(p,l) -> append_substitution (List.map (funct sub2) p) l;;
	
let rec mgu (t1:term) (t2:term) : (substitution list) = match (t1,t2) with
				|(V(a),V(b)) -> if(a=b) then [] else [Subs(a,V(b))]
				|(V(a),Const(b)) -> [Subs(a,Const(b))]
				|(Const(b),V(a)) -> [Subs(a,Const(b))]
				|(Const(a),Const(b)) -> if(a=b) then [] else raise NOT_UNIFIABLE
				|(Const(x), At(s,tl)) -> if ( ((List.length tl) = 0) && (x = s)) then [] else raise NOT_UNIFIABLE
				|(At(s,tl), Const(x)) -> if ( ((List.length tl) = 0) && (x = s)) then [] else raise NOT_UNIFIABLE
				|(V(a), At(b,[])) -> [Subs(a,At(b,[]))]
				|(At(b,[]),V(a)) -> [Subs(a,At(b,[]))]
				|(At(a,[]),At(b,[])) -> if(a=b) then [] else raise NOT_UNIFIABLE
				|(V(a),At(b,c)) -> if(occurs a (At(b,c))) then (raise NOT_UNIFIABLE) else [Subs(a,At(b,c))]
				|(At(b,c),V(a)) -> if(occurs a (At(b,c))) then (raise NOT_UNIFIABLE) else [Subs(a,At(b,c))]
				|(At(a,l1),At(b,l2)) -> if((a=b) && ((List.length l1)=(List.length l2)) && (a=b)) then
															let rec unify l1 l2 = match (l1,l2) with
															|(_,[]) |([],_) -> []
															|(x::xs,t::ts)->let unifier = (mgu x t) in 
																			composition (unify ( List.map ( subst unifier )  xs) (List.map (subst unifier) ts)) unifier
															in unify l1 l2;
											else (raise NOT_UNIFIABLE);;
(* ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- *)

(* let rec print_subs s = match s with
|[] -> print_string "\n"
|Subs(x,tm)::xs -> (print_string (x ^ " = " ^ (print_term tm) ^ "\n")); (print_subs xs);;
let print_clause cl =  ((List.fold_left (fun acc tm -> acc ^ (print_term tm) ^ " " ) "CL: [" cl)^"]");;
let print_db db = print_string ((List.fold_left (fun acc cl -> acc ^ (print_clause cl) ^ ", " ) "DB: [" db)^"]\n");; *)







