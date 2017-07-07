open Types
open Printf
open Main

let lexbuf = Lexing.from_channel (open_in "clauses.pl");;
let clause_list = Parser.program Lexer.token lexbuf;;   (*raw clause list created*)

let unify env t1 t2 = try (true, (composition env (mgu (subst env t1) (subst env t2)) ))
      with NOT_UNIFIABLE -> (false, []);;

let filt (c) = match c with
|V(_) -> false
|Const(_) -> false
|_ -> true;;

let filter2 g = match g with
|At(_,_) -> true
|_ -> false;;

let cList = let list_filter = List.filter (filt) in List.map list_filter clause_list;;

(*-------------------------------------------------------HELPER FUNCTIONS---------------------------------------------------------------------------*)
let rec print_term (t:term) = match t with
    |V(x) -> x
    |Const(x) -> x
    |At(x,y) -> x ^ "(" ^ (String.concat "," (List.map print_term y)) ^ ")"

let rec print_orig_substitution subs = match subs with
|[] -> print_string "\n"
|Subs(var,term)::xs -> if(not (String.contains var ('\''))) then ((print_string (var ^ " = " ^ (print_term term) ^ "\n")); (print_orig_substitution xs)) else (print_orig_substitution xs);;

let print_answer ans = match ans with
|(false,_) -> (print_string "No.\n")
|(true,s) ->if (s = []) then (print_string "Yes.\n") else (print_string "Yes.\n");(print_orig_substitution s);;

(*-----------------------------------------------------------MAIN CODE-----------------------------------------------------------------------------------*)

(*to copy one clause list to another clause list after appending a marker to each variable term*)
let rec copy_all term_list_list = 
 let rec copy_term term= match term with
  | V(x) -> V(x^"'")
  | Const(x) -> Const(x)
  | At(x,y) -> (At (x,(List.map (copy_term) y)))
 in
 let copy_term_list = List.map (copy_term) in List.map copy_term_list term_list_list;;

(*method that gives the first unification of a goal with a clause list*)
let rec get_first_matching f l = match l with
 | [] -> (false,[],[])
 | x::xs -> let temp = f x in match temp with
    |(true,_,_) -> temp
    |(false,_,_) -> get_first_matching f xs;;

(*method to solve the goals, database is stored as an entity of tupled, and new databse is always used for the next check*)
let rec solve_a_given_clause database env queryList clause = match (unify env (List.hd clause) (List.hd queryList)) with
 | (false,old_sub_list) -> (false,database,old_sub_list)
 | (true,new_sub_list) -> solve_all_the_goals (database,new_sub_list) ((List.tl clause)@(List.tl queryList))
 and solve_all_the_goals (database,env) total_goals = match total_goals with
 | [] -> (true,database,env)
 | _ ->let new_database = copy_all database in get_first_matching (solve_a_given_clause new_database env total_goals) new_database
 and solve_stuff database queryList = match (solve_all_the_goals (database,[]) queryList) with
 |(x,_,e) -> (x,e);;


(*----------------------------------------CALLING MAIN-----------------------------------------------------------------------------------------------*)
let main () =
    try
    while true do
    let () = print_string "?- " in 
    let _ = flush stdout in
    let goal = (Parser.query Lexer.token (Lexing.from_channel stdin)) 
	in 
		let doit q = 
		let gList =List.filter filter2 q
		in (print_answer (solve_stuff (List.rev cList) gList));
	in
	doit goal;
    done
    	with _ -> exit 0

let _ = Printexc.print main () 

(*--------------------------------------------------------EXIT---------------------------------------------------------------------------------------*)