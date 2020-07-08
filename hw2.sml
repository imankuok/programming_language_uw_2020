(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun  all_except_option (s, lst)= (* string * string lst  -> opt  *)
     let
	 fun aux (s, lst)= 
	     case lst of
		 []  => []
	       | head::tail => if same_string(head, s)
			       then aux(s, tail)
			       else head::aux(s, tail)
				   
     in
	 if (lst= aux(s, lst))
	  then NONE
	 else SOME (aux(s, lst))
     end
	 
fun  all_except_option1 (s, lst)= (* string * string lst  -> list  *)
     let
	 fun aux (s, lst)= 
	     case lst of
		 []  => []
	       | head::tail => if same_string(head, s)
			       then aux(s, tail)
			       else head::aux(s, tail)
				   
     in
	 if (lst= aux(s, lst))
	  then []
	 else (aux(s, lst))
     end
				     
fun get_substitutions1 (lstst, s)=
    let	
	fun append(xs, ys)=
	    case  xs of
		[] => ys
	     |  x::xs' => x :: append(xs',ys)
    in
	case lstst of
	    [] => []
         | head::tail  => append(all_except_option1(s, head), get_substitutions1(tail, s))

    end

					
fun get_substitutions2 (lstst, s)=
         let	
         	fun append(xs, ys)=
	    case  xs of
		[] => ys
	     |  x::xs' => x :: append(xs',ys)

         in
	     let fun aux(s, lstst, acc) = 
             	case lstst of
	             [] => acc
   | head::tail  =>  aux(s,tail, append((all_except_option1(s,head)),acc))
	     in
		 aux(s, lstst, [])
	     end
    end


fun similar_names (lstst, {first= x, middle =y, last =z})=
		  let
		      fun helper (lst, {first= x, middle =y, last =z})=
			       case lst of
			 [] =>  []
      | head::tail =>  {first= head, middle =y, last =z}::helper(tail, {first= x, middle =y, last =z})

		  in
		      {first= x, middle =y, last =z}::helper((get_substitutions2(lstst, x)),{first= x, middle =y, last =z})

		  end
		  
		  
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card
	      | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)									      

fun card_color x =
    case x of
	(suit,rank) => if (suit = Clubs orelse suit = Spades)
			then Black 
		       else Red



fun card_value (suit, rank) =
    case rank of
	Num x=> x
      | Ace => 11
      | _ =>10

fun append(xs, ys)=
	    case  xs of
		[] => ys
	     |  x::xs' => x :: append(xs',ys)
				     
				    
fun remove_card (x, card, IllegalMove) =
    let
	 fun aux (card, x , acc)= 
	     case x of
		 []  => acc
	       | head::tail => if head = card
			       then append(tail, acc)
			       else aux(card, tail, head::acc)
				   
     in
	 if (x= aux(card, x,[]))
	  then raise IllegalMove
	 else (aux(card, x, []))
    end

fun all_same_color (x)=
	    case x of
		[] => true
	      | head::[] => true
	      | head::(neck::tail) => if card_color(head) = card_color(neck)
				      then true andalso all_same_color(neck::tail)
				      else false
					       
    
fun sum_cards (x)=
    let fun help (x, acc)=
	case x of
	[] => acc
      | head::tail =>  help (tail, (card_value(head))+acc)
    in
	help(x,0)
    end
	
	
fun score (x, goal)=
    let
	fun help (x, goal)=
	    if sum_cards(x)> goal
            then ((sum_cards x)-goal)*3
            else goal-(sum_cards x)
    in
	if all_same_color(x)
	then help(x, goal) div 2
	else help(x, goal)
    end


fun officiate(cardlist, movelist, goal)=
		 let
	
		     fun help (cardlist, movelist, goal, heldlist, IllegalMove)=
			 if sum_cards(heldlist) > goal
			 then score(heldlist, goal)
			 else
			 (case movelist of
			     [] => score(heldlist, goal)
			   | head_movelist::rest =>
			     case head_movelist of 
				   Draw =>
				    (case cardlist of
				        [] => score(heldlist, goal)
				      | head::tail  => help(remove_card(cardlist, head, IllegalMove), remove_card(movelist, head_movelist, IllegalMove), goal, (head::heldlist), IllegalMove)
				     )
			       |  Discard(card) => 
				( help(cardlist, remove_card(movelist, head_movelist, IllegalMove), goal, remove_card(heldlist, card, IllegalMove), IllegalMove))
				     )
		 in
		     

			 help (cardlist, movelist, goal, [], IllegalMove)		   
					  

	     end
	     

