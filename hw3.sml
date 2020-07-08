val only_capitals = List.filter(fn xs=> Char.isUpper (String.sub (xs,0)))

fun longest_string1 xs = List.foldl (fn (x,y) => if (String.size x) > (String.size y) then x else y) "" xs

fun longest_string2 xs =  List.foldl (fn (x,y) => if (String.size y) > (String.size x) then y else x) "" xs

fun longest_string_helper (x,y) =  if (String.size x) > (String.size y) then x else y
								      
fun curry  f x y = f(x,y)
					   
val longest_string3 =  fn xs => List.foldl (fn (x,y)=> (((curry longest_string_helper) x) y)) "" xs
val longest_string4 =  fn xs => List.foldl (fn (x,y)=> (((curry longest_string_helper) y) x)) "" xs
					   			    
val longest_capitalized = fn xs => (longest_string1 (only_capitals xs))
				       
fun rev_string(x) = implode (rev (explode x))
			    

exception NoAnswer


	      
fun first_answer f = fn lst =>
    case lst of
	[] => raise NoAnswer
      | head::tail => case f head of
			  NONE => first_answer f tail
			| SOME head => head

fun all_answers f = fn lst =>
		       let
			   fun aux(lst,acc) =
			       case lst of
				   [] => acc
				 | head::tail => case f head of
						     NONE => aux(tail,acc)
						   | SOME head => head@aux(tail,acc)
		       in
			   if aux(lst, []) = []
			   then NONE
			   else SOME (aux(lst, []))
		       end



datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu
	

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

					   
val count_wildcards = g (fn () => 1) (fn _ => 0)
	     
val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x)	     
    
fun count_some_var (x, pattern) = g (fn () => 0) (fn y => if x = y then 1 else 0 ) pattern



				    
    
	
			       
			 
		    
	
		 
				 
											 
    
    
		    
				     
