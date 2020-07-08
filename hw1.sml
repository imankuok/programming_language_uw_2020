fun is_older(d1:int*int*int, d2:int*int*int)=
	    if (#1 d1) < (#1 d2) then true
	    else if (#1 d1) > (#1 d2) then false
	    else if (#2 d1) < (#2 d2) then true
	    else if (#2 d1) > (#2 d2) then false
	    else if (#3 d1) < (#3 d2) then true
	    else if (#3 d1 > #3 d2) then false
	    else false

fun number_in_month(date:(int*int*int) list, month:int)=
    if null date
    then 0
    else
	let
	    val x = number_in_month(tl date, month)
	in
	    if #2 (hd date) = month
	    then 1 + x
	    else x
	end
	    
fun number_in_months(date:(int*int*int) list, month:int list)=
    if null month
    then 0
    else
	number_in_month(date, hd month) + number_in_months(date, tl month)


fun dates_in_month(date:(int*int*int) list, month:int)=
    if null date
    then []
    else
	let
	    val x = dates_in_month((tl date), month)
	in
	    if #2 (hd date) = month
	    then (hd date)::x
	    else x
	end

fun append(xs: (int*int*int) list, ys:(int*int*int) list)=
    if null xs
    then ys
    else (hd xs)::append((tl xs),ys)
			

fun dates_in_months(date:(int*int*int) list, month:int list)=
    if null month
    then []
    else
	append(dates_in_month(date, hd month), dates_in_months(date, tl month))
	      

fun get_nth(x: string list, y:int)=
	if y = 1
	then hd x
	else get_nth(tl x, y-1)

fun date_to_string(x:int, y:int, z:int)=
    let
	val month_name = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in

	get_nth(month_name,y)^" "^(Int.toString z)^", "^(Int.toString x)

    end


fun number_before_reaching_sum(sum:int, x :int list)=
    if null x
    then 0
	     
    else
	(if null (tl x) then 0  else
	(if (sum - (hd x) > (hd (tl x)))
	then 1 + number_before_reaching_sum((sum - (hd x)), (tl x))
	else 1))
		 
	   
fun what_month(x:int)=
    let
	val month_n = [0,31,28,31,30,31,30,31,31,30,31,30,31]

    in
	number_before_reaching_sum(x, month_n)
				   
    end



fun month_range(x:int, y:int)=
    if x>y
    then []
    else
	what_month(x)::month_range(x+1, y)


fun oldest(dates: (int*int*int) list)=
    if null dates
    then NONE
    else
	let (* (int*int*int) list  -> (int*int*int) *)

	    fun max_nonempty(dates: (int*int*int) list)=
		if null (tl dates)
		then hd dates
		else
		    let
			val tl_ans = max_nonempty (tl dates)
		    in
			if is_older(hd dates, tl_ans)
			then hd dates
			else tl_ans
		    end
			
	in
	    SOME (max_nonempty dates)
	end
	    
	
			
	
