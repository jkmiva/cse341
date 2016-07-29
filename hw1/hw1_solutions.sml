(* solutions to hw1*)

(* 1 *)
fun is_older (d1 : (int*int*int), d2 : (int*int*int)) =
  if (#1 d1) < (#1 d2)
  then true
  else if (#1 d1) = (#1 d2)
  then if (#2 d1) < (#2 d2)
       then true
       else if (#2 d1) = (#2 d2)
       then if (#3 d1) < (#3 d2)
            then true
            else false
       else false
  else false

(* 2 *)
fun number_in_month (dl : (int*int*int) list, m : int) =
  if null dl
  then 0
  else if (#2 (hd dl)) = m
  then 1 + number_in_month(tl dl, m)
  else number_in_month (tl dl, m)

(* 3 *)
fun number_in_months (dl : (int*int*int) list, ml : int list) =
  if null ml
  then 0
  else number_in_month(dl, hd ml) + number_in_months(dl, tl ml)

(* 4 *)
fun dates_in_month (dl : (int*int*int) list, m : int) =
  if null dl
  then []
  else if (#2 (hd dl)) = m
  then (hd dl) :: dates_in_month(tl dl, m)
  else dates_in_month(tl dl, m)

(* 5 *)
fun dates_in_months (dl : (int*int*int) list, ml : int list) =
  if null ml
  then []
  else dates_in_month(dl, hd ml) @ dates_in_months(dl, tl ml)

(* 6 *)
fun get_nth (sl : string list, n : int) =
  if n = 1
  then hd sl
  else get_nth(tl sl, n-1)

(* 7 *)
fun date_to_string (d : int*int*int) =
  let
    val months = ["January","February","March","April","May","June",
                "July","August","September","October","November","December"]
  in
    get_nth(months, #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
  end

(* 8 *)
fun number_before_reaching_sum (sum : int, l : int list) =
  if sum <= hd l
  then 0
  else 1 + number_before_reaching_sum(sum - hd l, tl l)

(* 9 *)
fun what_month (day : int) =
  let
    val dayList = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
    1 + number_before_reaching_sum(day, dayList)
  end

(* 10 *)
fun month_range (day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1+1, day2)

(* 11 *)
fun oldest (dl : (int*int*int) list) =
  if null dl
  then NONE
  else if null (tl dl)
  then SOME (hd dl)
  else if is_older(hd dl, valOf (oldest(tl dl)))
  then SOME (hd dl)
  else oldest(tl dl)

(* 12 *)
fun dates_in_months_challenge (dl : (int*int*int) list, ml : int list) =
  let
    fun isIn (gl : int list, x : int) =
      if null gl
      then false
      else ((x = (hd gl)) orelse isIn(tl gl, x))

    fun rmvDup (m : int list) =
      if null m
      then []
      else
        let val m_ans = rmvDup(tl m)
        in
          if isIn(m_ans, hd m)
          then m_ans
          else (hd m) :: m_ans
        end
  in
    dates_in_months(dl,rmvDup(ml))
  end

(* extra *)
fun cumulative_sum (xs : int list) =
