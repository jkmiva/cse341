(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* a *)
fun all_except_option (str, strlst) =
  let fun getSub (istr,ret) =
        case istr of
            [] => NONE
          | s::strlst' => if same_string(str, s)
                          then SOME (ret @ strlst')
                          else getSub(strlst',s::ret)
  in
    getSub (strlst,[])
  end

(* b *)
fun get_substitutions1 (strlsts, str) =
  case strlsts of
      [] => []
    | strlst::strlsts' => let val sub = all_except_option (str, strlst)
                          in
                            case sub of
                                NONE => get_substitutions1(strlsts', str)
                              | SOME s => s @ get_substitutions1(strlsts', str)
                          end

(* c *)
fun get_substitutions2 (strlsts, str) =
  let fun getSub (istr,ret) =
        case istr of
            [] => ret
          | s::istr' => let val sub = all_except_option (str, s)
                        in
                          case sub of
                              NONE => getSub(istr',ret)
                            | SOME s => getSub(istr', ret @ s)
                        end
  in
    getSub(strlsts,[])
  end

(* d *)
fun similar_names (strlsts, {first=f,middle=m,last=l})=
  let fun getSub(istr, ret) =
        case istr of
            [] => ret
          | s::istr' => getSub(istr', ret @ [{first=s,middle=m,last=l}])
  in
    let val sub = get_substitutions2(strlsts, f)
    in
      getSub(sub,[{first=f,middle=m,last=l}])
    end
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

(* a *)
fun card_color x =
  case x of
      (Clubs, _) => Black
    | (Diamonds, _) => Red
    | (Hearts, _) => Red
    | (Spades, _) => Black


(* b *)
fun card_value x =
  case x of
      (_, Ace) => 11
    | (_, Num n) => n
    | _ => 10


(* c *)
fun remove_card (cs, c, e) =
  let fun getSub (cs, ret) =
        case cs of
            [] => raise e
          | x::cs' => if x = c
                      then ret @ cs'
                      else getSub(cs',x::ret)
  in
    getSub(cs,[])
  end


(* d *)
fun all_same_color cs =
  case cs of
      [] => true
    | c1::[] => true
    | c1::(c2::cs') => card_color c1 = card_color c2 andalso all_same_color (c2::cs')


(* e *)
fun sum_cards cs =
  let fun getSum (cs, sum) =
        case cs of
            [] => sum
          | c::cs' => getSum(cs', sum + card_value(c))
  in
    getSum(cs,0)
  end


(* f *)
fun score (cs, goal) =
  let val isSameColor = all_same_color cs
      val sum = sum_cards cs
  in
    if sum > goal
    then
      if isSameColor
      then 3 * (sum - goal) div 2
      else 3 * (sum - goal)
    else
      if isSameColor
      then (goal - sum) div 2
      else (goal - sum)
  end


(* g *)
fun officiate (cs, ms, goal) =
  let fun action (hs, ms, cs) =
        case ms of
            [] => score(hs, goal)
          | m::ms' => case m of
                          Discard c => action(remove_card(hs,c,IllegalMove), ms', cs)
                        | Draw => case cs of
                                      [] => score(hs, goal)
                                    | c::cs' => if sum_cards(c::hs) > goal
                                                then score(c::hs, goal)
                                                else action(c::hs, ms', cs')
  in
    action ([], ms, cs)
  end
