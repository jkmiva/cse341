(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* 1 *)
fun only_capitals xs =
  List.filter (fn x => Char.isUpper (String.sub (x,0))) xs

(* 2 *)
fun longest_string1 xs =
  List.foldl (fn (x,y) => if String.size x > String.size y then x else y) "" xs

(* 3 *)
fun longest_string2 xs =
  List.foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" xs

(* 4 *)
fun longest_string_helper f xs =
  List.foldl (fn (x,y) => if f (String.size x, String.size y) then x else y) "" xs

val longest_string3 = longest_string_helper (fn (x,y) => x>y)
val longest_string4 = longest_string_helper (fn (x,y) => x>=y)

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
fun rev_string x =
  (String.implode o List.rev o String.explode) x

(* 7 *)
fun first_answer f xs =
  case xs of
      [] => raise NoAnswer
    | x::xs' => case f x of
                    SOME v => v
                  | NONE => first_answer f xs'


(* 8 *)
fun all_answers f xs =
  let
    fun helper (xs, acc) =
      case xs of
          [] => SOME acc
        | x::xs' => case f x of
                        NONE => NONE
                      | SOME lst => helper(xs', lst @ acc)
  in
    helper (xs,[])
  end


(* 9 *)
(** a **)
fun count_wildcards p =
  g (fn () => 1) (fn _ => 0) p

(** b **)
fun count_wild_and_variable_lengths p =
  g (fn () => 1) String.size p

(** c **)
fun count_some_var (s,p) =
  g (fn () => 0) (fn x => if x=s then 1 else 0) p


(* 10 *)
fun check_pat p =
  let
    fun all_strings (p,acc) =
      case p of
          Variable s => s::acc
        | TupleP ps => List.foldl (fn (x,y) => all_strings(x,y)) acc ps
        | ConstructorP (_,p) => all_strings(p,acc)    (*  "DON'T TAKE STRING OF CONSTRUCTORP INTO ACCOUNT, before revision: ConstructorP (s,p) => all_strings(p,s::acc) *)
        | _ => acc
    fun is_unique xs =
      case xs of
          [] => true
        | x::xs' => if (List.exists (fn s => x=s) xs')
                    then false
                    else is_unique xs'
    val s = all_strings (p, [])
  in
    is_unique s
  end

(* 11 *)
fun match (v,p) =
  (case (v,p) of
       (_,Wildcard) => SOME []
     | (_, Variable s) => SOME [(s,v)]
     | (Unit,UnitP) => SOME []
     | (Const c1,ConstP c2) => if c1=c2 then SOME [] else NONE
     | (Tuple vs,TupleP ps) => let val pairs = ListPair.zipEq (vs,ps) in all_answers match pairs end
     | (Constructor(s2,v2),ConstructorP(s1,p1)) => if s2=s1 then match (v2,p1) else NONE
     | _ => NONE
  )
  handle UnequalLengths => NONE

(* 12 *)
fun first_match v ps =
  SOME (first_answer (fn p => match (v,p)) ps)
  handle NoAnswer => NONE
