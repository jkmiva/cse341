(* community practice problems, week 4 *)

(* 1.There can be only one *)
fun fold_map f =
  List.foldr (fn (x,acc) => f x :: acc) []

fun fold_filter f =
  List.foldr (fn (x,acc) => if f x then x :: acc else acc) []

(* 2.The evil twin *)
fun unfold f s =
  case f s of
      NONE => []
    | SOME (a,b) => b::(unfold f a)

(* 3.A novel approach *)
fun factorial n =
  let val xs = unfold (fn x => if x < 1 then NONE else SOME (x-1,x)) n
  in
    List.foldl (fn (x,y) => x*y) 1 xs
  end
(*
val test_factorial_1 = factorial 4 = 24
val test_factorial_2 = factorial 0 = 1
val test_factorial_3 = factorial 5 = 120
val test_factorial_4 = factorial 7 = 5040
*)

(* 4.Unforeseen developments *)
fun unfole_map f xs =
  let
    fun helper xs =
      case xs of
          [] => NONE
        | x::xs' => SOME (xs', f x)
  in
    unfold helper xs
  end

(* 5.So imperative *)
fun do_until f p x =
  if p x then x else do_until f p (f x)

(* 6.Yet another factorial *)
fun imp_factorial n =
  #1 (do_until (fn (acc,x) => (acc*x, x-1)) (fn (_,x) => x = 0) (1,n))

(* 7.Fixed Point *)
fun fixed_point f x =
  if (f x) = x then x else fixed_point f (f x)

fun fixed_point2 f = do_until f (fn x => f x = x)

(* 8.Newton's Method *)
fun my_sqrt n =
  let
    fun fixed_point f = do_until f (fn x => abs (f x - x) < 0.0001)
  in
    fixed_point (fn x => 0.5 * (x + n/x)) n
  end
