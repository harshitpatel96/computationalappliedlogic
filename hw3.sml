(*
*  Harshit Patel
*  hpatel24
*  CSC 503, Fall 2019
*)


structure Propositional =
struct
  
  datatype ('a, 'b) Or = Inl of 'a | Inr of 'b

  datatype Void = V of Void

  (* abort = fn : Void -> 'a *)
  fun abort (V v:Void) : 'a = abort v
  
  (* mkPair = fn : 'a -> 'a * 'a *)
  val mkPair = fn x => (x, x)
  
  (* blob = fn : 'a * ('b,'c) Or -> ('a * 'b,'a * 'c) *)
  val blob = 
    fn x:'a*(('b,'c) Or) => 
      case #2 x of 
           Inl y => Inl (#1 x, y) 
         | Inr z => Inr (#1 x, z)

  (* zorp = fn : ('a * 'a,'a) Or -> 'a *)
  val zorp = fn x => #1 (case x of Inl y => y | Inr z => (z,z)) 

  (* compose = fn : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *)
  val compose = fn f => fn g => fn x => g (f x)

  (* curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c *)
  val curry = fn f => fn g => fn h => f (g,h)

  (* uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c *)
  val uncurry = fn f => fn g:'a * 'b   =>  (f (#1 g))(#2 g)

  (* exceptn : ('a,'b) Or -> ('b -> Void) -> 'a *)
  val exceptn = fn f => fn g => (case f of Inl g => g | Inr h=>abort(g h) )
end

structure PrimRec =
struct

  datatype nat = Zero | Succ of nat

  datatype parity = Even | Odd

  datatype natOption = Inl | Inr of nat

  datatype boolean = True | False

  fun peanify (x:int) : nat =
	  if x=0 then Zero
	  else Succ (peanify (x-1))

  fun decimal (n:nat) : int =
	  case n of
	 	 Zero => 0
	    | (Succ n) => 1 + (decimal n)
	   
  fun plus (x:nat) (y:nat) : nat =
	  case x of
		 Zero => y
	   | (Succ x') => Succ (plus x' y)
 
  fun mul (x:nat) (y:nat) : nat =
	  case x of
		 Zero => Zero
	   | Succ x' => plus ((mul x' y)) y
	   

  fun fact (x:nat) : nat =
	  case x of
		 Zero => Succ Zero
	   | Succ x' => mul (x) (fact(x'))

  fun pred (x:nat) : natOption =
	  case x of 
		 Zero => Inl
	   | (Succ x') => Inr x'
	   
	(* A seperate predicate function that provides output in nat format. Useful in tsub *)
  fun prednew (x : nat) : nat = 
	  case x of
		 Zero => Zero
	   | (Succ x') => x'
	   
  fun geq (x:nat) (y:nat) : boolean =
	  case x of
		 Zero => (case y of 
					 Zero => True
				    | Succ y' => False)
	   | Succ x' => case y of 
					 Zero => True
				   | Succ y' => geq x' y'
			
	
				  
  fun tsub (x : nat) (y : nat) : nat =
	  case y of
		 Zero => x
	   | Succ y' => tsub (prednew x) y'

			   
  fun isqrt (x:nat) : nat =
	  case x of
		 Zero => Zero
	   | Succ x' =>
		   let
			 val n = isqrt x'
		   in
			 case (geq (Succ x') (mul (Succ n) (Succ n))) of
				 False => n
			   | True => Succ n
		   end;

	
  Control.Print.printDepth := 100;	
  (* Multiplication Tests *)
  
  val mul0 = mul (peanify 2) (peanify 2) (* Succ (Succ (Succ (Succ Zero))) *)
  val mul1 = mul (peanify 2) (peanify 3) (* Succ (Succ (Succ (Succ (Succ (Succ Zero))))) *)
  val mul2 = mul (peanify 3) (peanify 3) (* Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))) *)
  
  (* Fact Tests *)

  val fact0 = fact (peanify 2) (* Succ (Succ Zero) *)
  val fact1 = fact (peanify 3) (* Succ (Succ (Succ (Succ (Succ (Succ Zero))))) *)
  val fact2 = fact (peanify 4) (* Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))))))))))))))))) *)
  
  (* Pred Tests *)

  val pred0 = pred (peanify 0) (* Inl *)
  val pred0 = pred (peanify 2) (* Inr Succ (Succ Zero) *)
  val pred1 = pred (peanify 5) (* Inr Succ (Succ (Succ (Succ Zero))) *)
  
  (* Geq Tests *)
  
  val geq0 = geq (peanify 10) (peanify 15) (* False *)
  val geq1 = geq (peanify 15) (peanify 15) (* True *)
  val geq2 = geq (peanify 15) (peanify 10) (* True *)
  
  (* Truncated Subtraction Tests *)
  
  val tsub0 = tsub (peanify 10) (peanify 10) (* Zero *)
  val tsub1 = tsub (peanify 10) (peanify 5) (* Succ (Succ (Succ (Succ (Succ Zero)))) *)
  val tsub2 = tsub (peanify 10) (peanify 15) (* Zero *)
  
  (* Square Root Tests *)
  
  val sqrt0 = isqrt (peanify 4) (* Succ (Succ Zero) *)
  val sqrt1 = isqrt (peanify 10) (* Succ (Succ (Succ Zero)) *)
  val sqrt2 = isqrt (peanify 25) (* Succ (Succ (Succ (Succ (Succ Zero)))) *)
  
  val myst1 = mystery (peanify 0) (peanify 0)
  
end

