object Pair {
 sealed trait Pair {
   type First
   type Second <: Pair
 }

 class End extends Pair {
   type First = Nothing
   type Second = End

   def ::[T](v : T) : Cons[T, End] = Cons(v, this)
 }

 case object End extends End

 final case class Cons[T1, T2 <: Pair](_1 : T1, _2 : T2) extends Pair {
   type First = T1
   type Second = T2

   def ::[T](v : T) : Cons[T, Cons[T1, T2]] = Cons(v, this)
   def find[T](implicit finder : Cons[T1, T2] => T) = finder(this)
 }

 implicit def findFirst[T1, T2 <: Pair] : Cons[T1, T2] => T1 = (p : Cons[T1, T2]) => p._1
 implicit def findSecond[T, T1, T2 <: Pair](implicit finder : T2 => T) : Cons[T1, T2] => T = (p : Cons[T1, T2]) => finder(p._2)

 val p : Cons[Int, Cons[Boolean, End]] = 10 :: false :: End
// val x : Boolean = p.find[Boolean](findSecond(findFirst))
 val x2 : Boolean = p.find[Boolean]  // Doesn't compile
}
