class Contra[-T]
trait A
trait B extends A
trait C extends B

// test improved variance inference: first try formals to see in which variance positions the type param appears;
// only when that fails to determine variance, look at result type
object Test {
  def contraLBUB[a >: C <: A](): Contra[a] = null
  def contraLB[a >: C](): Contra[a] = null

{
  val x = contraLBUB() //inferred Contra[C] instead of Contra[A]
  val x1: Contra[A] = x
}

{
  val x = contraLB() //inferred Contra[C] instead of Contra[Any]
  val x1: Contra[Any] = x
}

{
  val x = contraLBUB // make sure it does the same thing as its ()-less counterpart
  val x1: Contra[A] = x
}

{
  val x = contraLB
  val x1: Contra[Any] = x
}
}
