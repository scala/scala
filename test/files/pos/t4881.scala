class Contra[-T]
trait A
trait B extends A
trait C extends B

object Test {
  def contraLBUB[a >: C <: A](): Contra[a] = null
  def contraLB[a >: C](): Contra[a] = null
  val x = contraLBUB() //inferred Contra[C] instead of Contra[A]
  val x1: Contra[A] = x
  val y = contraLB() //inferred Contra[C] instead of Contra[Any]
  val y1: Contra[Any] = y
}
