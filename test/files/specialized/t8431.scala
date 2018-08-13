import scala.{ specialized => spec }

trait EqProductFoo[@spec(Int) A] {
  def eqv(x0: A): Boolean = true
}

trait OrderProductFoo[@spec(Int) A] extends EqProductFoo[A] {
  override def eqv(x0: A): Boolean = super.eqv(x0)
}

class C extends OrderProductFoo[Int]

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    println(c.eqv(42))
  }
}
