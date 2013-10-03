class A(private val x: Int) extends AnyVal

object A {
  val Const = new A(0)
}

class A1(protected val x: Int) extends AnyVal

package p {
  class A2(private[p] val x: Int) extends AnyVal
}
