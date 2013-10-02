class A
class B

trait X

object Z {
  def transf(a: A, b: B): X = null
}

class Test {

  def bar(): (A, B)

  def foo {
    val (b, a) = bar()
    Z.transf(a, b) match {
      case sth =>
        run(sth, b)
    }
  }

  def run(x: X, z: B): Unit = ()
}
