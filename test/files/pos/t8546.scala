package test

class F1() {
  private sealed abstract class T
  private case class A(m: Int) extends T
  private case class B() extends T
  private case object C extends T

  // No warnings here
  private def foo(t: T) = t match {
    case A(m) => println("A:" + m)
    case B() => println("B")
    case C => println("C")
  }

  def test(m: Int): Unit = {
    foo(A(m))
    foo(B())
    foo(C)
  }
}

class F2[M]() {
  private sealed abstract class T
  private case class A(m: M) extends T
  private case class B() extends T
  private case object C extends T

  // match may not be exhaustive. It would fail on the following input: C
  private def foo(t: T) = t match {
    case A(m) => println("A:" + m)
    case B() => println("B")
    case C => println("C")
  }

  def test(m: M): Unit = {
    foo(A(m))
    foo(B())
    foo(C)
  }

}

object Test {
  def main(args: Array[String]): Unit = {
    new F1().test(1)
    new F2[Int]().test(1)
  }
}