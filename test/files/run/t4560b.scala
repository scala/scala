object Outer {
  class Tester
  private[Outer] trait B4 { _: Tester =>
    protected val FREQ = 23
    def fail() = {
      println(FREQ)
    }
  }
  object C4 extends Tester with B4
}

object Outer2 {
  abstract class A5
  private[Outer2] trait C5 {
    def impl() { println("SUCCESS") }
  }
  trait B5 extends C5 { self: A5 =>
    def fail() { impl() }
  }
  object Test5 extends A5 with B5 with C5
}

object Test {
  def main(args: Array[String]): Unit = {
    Outer.C4.fail()
    Outer2.Test5.fail()
  }
}
