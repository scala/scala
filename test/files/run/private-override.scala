package test.p1.p2 {
  abstract class A {
    private[p2] def f2(): Int = 1
  }
  abstract class Other extends A {
    // It's a private method - not a private[p2] method. Not a failed
    // "weaker access privileges" override, a different namespace.
    private def f2(): Int = super.f2() + 2
    def go() = f2()
  }
}

object Test extends test.p1.p2.Other {
  def main(args: Array[String]): Unit = {
    println(go())
  }
}
