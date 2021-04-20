object Test {
  case class O(i: Int) {
    class A
    class B extends A {
      def bOuter = O.this
    }
    trait C {
      def cOuter = O.this
    }
    class D extends o2.B with C
  }
  val o1 = new O(1);
  val o2 = new O(2);
  def pat1(a: Test.o1.C) = a match {
    case b: Test.o1.B =>
      assert(b.bOuter eq Test.o1,
             s"expected ${o1} as outer of value conforming to pattern `b: Test.o1.B`, but got ${b.bOuter}")
    case _ =>

  }
  def main(args: Array[String]): Unit = {
    pat1(new o1.D)
  }
}
