// $Id$

// Test constructors, including multiple ones.

import java.lang.System; // to avoid name clash with .NET's library

class A(x: Int, y: Int) {
  def this(x: Int) = this(x, x);
  def this() = this(1);
  override def toString() = "x=" + x + " y=" + y;
  class B(a: Int, b: Int, c: String) {
    def this(str: String) = this(x, y, str);
    override def toString() =
      "x=" + x + " y=" + y + " a=" + a + " b=" + b + " c=" + c;
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val a1 = new A(1,2);
    val a2 = new A(3);
    val a3 = new A();
    val b1 = new a1.B(1,2,"a");
    val b2 = new a2.B("b");
    System.out.println(a1);
    System.out.println(a2);
    System.out.println(a3);
    System.out.println(b1);
    System.out.println(b2);
  }
}
