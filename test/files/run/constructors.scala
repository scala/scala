// Test constructors, including multiple ones.

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
    Console.println(a1);
    Console.println(a2);
    Console.println(a3);
    Console.println(b1);
    Console.println(b2);
  }
}
