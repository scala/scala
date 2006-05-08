object Test {

  abstract class Foo(_a: Int, _b: Int) {

    Console.println("Foo 1: " + this);
    val a: Int = _a;
    Console.println("Foo 2: " + this);
    val b: Int = { fun(); _b }
    Console.println("Foo 3: " + this);
    val x: Int;
    Console.println("Foo 4: " + this);
    val y: Int;
    Console.println("Foo 5: " + this);


    def fun(): Unit = ();

    override def toString(): String =
      "a = " + a + ", b = " + b + ", x = " + x + ", y = " + y;

  }

  class Bar(_a: Int, _b: Int, _x: Int, _y: Int) extends Foo(_a, _b) {

    Console.println("Bar 1: " + this);
    val x: Int = _x;
    Console.println("Bar 2: " + this);
    val y: Int = { fun(); _y }
    Console.println("Bar 3: " + this);

  }

  def main (args: Array[String]): Unit = {
    new Bar(2,3,5,7);
  }

}
