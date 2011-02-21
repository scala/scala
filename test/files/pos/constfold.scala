object A {
  val x = 2;
  val y = x.asInstanceOf[Byte];
  val z = 1.0 / 2;
  val s = "z is " + z;
}

object Test extends App {

    Console.println(A.x);
    Console.println(A.y);
    Console.println(A.z);
    Console.println(A.s);
}
