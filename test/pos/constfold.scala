object A {
  val x = 2;
  val y = x.asInstanceOf[byte];
  val z = 1.0 / 2;
  val s = "z is " + z;
}

object Test with Application {

    System.out.println(A.x);
    System.out.println(A.y);
    System.out.println(A.z);
    System.out.println(A.s);
}
