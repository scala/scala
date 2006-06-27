object Test extends Application {
  import BigInt._

  val x: BigInt = 1
  val y = x + 1
  val z = 1 + y
  System.out.println(z)
  System.out.println(z <= 3)
  System.out.println(3 < z)
  System.out.println(z == 3)
  System.out.println(3 == z)

}

