object Test extends Application {
  import BigInt._

  val x: BigInt = 1
  val y = x + 1
  val z = 1 + y
  Console.println(z)
  Console.println(z <= 3)
  Console.println(3 < z)
  Console.println(z == 3)
  Console.println(3 == z)

}
