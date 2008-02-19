object Test extends Application {
  Test1
  Test2
}

object Test1 {
  import BigInt._

  val x: BigInt = 1
  val y = x + 1
  val z = 1 + y
  println(z)
  println(z <= 3)
  println(3 < z)
  println(z == 3)
  println(3 == z)
  println()
}

object Test2 {
  import BigDecimal._

  val x: BigDecimal = 1
  val y = x + 1
  val z = 1 + y
  println(z)
  println(z <= 3)
  println(3 < z)
  println(z == 3)
  println(3 == z)

  val a = BigDecimal(Math.MAX_LONG)
  val b = BigDecimal(Test1.x)
  val c = a - b
  println(c)
  println(c > Math.MAX_LONG)
  println(c <= Math.MAX_LONG)
}
