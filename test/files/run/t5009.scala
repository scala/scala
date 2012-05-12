object Test extends App {

  case class C[T, U <: String, O >: Object](x: Int, y: T)(z: U, b: Boolean)(s: O, val l: Int)

  val c = C(1, true)("dlkfj", true)("dlkfjlk", 10)
  println(c)
  println(c.l)

  val f1a = c.copy(y = 20, x = 7283)

  val f1b = c.copy[Int, String, Object](y = 20, x = 7283)
  val f2b = f1b("lkdjen", false)
  val res = f2b(new Object, 100)
  println(res)
  println(res.l)
  
}
