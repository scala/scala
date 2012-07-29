object Test extends App {

  case class C[T, U <: String, O >: Object](x: Int, y: T)(z: U, b: Boolean)(s: O, val l: Int)

  val c = C(1, true)("dlkfj", true)("dlkfjlk", 10)
  println(c)
  println(c.l)

  println(c.copy(y = 20, x = 7283)("enwa", b = false)(l = -1, s = new Object))

  val res = c.copy[Int, String, Object](y = -3, x = 66)("lkdjen", false)(new Object, 100)
  println(res)
  println(res.l)
}
