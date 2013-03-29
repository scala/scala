case class TwentyThree(
  _1: Int,
  _2: Int,
  _3: Int,
  _4: Int,
  _5: Int,
  _6: Int,
  _7: Int,
  _8: Int,
  _9: Int,
  _10: Int,
  _11: Int,
  _12: Int,
  _13: Int,
  _14: Int,
  _15: Int,
  _16: Int,
  _17: Int,
  _18: Int,
  _19: Int,
  _20: Int,
  _21: Int,
  _22: Int,
  _23: Int
)

object Test extends App {
  val x = new TwentyThree(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
  println(x._23)
  assert(x.copy(_1 = 1) == x)
  val TwentyThree(a, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, b) = x
  println((a, b))
}
