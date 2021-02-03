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

object TwentyThree {
  def unapply(a: Array[Int]): Option[TwentyThree] = {
    Option.when(a.length == 23) {
      TwentyThree(
        a(0), a(1), a(2), a(3), a(4), a(5), a(6), a(7), a(8), a(9),
        a(10), a(11), a(12), a(13), a(14), a(15), a(16), a(17), a(18), a(19),
        a(20), a(21), a(22),
      )
    }
  }
}

// This is borked.. but I'm fairly certain it's borked since before I started meddling with it..
object Test extends App {
  val x = new TwentyThree(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
  val TwentyThree(a, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, b) = x
  println((a, b))
  val arr = Array(0 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
  val TwentyThree(a2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, b2) = arr
  println((a2, b2))
}
