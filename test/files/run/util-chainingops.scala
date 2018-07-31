object Test {
  implicit class SymMinus(s: Symbol) {
    def -(f: => Unit): Unit = f
  }
  import scala.util.chainingOps._

  def main(args: Array[String]): Unit = {

    'testAnyTap - {
      var x: Int = 0
      val result = List(1, 2, 3)
        .tap(xs => x = xs.head)
        .tap(println)

      assert(1 == x)
      assert(List(1, 2, 3) == result)
      ()
    }

    'testAnyPipe - {
      val times6 = (_: Int) * 6
      val result = (1 - 2 - 3)
        .pipe(times6)
        .pipe(scala.math.abs)

      assert(24 == result)
      ()
    }
  }
}
