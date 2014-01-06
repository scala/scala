trait AbsFunction1[@specialized -T, @specialized +U] {
  def apply(x: T): U
}

final class IntTest {

  val niters = 10000

  def transF(xs: Array[Int], f: AbsFunction1[Int, Int]) = {
    var i = 0
    var s = 0
    while (i < xs.length) {
      xs(i) = f(xs(i)) + 1
      i += 1
    }
  }

  def run() {
    val xs = new Array[Int](10000)
    val f = new AbsFunction1[Int, Int] {
      def apply(x: Int): Int = x * x
    }
    for (j <- 0 until niters) {
      transF(xs, f)
    }
    var acc = 0
    for (i <- 0 until xs.length) acc += xs(i)
    println(acc)
  }
}

final class ClosureTest {

  val niters = 10000

  def transF(xs: Array[Int], f: Int => Int) = {
    var i = 0
    var s = 0
    while (i < xs.length) {
      xs(i) = f.apply(xs(i)) + 1
      i += 1
    }
  }

  def run() {
    val xs = new Array[Int](10000)
//    val f = (x: Int) => x * x
    for (j <- 0 until niters) {
      transF(xs, x => x * x)
    }
    var acc = 0
    for (i <- 0 until xs.length) acc += xs(i)
    println(acc)
  }
}

object TestRunner {
  (new IntTest).run()
  (new ClosureTest).run()
}
