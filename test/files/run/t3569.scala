object Test {
  final val bippy1 = 1
  final lazy val bippy2 = 2

  lazy val lv = scala.util.Random.nextInt()

  trait T { final lazy val const1 = 1 } // no fields

  class X(final var x: Int) extends T {
    // a lazy val does not receive a constant type, for backwards compat (e.g. for the repl)
    // besides, since you explicitly wanted something lazy, we'll give you something lazy! (a field and a bitmap)
    final lazy val const2 = 2

    final var var1: Int = 0
    final private var var2: Int = 0
    final private[this] var var3: Int = 0

    final val val1: Int = 1
    final private val val2: Int = 1
    final private[this] val val3: Int = 1

    final lazy val lval1: Int = 2
    final private lazy val lval2: Int = 2
    final private[this] lazy val lval3: Int = 2
  }
  case class Y(final var x: Int, final private var y: Int, final val z1: Int, final private val z2: Int) { }

  def f = new X(0).x += 1
  def main(args: Array[String]) {
    f
    val s = new X(0)
    s.x += 1
    println(s.x)

    // under -Xcheckinit there's an additional $init$ field
    (classOf[X].getDeclaredFields map ("" + _)).sorted.filter(_ != "private volatile byte Test$X.bitmap$init$0") foreach println
    (classOf[Y].getDeclaredFields map ("" + _)).sorted foreach println
  }
}
