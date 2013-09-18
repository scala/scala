object Test {
  final val bippy1 = 1
  final lazy val bippy2 = 2

  lazy val lv = scala.util.Random.nextInt()

  class X(final var x: Int)  {
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

    (classOf[X].getDeclaredFields map ("" + _)).sorted foreach println
    (classOf[Y].getDeclaredFields map ("" + _)).sorted foreach println
  }
}
