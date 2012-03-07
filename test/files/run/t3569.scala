object Test {
  lazy val lv = scala.util.Random.nextInt()
  
  class X(final var x: Int)  {
    final private[this] var x2: Int = 0
    final var x3: Int = 0
    private[this] var x4: Int = 0
    final private[this] var x5: Int = 0
    final lazy val x6: Int = 0
    final private[this] lazy val x7: Int = 0
  }
  case class Y(final var x: Int, final private var y: Int, var z1: Int, private var z2: Int) { }
  
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
