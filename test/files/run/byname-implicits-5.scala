
object Test extends App {
  var x = 0

  def foo(implicit y0: => Int): Int = {
    x = 0
    val y = y0 // side effect
    y*x
  }

  implicit lazy val bar: Int = { x = 1 ; 23 }

  assert(foo == 23)
}
