// tricky to do differently?
class C

object Test {
  def f = { object o extends C; o}
  val y: C = f
  val x = f
}
