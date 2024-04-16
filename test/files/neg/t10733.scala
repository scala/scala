//> using options -Yrangepos -Ystop-after:parser
trait T[_]
trait U[_, _]

object Test {
  val a: T[_ <: _] = null
  val b: T[_ >: _] = null

  def x[Y <: _] = null
  def y[X >: _ <: Int] = null

  val ok1 : ((_, Int) => Int) = (_, _) => 1
  val ok2 : (_, Long) = (1L, 1L)
  val ok3 : _ => String = (_: Class[_]).toString

}
