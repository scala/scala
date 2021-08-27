
package p

object Sample

trait T {
  def f(x: Any) =
    x match {
      case Sample => 1
      case Simple => 2
      case Simple.member => 3
      case `sample` => 4
      case _: Simple => 5
      case Simple(_) => 6
      case _ => 7
    }
  def g = p.Simple

  val x :: Nil = List(42)

  val X :: Nil = List(42)
}
