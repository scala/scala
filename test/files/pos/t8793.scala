package regr 
 
trait F[A]
 
class G(val a: F[_], val b: F[_])
 
object G {
  def unapply(g: G) = Option((g.a, g.b))
}
 
object H {
  def unapply(g: G) = g match {
    case G(a, _) => Option(a)
  }
}
