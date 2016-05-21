
object | { def unapply(x: (Any, Any)) = Some(x) }

trait Test {
  def f() = (1,2) match { case 1 `|` 2 => }
  def g() = 2 match { case 1 | 2 => }
}
