
class Named(var name: String)

class X(name: String) extends Named(name) {
  def x = "Hi, $name"   // missing interp
  def f(p: (Int, Int)): Int = p._1 * p._2
  def g = f(3, 4)       // adapted
  def u: Unit = ()      // unitarian universalist
  override def toString = name // shadowing mutable var name
}
