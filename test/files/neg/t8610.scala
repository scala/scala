
case class X(name: String) {
  def x = "Hi, $name"   // missing interp
  def f(p: (Int, Int)): Int = p._1 * p._2
  def g = f(3, 4)       // adapted
  def u: Unit = ()      // unitarian universalist
}
