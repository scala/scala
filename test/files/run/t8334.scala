object Test extends App {
  def f: Boolean = {
    val xs = Nil map (_ => return false)
    true
  }

  def g: Boolean = {
    val xs = Nil collect { case _ => return false }
    true
  }

  def h: Boolean = {
    val xs = Nil flatMap { _ => return false }
    true
  }
  assert(f && g && h)
}
