class A {
  def f: Boolean = {
    val xs = Nil map (_ => return false)
    true
  }
}
