trait IllKind1 {
  def g(s: String): String = s
  def f: String = ???
  def f[C](c: C): String = g(f)
}

trait IllKind2 {
  def b1: Char = ???
  def b2: Byte = ???

  def f1 = "abc" contains b1
  def f2 = "abc" contains b2
}
