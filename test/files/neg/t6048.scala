class A {
  def f1(x: Int) = x match {
    case _ if false => x // unreachable
    case 5          => x
  }

  def f2(x: Int) = x match {
    case _ if false => x // unreachable
    case 5 if true  => x
  }

  def f3(x: Int) = x match {
    case _ => x
    case 5 if true  => x // unreachable
  }

  def test1(x: Int) = x match {
    case c if c < 0 => 0
    case 1          => 1
    case _          => 2
  }
}
