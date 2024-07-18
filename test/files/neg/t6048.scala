//> using options -Xfatal-warnings
//
class A {
  def f1(x: Int) = x match {
    case _ if false => x // unreachable
    case _          => x
  }

  def f2(x: Int) = x match {
    case _ if false => x // unreachable
    case _ if true  => x
  }

  def f3(x: Int) = x match {
    case _ => x
    case _ if true  => x // unreachable
  }

  def test1(x: Int) = x match {
    case c if c < 0 => 0
    case 1          => 1
    case _          => 2
  }
}
