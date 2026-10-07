class JSFunction

trait JSFunction0[+R] extends JSFunction {
  def apply(): R
}

object A {
  def lambdaAnything() = test(() => 1)
  def test(cmp: JSFunction0[Int]) = ()
}
