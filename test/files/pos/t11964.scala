//> using options -Werror -Xlint

object Hmm {
  def zxc(b: Int*)(implicit x: Int = 3) = "" + b + x
  def res = zxc(4)
}

object Test {
  def foo(a: Any, b: Any = null, c: Any = null)(cs: String*) = ???
  def res = foo("", c = "")("X")
}

object OP {
  def f(a: Int, b: String*) = "first"
  def res = f(b = "sl19", a = 28)  // looks like the issue is only with single arg supplied to varargs.
  def or  = f(b = ("x"::"y"::Nil):_*, a = 42)  // 2.13 syntax only
  //def and = f(b = ("x"::"y"::Nil):_*)   // broken under 2.13, which disallows default + varargs
  def and = List(elems = ("x"::"y"::Nil):_*)
}
