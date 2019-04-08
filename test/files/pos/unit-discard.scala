// scalac: -Yunit-discard

trait T {
  def f() = 42
}
trait U {
  def t: T = ???
  def g: Int = t f ()
}
