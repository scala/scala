//> using options -Xsource:3
trait C {
  private trait Style
  private object P

  def a() = P.getClass
  def b(s: AnyRef) = { val c = s.asInstanceOf[Style].getClass; c }

  private def foo(s: Style) = s.getClass
  def c(s: AnyRef) = foo(s.asInstanceOf[Style])
}
