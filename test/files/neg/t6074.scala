class A private () { }
class B { }
object T {
  implicit def a2b(a: A): B = null
  def t = new A()
}
