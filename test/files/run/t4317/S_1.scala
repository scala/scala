import language.existentials

object S_1 {
  def foo1(x: Class[_ <: AnyRef]) = 0
  def foo2(x: Class[_ <: AnyRef], y: Int) = 99
  def foo3[T](x: Int, y: Int) = x + y
  def foo4a(x: Unit): Unit = ()
  def foo4[T](x: Unit): Unit = ()
  def foo5[T <: Unit](x: T): T = sys.error("")
  def foo6[T](x: Class[_], y: Class[T], z: Class[_ <: T]) = ((x, y, z))
}
