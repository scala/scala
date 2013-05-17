import language.experimental.macros

object Macros {
  def foo1 = macro ???
  def foo2(x: Int) = macro ???
  def foo3[T] = macro ???
}