import language.experimental.macros

object Macros {
  def foo1: Nothing = macro ???
  def foo2(x: Int): Nothing = macro ???
  def foo3[T]: Nothing = macro ???
}
