import language.experimental.macros

object Macros {
  def foo1: Any = macro ???
  foo1

  def foo2(x: Int): Any = macro ???
  foo2
  foo2(1)

  def foo3[T]: Any = macro ???
  foo3[Int]
}
