import language.experimental.macros

object Macros {
  def foo1 = macro ???
  foo1

  def foo2(x: Int) = macro ???
  foo2
  foo2(1)

  def foo3[T] = macro ???
  foo3[Int]
}