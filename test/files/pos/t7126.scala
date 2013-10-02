import language._

object Test {
  type T = Any
  boom(???): Option[T] // SOE
  def boom[CC[U]](t : CC[T]): Option[CC[T]] = None

  // okay
  foo(???): Option[Any]
  def foo[CC[U]](t : CC[Any]): Option[CC[Any]] = None
}