//> using options -Werror

import reflect.ClassTag

object Test {
  def f[T: ClassTag](bar: Either[String, T]): Boolean = bar match { // not exhaustive
    case Left(_) => false
    case Right(_: T) => true
  }
  def g[T](bar: Either[String, T]): Boolean = bar match {
    case Left(_) => false
    case Right(_: T) => true // unchecked but mention that the test is extraneous
  }
  def h[T](bar: Either[String, T]): Boolean = bar match {
    case Left(_) => false
    case _: Right[_, T] => true
  }
  def um[T](bar: Either[String, Any]): Boolean = bar match {
    case Left(_) => false
    case Right(_: T) => true // unchecked simply
  }
}
