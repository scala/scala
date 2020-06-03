object Test extends App {
  class F[T]
  object F {
    implicit def fi: F[Int] = new F[Int]
    implicit def fu: F[Unit] = new F[Unit]
  }
  case class Widened[T](value: Boolean)
  object Widened {
    implicit def notWidened[T <: Singleton]: Widened[T] = Widened(false)
    implicit def widened[T]: Widened[T] = Widened(true)
  }

  def widened[T](t: T)(implicit w: Widened[T], @annotation.unused f: F[T]): Boolean = w.value

  def boundedWidened[T <: Singleton](t: T)(implicit w: Widened[T]): Boolean = w.value

  assert(widened(23))
  assert(!boundedWidened(23))
  assert(widened(()))
}
