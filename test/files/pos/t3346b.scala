import scala.language.implicitConversions

trait T[X]
trait U[X]
trait TC[M[_]]

object Test extends App {
  def foo[M[_]: TC, A](ma: M[A]) = ()
  implicit val TCofT: TC[T] = new TC[T] {}
  implicit def any2T[A](a: A): T[A] = new T[A] {}
  implicit def any2U[A](a: A): U[A] = new U[A] {}

  val x = foo[T, Int](1)
  val y = foo(1)
}