import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.reflect.runtime.universe._

object Test extends App {
  class A[T]
  class B[T]
  implicit def foo[T: TypeTag](a: A[T])(implicit b: B[T]) = new { def baz = typeOf[T] }
  implicit def bar[T <: Int]: B[T] = new B[T]()
  println(new A[Int]().baz)
}