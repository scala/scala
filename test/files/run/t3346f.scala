import scala.language.implicitConversions
import scala.language.reflectiveCalls

object Test extends App {
  trait Foo[A]
  implicit def fooString: Foo[String] = null
  implicit def value[A](implicit foo: Foo[A]) = 5

  println(implicitly[Int])

  implicit def conversion[A](x: Int)(implicit foo: Foo[A]) = new {
    def aMethod = 5
  }
  println(1.aMethod)
}
