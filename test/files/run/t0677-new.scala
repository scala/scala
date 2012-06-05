import scala.reflect.{ArrayTag, arrayTag}

object Test extends App {
  class X[T: ArrayTag] {
    val a = Array.ofDim[T](3, 4)
  }
  val x = new X[String]
  x.a(1)(2) = "hello"
  assert(x.a(1)(2) == "hello")
}