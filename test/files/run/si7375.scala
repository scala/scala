import scala.reflect.ClassTag
object Test extends App {
  class Foo(val n: Int) extends AnyVal
  type F = Foo
  println(implicitly[ClassTag[Foo]])
  println(implicitly[ClassTag[F]])
}
