class Foo[T](val x: T) extends AnyVal

object Test extends App {
  println(scala.reflect.runtime.universe.typeOf[Foo[_]])
}