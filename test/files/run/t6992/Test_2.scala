import scala.language.reflectiveCalls

object Test extends App {
  val foo = Macros.foo("T")
  println(scala.reflect.runtime.universe.weakTypeOf[foo.T])

  val bar = Macros.bar("test")
  println(bar.test)

  val baz = Macros.baz("test")
  println(baz.test)
}