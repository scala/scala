import scala.language.reflectiveCalls

object Test extends App {
  val foo = Macros.foo("T")
  val ttpe = scala.reflect.runtime.universe.weakTypeOf[foo.T]
  println(ttpe)
  println(ttpe.typeSymbol.info)

  val bar = Macros.bar("test")
  println(bar.test)

  val baz = Macros.baz("test")
  println(baz.test)
}