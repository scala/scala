import scala.reflect.mirror._

object Test extends App {
  class C[T >: Null] {
    val code = reify{val x: T = "2".asInstanceOf[T]; println("ima worx: %s".format(x)); x}
    println(freeTypes(code))
    val T = freeTypes(code)(0)
    mkToolBox().runExpr(code, Map(T -> definitions.StringClass.asType))
  }

  new C[String]
}