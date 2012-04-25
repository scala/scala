import scala.reflect.mirror._

object Test extends App {
  class C[T >: Null] {
    val code = reify{val x: T = "2".asInstanceOf[T]; println("ima worx: %s".format(x)); x}
    println(freeTypes(code.tree))
    val T = freeTypes(code.tree)(0)
    mkToolBox().runExpr(code.tree, Map(T -> definitions.StringClass.asType))
  }

  new C[String]
}