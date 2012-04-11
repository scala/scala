import scala.reflect.mirror._

object Test extends App {
  class C[T[_] >: Null] {
    val code = reify{val x: T[String] = null; println("ima worx"); x}
    println(freeTypes(code))
    val T = freeTypes(code)(0)
    mkToolBox().runExpr(code, Map(T -> definitions.ListClass.asType))
  }

  new C[List]
}