import scala.reflect.mirror._

object Test extends App {
  class C[T >: Null] {
    val code = reify{
      val tt = implicitly[TypeTag[T]]
      println("mah typetag is: %s".format(tt))
    }
    println(freeTypes(code))
    val T = freeTypes(code)(0)
    mkToolBox().runExpr(code, Map(T -> definitions.StringClass.asType))
  }

  new C[String]
}