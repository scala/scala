import scala.reflect.runtime.universe._

class C {
  implicit val v = new C
  implicit def d(x: C)(implicit c: C): Int = ???
  implicit class X(val x: Int)
}

object Test extends App {
  val decls = typeOf[C].typeSymbol.typeSignature.declarations.sorted.toList.filter(sym => !sym.isTerm || (sym.isMethod && !sym.asMethod.isConstructor))
  println(decls map (_.isImplicit))
  val param = decls.find(_.name.toString == "d").get.asMethod.paramss.last.head
  param.typeSignature
  println(param.isImplicit)
}