import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {

  val s = "hello world"
  val m = cm.reflect(s)
  val sc = m.symbol
  val st = sc.asType
  val meth = (st member newTermName("indexOf")).asTermSymbol
  val IntType = definitions.IntClass.asType
  val indexOf = (meth resolveOverloaded(posVargs = List(IntType))).asMethodSymbol
  assert(m.reflectMethod(indexOf)('w') == 6)
  assert((m.reflectMethod(indexOf)('w') match { case x: Int => x }) == 6)

  val meth2 = (st member newTermName("substring")).asTermSymbol
  val substring = (meth2 resolveOverloaded(posVargs = List(IntType, IntType))).asMethodSymbol
  assert(m.reflectMethod(substring)(2, 6) == "llo ")
}
