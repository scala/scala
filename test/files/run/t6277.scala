import scala.reflect.runtime.universe._

object Test extends App {
  locally {
    val sym = typeOf[List[_]].typeSymbol.asClass
    val q = sym.isSealed
    println(q)
  }
}