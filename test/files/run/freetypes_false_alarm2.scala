import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.Eval

object Test extends App {
  val tpe = typeOf[ru.Type]
  println(tpe.typeSymbol.isFreeType)
}