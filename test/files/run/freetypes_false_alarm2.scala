import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import internal._

object Test extends App {
  val tpe = typeOf[ru.Type]
  assert(!isFreeType(tpe.typeSymbol))
}
