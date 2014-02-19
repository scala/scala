import scala.reflect.runtime.universe._

object Test extends App {
  val tyn: TypeName = (??? : TypeSymbol).name
  val ten: TermName = (??? : TermSymbol).name
}