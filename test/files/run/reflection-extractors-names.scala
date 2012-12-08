import scala.reflect.runtime.universe._

object Test extends App {

  TermName("name") match { case TermName(name) => assert(name == "name") }

  TypeName("name") match { case TypeName(name) => assert(name == "name") }
}