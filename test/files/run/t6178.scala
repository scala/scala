import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  val plusMethod = typeOf[java.lang.String].member(TermName("$plus")).asMethod
  assert(cm.reflect("").reflectMethod(plusMethod).apply("2") == "2")
}
