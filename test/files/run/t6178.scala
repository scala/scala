import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  val plus = typeOf[java.lang.String].member(newTermName("+")).asMethod
  println(cm.reflect("").reflectMethod(plus).apply("2"))
}