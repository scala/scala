import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}

final class table extends annotation.StaticAnnotation
@table class A

object Test extends App {
  val s = cm.classSymbol(classOf[A])
  println(s.annotations)
}