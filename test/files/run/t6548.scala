import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

class Bean {
  @JavaAnnotationWithNestedEnum(JavaAnnotationWithNestedEnum.Value.VALUE)
  def value = 1
}

object Test extends App {
  println(cm.staticClass("Bean").isCaseClass)
  println(typeOf[Bean].declaration(newTermName("value")).annotations)
}
