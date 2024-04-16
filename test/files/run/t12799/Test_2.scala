
//> using options -Xlint

import example._

object Test extends App {
  val deprecatedMethod = classOf[B].getMethod("a")
  assert(deprecatedMethod.isAnnotationPresent(classOf[Deprecated]))
  println(new B().a)
}
//java.lang.annotation.AnnotationFormatError: Duplicate annotation for class: interface java.lang.Deprecated: @java.lang.Deprecated(forRemoval=false, since="")
