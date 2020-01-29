package tastytest

import scala.annotation.StaticAnnotation

object ObjOverload {

  def foo[A <: 42](a: A) = 42
  object foo { def apply(i: 43) = i }

}
