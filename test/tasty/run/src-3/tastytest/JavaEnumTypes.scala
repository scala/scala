package tastytest

import java.lang.annotation.ElementType

object JavaEnumTypes {

  val method = ElementType.METHOD

  class TypeBox[T <: ElementType] {
    def id(t: T): t.type = t
  }

  class simulatedTarget(value: Array[ElementType]) extends scala.annotation.Annotation

  @simulatedTarget(Array(ElementType.LOCAL_VARIABLE))
  def foo = 23

}
