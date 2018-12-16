import scala.annotation.Annotation

abstract class AbstractAnnotation() extends Annotation {}

object AbstractAnnotationFail {
  1: @AbstractAnnotation
}