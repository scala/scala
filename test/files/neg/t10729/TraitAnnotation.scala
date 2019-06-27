import scala.annotation.Annotation

trait TraitAnnotation extends Annotation {}

object TraitAnnotationFail {
  1: @TraitAnnotation
}