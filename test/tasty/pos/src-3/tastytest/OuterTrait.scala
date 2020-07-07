package tastytest

trait OuterTrait {

  final class Inner

  final class innerAnnot(inner: Inner) extends scala.annotation.StaticAnnotation

}
