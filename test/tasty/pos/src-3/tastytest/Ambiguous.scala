package tastytest

class Ambiguous[T] {
  class annot(elem: Box[T]) extends scala.annotation.StaticAnnotation {
    def this(elem: T) = this(new Box[T](elem))
  }
}

object Ambiguous {
  class AmbiguousBox[T] extends Ambiguous[Box[T]] {
    class annotBox(elem: T) extends annot(new Box(elem))
  }
}
