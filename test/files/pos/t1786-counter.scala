trait ShapeLevel

object Fail {
  abstract class ProductNodeShape[Level <: ShapeLevel, C, M <: C, U <: C, P <: C] extends Shape[Level, M, U, P] {
    def copy(shapes: Seq[Shape[_, _, _, _]]): Shape[Level, _, _, _]
  }

  abstract class Shape[Level <: ShapeLevel, -Mixed_, Unpacked_, Packed_]

  final class TupleShape[Level <: ShapeLevel, M <: Product, U <: Product, P <: Product](val shapes: Shape[_, _, _, _]*) extends ProductNodeShape[Level, Product, M, U, P] {
    def copy(shapes: Seq[Shape[_, _, _, _]]): Shape[Level, _, _, _] = ???
  }

  trait ShapeLevel
}

object Ok {
  abstract class Shape[Level <: ShapeLevel, -Mixed_, Unpacked_, Packed_]

  abstract class ProductNodeShape[Level <: ShapeLevel, C, M <: C, U <: C, P <: C] extends Shape[Level, M, U, P] {
    def copy(shapes: Seq[Shape[_, _, _, _]]): Shape[Level, _, _, _]
  }

  final class TupleShape[Level <: ShapeLevel, M <: Product, U <: Product, P <: Product](val shapes: Shape[_, _, _, _]*) extends ProductNodeShape[Level, Product, M, U, P] {
    def copy(shapes: Seq[Shape[_, _, _, _]]): Shape[Level, _, _, _] = ???
  }
}

// This is why we reverted the fix for SI-1786 -- see SI-6169 for a potential alternative that could be extended to cover this.
// both objects type check on 2.10.3, but only Ok was accepted by 2.11 after the original fix to SI-1786.
// Fail results in:
/*
t1786-counter.scala:10: error: class TupleShape needs to be abstract, since method copy in class ProductNodeShape of type (shapes: Seq[Fail.Shape[_, _, _, _]])Fail.Shape[Level, _, _, _] is not defined
(Note that Seq[Fail.Shape[_, _, _, _]] does not match Seq[Fail.Shape[_ <: Fail.ShapeLevel, _, _, _]]: their type parameters differ)
  final class TupleShape[Level <: ShapeLevel, M <: Product, U <: Product, P <: Product](val shapes: Shape[_, _, _, _]*) extends ProductNodeShape[Level, Product, M, U, P] {
              ^
one error found
*/