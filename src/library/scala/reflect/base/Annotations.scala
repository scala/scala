package scala.reflect
package base

import scala.collection.immutable.ListMap

trait Annotations { self: Universe =>

  type Annotation >: Null <: AnyRef
  implicit val AnnotationTag: ClassTag[Annotation]
  val Annotation: AnnotationExtractor

  abstract class AnnotationExtractor {
    def apply(tpe: Type, scalaArgs: List[Tree], javaArgs: ListMap[Name, JavaArgument]): Annotation
    def unapply(ann: Annotation): Option[(Type, List[Tree], ListMap[Name, JavaArgument])]
  }

  type JavaArgument >: Null <: AnyRef
  implicit val JavaArgumentTag: ClassTag[JavaArgument]

  type LiteralArgument >: Null <: AnyRef with JavaArgument
  implicit val LiteralArgumentTag: ClassTag[LiteralArgument]
  val LiteralArgument: LiteralArgumentExtractor

  abstract class LiteralArgumentExtractor {
    def apply(value: Constant): LiteralArgument
    def unapply(arg: LiteralArgument): Option[Constant]
  }

  type ArrayArgument >: Null <: AnyRef with JavaArgument
  implicit val ArrayArgumentTag: ClassTag[ArrayArgument]
  val ArrayArgument: ArrayArgumentExtractor

  abstract class ArrayArgumentExtractor {
    def apply(args: Array[JavaArgument]): ArrayArgument
    def unapply(arg: ArrayArgument): Option[Array[JavaArgument]]
  }

  type NestedArgument >: Null <: AnyRef with JavaArgument
  implicit val NestedArgumentTag: ClassTag[NestedArgument]
  val NestedArgument: NestedArgumentExtractor

  abstract class NestedArgumentExtractor {
    def apply(annotation: Annotation): NestedArgument
    def unapply(arg: NestedArgument): Option[Annotation]
  }
}