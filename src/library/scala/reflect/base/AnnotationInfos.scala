package scala.reflect
package base

trait AnnotationInfos { self: Universe =>

  type AnnotationInfo >: Null <: AnyRef
  implicit val AnnotationInfoTag: ClassTag[AnnotationInfo]
  val AnnotationInfo: AnnotationInfoExtractor

  abstract class AnnotationInfoExtractor {
    def apply(atp: Type, args: List[Tree], assocs: List[(Name, ClassfileAnnotArg)]): AnnotationInfo
    def unapply(info: AnnotationInfo): Option[(Type, List[Tree], List[(Name, ClassfileAnnotArg)])]
  }

  type ClassfileAnnotArg >: Null <: AnyRef
  implicit val ClassfileAnnotArgTag: ClassTag[ClassfileAnnotArg]

  type LiteralAnnotArg >: Null <: AnyRef with ClassfileAnnotArg
  implicit val LiteralAnnotArgTag: ClassTag[LiteralAnnotArg]
  val LiteralAnnotArg: LiteralAnnotArgExtractor

  abstract class LiteralAnnotArgExtractor {
    def apply(const: Constant): LiteralAnnotArg
    def unapply(arg: LiteralAnnotArg): Option[Constant]
  }

  type ArrayAnnotArg >: Null <: AnyRef with ClassfileAnnotArg
  implicit val ArrayAnnotArgTag: ClassTag[ArrayAnnotArg]
  val ArrayAnnotArg: ArrayAnnotArgExtractor

  abstract class ArrayAnnotArgExtractor {
    def apply(args: Array[ClassfileAnnotArg]): ArrayAnnotArg
    def unapply(arg: ArrayAnnotArg): Option[Array[ClassfileAnnotArg]]
  }

  type NestedAnnotArg >: Null <: AnyRef with ClassfileAnnotArg
  implicit val NestedAnnotArgTag: ClassTag[NestedAnnotArg]
  val NestedAnnotArg: NestedAnnotArgExtractor

  abstract class NestedAnnotArgExtractor {
    def apply(annInfo: AnnotationInfo): NestedAnnotArg
    def unapply(arg: NestedAnnotArg): Option[AnnotationInfo]
  }
}