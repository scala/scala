package scala.reflect
package api

trait AnnotationInfos { self: Universe =>

  type AnnotationInfo <: AnyRef
  val AnnotationInfo: AnnotationInfoExtractor

  abstract class AnnotationInfoExtractor {
    def apply(atp: Type, args: List[Tree], assocs: List[(Name, ClassfileAnnotArg)]): AnnotationInfo
    def unapply(info: AnnotationInfo): Option[(Type, List[Tree], List[(Name, ClassfileAnnotArg)])]
  }

  type ClassfileAnnotArg <: AnyRef
  implicit def classfileAnnotArgTag: ArrayTag[ClassfileAnnotArg] // need a precise tag to pass to UnPickle's toArray call

  type LiteralAnnotArg <: ClassfileAnnotArg
  val LiteralAnnotArg: LiteralAnnotArgExtractor

  type ArrayAnnotArg <: ClassfileAnnotArg
  val ArrayAnnotArg: ArrayAnnotArgExtractor

  type NestedAnnotArg <: ClassfileAnnotArg
  val NestedAnnotArg: NestedAnnotArgExtractor

  abstract class LiteralAnnotArgExtractor {
    def apply(const: Constant): LiteralAnnotArg
    def unapply(arg: LiteralAnnotArg): Option[Constant]
  }

  abstract class ArrayAnnotArgExtractor {
    def apply(const: Array[ClassfileAnnotArg]): ArrayAnnotArg
    def unapply(arg: ArrayAnnotArg): Option[Array[ClassfileAnnotArg]]
  }

  abstract class NestedAnnotArgExtractor {
    def apply(anninfo: AnnotationInfo): NestedAnnotArg
    def unapply(arg: NestedAnnotArg): Option[AnnotationInfo]
  }
}


