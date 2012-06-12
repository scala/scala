package scala.reflect
package api

trait AnnotationInfos extends base.AnnotationInfos { self: Universe =>

  override type AnnotationInfo >: Null <: AnyRef with AnnotationInfoApi
  trait AnnotationInfoApi {
    def atp: Type
    def args: List[Tree]
    def assocs: List[(Name, ClassfileAnnotArg)]
  }

  override type LiteralAnnotArg >: Null <: ClassfileAnnotArg with LiteralAnnotArgApi
  trait LiteralAnnotArgApi {
    def const: Constant
  }

  override type ArrayAnnotArg >: Null <: ClassfileAnnotArg with ArrayAnnotArgApi
  trait ArrayAnnotArgApi {
    def args: Array[ClassfileAnnotArg]
  }

  override type NestedAnnotArg >: Null <: ClassfileAnnotArg with NestedAnnotArgApi
  trait NestedAnnotArgApi {
    def annInfo: AnnotationInfo
  }
}