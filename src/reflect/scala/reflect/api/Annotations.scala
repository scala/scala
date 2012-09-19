package scala.reflect
package api

import scala.collection.immutable.ListMap

trait Annotations extends base.Annotations { self: Universe =>

  override type Annotation >: Null <: AnyRef with AnnotationApi
  trait AnnotationApi {
    def tpe: Type
    def scalaArgs: List[Tree]
    def javaArgs: ListMap[Name, JavaArgument]
  }

  override type LiteralArgument >: Null <: JavaArgument with LiteralArgumentApi
  trait LiteralArgumentApi {
    def value: Constant
  }

  override type ArrayArgument >: Null <: JavaArgument with ArrayArgumentApi
  trait ArrayArgumentApi {
    def args: Array[JavaArgument]
  }

  override type NestedArgument >: Null <: JavaArgument with NestedArgumentApi
  trait NestedArgumentApi {
    def annotation: Annotation
  }
}