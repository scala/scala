package scala.reflect
package api

import scala.collection.immutable.ListMap

/**
 * Defines the type hierarchy for annotations.
 */
trait Annotations { self: Universe =>

  /** Typed information about an annotation. It can be attached to either a symbol or an annotated type.
   *
   *  Annotations are either ''Scala annotations'', which conform to [[scala.annotation.StaticAnnotation]]
   *  or ''Java annotations'', which conform to [[scala.annotation.ClassfileAnnotation]].
   *  Trait `ClassfileAnnotation` is automatically added to every Java annotation by the scalac classfile parser.
   */
  type Annotation >: Null <: AnyRef with AnnotationApi

  /** A tag that preserves the identity of the `Annotation` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val AnnotationTag: ClassTag[Annotation]

  /** The constructor/deconstructor for `Annotation` instances. */
   val Annotation: AnnotationExtractor

  /** An extractor class to create and pattern match with syntax `Annotation(atp, scalaArgs, javaArgs)`.
   *  Here, `atp` is the annotation type, `scalaArgs` the arguments, and `javaArgs` the annotation's key-value
   *  pairs.
   *
   *  Annotations are pickled, i.e. written to scala symtab attribute in the classfile.
   *  Annotations are written to the classfile as Java annotations if `atp` conforms to `ClassfileAnnotation`.
   *
   *  For Scala annotations, arguments are stored in `scalaArgs` and `javaArgs` is empty. Arguments in
   *  `scalaArgs` are represented as typed trees. Note that these trees are not transformed by any phases
   *  following the type-checker. For Java annotations, `scalaArgs` is empty and arguments are stored in
   *  `javaArgs`.
   */
  abstract class AnnotationExtractor {
    def apply(tpe: Type, scalaArgs: List[Tree], javaArgs: ListMap[Name, JavaArgument]): Annotation
    def unapply(ann: Annotation): Option[(Type, List[Tree], ListMap[Name, JavaArgument])]
  }

  trait AnnotationApi {
    def tpe: Type
    def scalaArgs: List[Tree]
    def javaArgs: ListMap[Name, JavaArgument]
  }

  /** A Java annotation argument */
  type JavaArgument >: Null <: AnyRef
  implicit val JavaArgumentTag: ClassTag[JavaArgument]

  /** A literal argument to a Java annotation as `"Use X instead"` in `@Deprecated("Use X instead")`*/
  type LiteralArgument >: Null <: AnyRef with JavaArgument with LiteralArgumentApi

  /** A tag that preserves the identity of the `LiteralArgument` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val LiteralArgumentTag: ClassTag[LiteralArgument]

  /** The constructor/deconstructor for `LiteralArgument` instances. */
  val LiteralArgument: LiteralArgumentExtractor

  /** An extractor class to create and pattern match with syntax `LiteralArgument(value)`
   *  where `value` is the constant argument.
   */
  abstract class LiteralArgumentExtractor {
    def apply(value: Constant): LiteralArgument
    def unapply(arg: LiteralArgument): Option[Constant]
  }

  trait LiteralArgumentApi {
    def value: Constant
  }

  /** An array argument to a Java annotation as in `@Target(value={TYPE,FIELD,METHOD,PARAMETER})`
   */
  type ArrayArgument >: Null <: AnyRef with JavaArgument with ArrayArgumentApi

  /** A tag that preserves the identity of the `ArrayArgument` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val ArrayArgumentTag: ClassTag[ArrayArgument]

  /** The constructor/deconstructor for `ArrayArgument` instances. */
  val ArrayArgument: ArrayArgumentExtractor

  /** An extractor class to create and pattern match with syntax `ArrayArgument(args)`
   *  where `args` is the argument array.
   */
  abstract class ArrayArgumentExtractor {
    def apply(args: Array[JavaArgument]): ArrayArgument
    def unapply(arg: ArrayArgument): Option[Array[JavaArgument]]
  }

  trait ArrayArgumentApi {
    def args: Array[JavaArgument]
  }

  /** A nested annotation argument to a Java annotation as `@Nested` in `@Outer(@Nested)`.
   */
  type NestedArgument >: Null <: AnyRef with JavaArgument with NestedArgumentApi

  /** A tag that preserves the identity of the `NestedArgument` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val NestedArgumentTag: ClassTag[NestedArgument]

  /** The constructor/deconstructor for `NestedArgument` instances. */
  val NestedArgument: NestedArgumentExtractor

  /** An extractor class to create and pattern match with syntax `NestedArgument(annotation)`
   *  where `annotation` is the nested annotation.
   */
  abstract class NestedArgumentExtractor {
    def apply(annotation: Annotation): NestedArgument
    def unapply(arg: NestedArgument): Option[Annotation]
  }

  trait NestedArgumentApi {
    def annotation: Annotation
  }
}