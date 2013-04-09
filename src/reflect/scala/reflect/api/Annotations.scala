package scala.reflect
package api

import scala.collection.immutable.ListMap

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 * This trait provides annotation support for the reflection API.
 *
 *  The API distinguishes between two kinds of annotations:
 *
 *  <ul>
 *  <li>''Java annotations'': annotations on definitions produced by the Java compiler, i.e., subtypes of [[java.lang.annotation.Annotation]]
 *  attached to program definitions. When read by Scala reflection, the [[scala.annotation.ClassfileAnnotation]] trait
 *  is automatically added as a subclass to every Java annotation.</li>
 *  <li>''Scala annotations'': annotations on definitions or types produced by the Scala compiler.</li>
 *  </ul>
 *
 *  When a Scala annotation that inherits from [[scala.annotation.StaticAnnotation]] or [[scala.annotation.ClassfileAnnotation]] is compiled,
 *  it is stored as special attributes in the corresponding classfile, and not as a Java annotation. Note that subclassing
 *  just [[scala.annotation.Annotation]] is not enough to have the corresponding metadata persisted for runtime reflection.
 *
 *  The distinction between Java and Scala annotations is manifested in the contract of [[scala.reflect.api.Annotations#Annotation]], which exposes
 *  both `scalaArgs` and `javaArgs`. For Scala or Java annotations extending [[scala.annotation.ClassfileAnnotation]] `scalaArgs` is empty
 *  and arguments are stored in `javaArgs`. For all other Scala annotations, arguments are stored in `scalaArgs` and `javaArgs` is empty.
 *
 *  Arguments in `scalaArgs` are represented as typed trees. Note that these trees are not transformed by any phases
 *  following the type-checker. Arguments in `javaArgs` are repesented as a map from [[scala.reflect.api.Names#Name]] to
 *  [[scala.reflect.api.Annotations#JavaArgument]]. Instances of `JavaArgument` represent different kinds of Java annotation arguments:
 *    - literals (primitive and string constants),
 *    - arrays and
 *    - nested annotations.
 *
 *  For more information about `Annotation`s, see the [[http://docs.scala-lang.org/overviews/reflection/annotations-names-scopes.html Reflection Guide: Annotations, Names, Scopes, and More]]
 *
 *  @contentDiagram hideNodes "*Api"
 *  @group ReflectionAPI
 */
trait Annotations { self: Universe =>

  /** Information about an annotation.
   *  @template
   *  @group Annotations
   */
  type Annotation >: Null <: AnyRef with AnnotationApi

   /** The constructor/extractor for `Annotation` instances.
    *  @group Extractors
    */
   val Annotation: AnnotationExtractor

  /** An extractor class to create and pattern match with syntax `Annotation(tpe, scalaArgs, javaArgs)`.
   *  Here, `tpe` is the annotation type, `scalaArgs` the payload of Scala annotations, and `javaArgs` the payload of Java annotations.
    *  @group Extractors
    */
  abstract class AnnotationExtractor {
    def apply(tpe: Type, scalaArgs: List[Tree], javaArgs: ListMap[Name, JavaArgument]): Annotation
    def unapply(ann: Annotation): Option[(Type, List[Tree], ListMap[Name, JavaArgument])]
  }

  /** The API of `Annotation` instances.
   *  The main source of information about annotations is the [[scala.reflect.api.Annotations]] page.
   *  @group API
   */
  trait AnnotationApi {
    /** The type of the annotation. */
    def tpe: Type

    /** Payload of the Scala annotation: a list of abstract syntax trees that represent the argument.
     *  Empty for Java annotations.
     */
    def scalaArgs: List[Tree]

    /** Payload of the Java annotation: a list of name-value pairs.
     *  Empty for Scala annotations.
     */
    def javaArgs: ListMap[Name, JavaArgument]
  }

  /** A Java annotation argument
   *  @template
   *  @group Annotations
   */
  type JavaArgument >: Null <: AnyRef


  /** A literal argument to a Java annotation as `"Use X instead"` in `@Deprecated("Use X instead")`
   *  @template
   *  @group Annotations
   */
  type LiteralArgument >: Null <: AnyRef with JavaArgument with LiteralArgumentApi

  /** The constructor/extractor for `LiteralArgument` instances.
   *  @group Extractors
   */
  val LiteralArgument: LiteralArgumentExtractor

  /** An extractor class to create and pattern match with syntax `LiteralArgument(value)`
   *  where `value` is the constant argument.
   *  @group Extractors
   */
  abstract class LiteralArgumentExtractor {
    def apply(value: Constant): LiteralArgument
    def unapply(arg: LiteralArgument): Option[Constant]
  }

  /** The API of `LiteralArgument` instances.
   *  The main source of information about annotations is the [[scala.reflect.api.Annotations]] page.
   *  @group API
   */
  trait LiteralArgumentApi {
    /** The underlying compile-time constant value. */
    def value: Constant
  }

  /** An array argument to a Java annotation as in `@Target(value={TYPE,FIELD,METHOD,PARAMETER})`
   *  @template
   *  @group Annotations
   */
  type ArrayArgument >: Null <: AnyRef with JavaArgument with ArrayArgumentApi

  /** The constructor/extractor for `ArrayArgument` instances.
   *  @group Extractors
   */
  val ArrayArgument: ArrayArgumentExtractor

  /** An extractor class to create and pattern match with syntax `ArrayArgument(args)`
   *  where `args` is the argument array.
   *  @group Extractors
   */
  abstract class ArrayArgumentExtractor {
    def apply(args: Array[JavaArgument]): ArrayArgument
    def unapply(arg: ArrayArgument): Option[Array[JavaArgument]]
  }

  /** API of `ArrayArgument` instances.
   *  The main source of information about annotations is the [[scala.reflect.api.Annotations]] page.
   *  @group API
   */
  trait ArrayArgumentApi {
    /** The underlying array of Java annotation arguments. */
    def args: Array[JavaArgument]
  }

  /** A nested annotation argument to a Java annotation as `@Nested` in `@Outer(@Nested)`.
   *  @template
   *  @group Annotations
   */
  type NestedArgument >: Null <: AnyRef with JavaArgument with NestedArgumentApi

  /** The constructor/extractor for `NestedArgument` instances.
   *  @group Extractors
   */
  val NestedArgument: NestedArgumentExtractor

  /** An extractor class to create and pattern match with syntax `NestedArgument(annotation)`
   *  where `annotation` is the nested annotation.
   *  @group Extractors
   */
  abstract class NestedArgumentExtractor {
    def apply(annotation: Annotation): NestedArgument
    def unapply(arg: NestedArgument): Option[Annotation]
  }

  /** API of `NestedArgument` instances.
   *  The main source of information about annotations is the [[scala.reflect.api.Annotations]] page.
   *  @group API
   */
  trait NestedArgumentApi {
    /** The underlying nested annotation. */
    def annotation: Annotation
  }
}
