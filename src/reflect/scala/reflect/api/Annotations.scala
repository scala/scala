package scala.reflect
package api

import scala.collection.immutable.ListMap

/** A slice of [[scala.reflect.api.Universe the Scala reflection cake]] that defines annotations and operations on them.
 *  See [[scala.reflect.api.Universe]] for a description of how the reflection API is encoded with the cake pattern.
 *
 *  Scala reflection supports:
 *    1. Annotations on definitions or types produced by the Scala compiler, i.e. subtypes of both
 *    [[scala.annotation.StaticAnnotation]] and [[scala.annotation.ClassfileAnnotation]] attached to program definitions or types
 *    (note: subclassing just [[scala.annotation.Annotation]] is not enough to have the corresponding
 *    metadata persisted for runtime reflection).
 *    1. Annotations on definitions produced by the Java compiler, i.e. subtypes of [[java.lang.annotation.Annotation]]
 *    attached to program definitions. When read by Scala reflection, the [[scala.annotation.ClassfileAnnotation]] trait
 *    is automatically added as a subclass to every Java annotation.
 *
 *  First of all [[scala.reflect.api.Annotations#Annotation]] provides `tpe`, which describes the type of the annotation.
 *  Depending on the superclasses of `tpe`, there are two flavors of annotations.
 *
 *  When annotations that subclass of [[scala.annotation.StaticAnnotation]] (dubbed ''Scala annotations'') are compiled by the Scala compiler,
 *  the information about them is ''pickled'', i.e. stored in special attributes in class files. To the contrast,
 *  annotations subclassing [[scala.annotation.ClassfileAnnotation]] (called ''Java annotations'') are written to class files as Java annotations.
 *  This distinction is manifested in the contract of [[scala.reflect.api.Annotations#Annotation]], which exposes
 *  both `scalaArgs` and `javaArgs`.
 *
 *  For Scala annotations, arguments are stored in `scalaArgs` and `javaArgs` is empty. Arguments in
 *  `scalaArgs` are represented as typed trees. Note that these trees are not transformed by any phases
 *  following the type-checker.
 *
 *  For Java annotations, `scalaArgs` is empty and arguments are stored in `javaArgs`.
 *  In this case, unlike in Java, Scala reflection only provides a key-value store of type [[scala.collection.immutable.ListMap]] from [[scala.reflect.api.Names#Name]] to
 *  [[scala.reflect.api.Annotations#JavaArgument]] that describes the annotations. Instances of `JavaArgument`
 *  represent different kinds of Java annotation arguments: literals (primitive and string constants), arrays and nested annotations.
 *  One shoud match against [[scala.reflect.api.Annotations#LiteralArgument]], [[scala.reflect.api.Annotations#ArrayArgument]] and [[scala.reflect.api.Annotations#NestedArgument]]
 *  to analyze them. We acknowledge that this process can be made more convenient and created [[https://issues.scala-lang.org/browse/SI-6423 an issue]] in the issue tracker
 *  to discuss possible improvements and track progress.
 *
 *  === Example ===
 *
 *  Entry points to the annotation API are [[scala.reflect.api.Symbols#Symbol.annotations]] (for definition annotations)
 *  and [[scala.reflect.api.Types#AnnotatedType]] (for type annotations).
 *
 *  To get annotations attached to a definition, first load the corresponding symbol (either explicitly using a [[scala.reflect.api.Mirror]]
 *  such as [[scala.reflect.runtime.package#currentMirror]]
 *  or implicitly using [[scala.reflect.api.TypeTags#typeOf]] and then either acquiring its `typeSymbol` or navigating its `members`).
 *  After the symbol is loaded, call its `annotations` method.
 *
 *  When inspecting a symbol for annotations, one should make sure that the inspected symbol is indeed the target of the annotation being looked for.
 *  Since single Scala definitions might produce multiple underlying definitions in bytecode, sometimes the notion of annotation's target is convoluted.
 *  For example, by default an annotation placed on a `val` will be attached to the private underlying field rather than to the getter
 *  (therefore to get such an annotation, one needs to do not `getter.annotations`, but `getter.asTerm.accessed.annotations`).
 *  This can get nasty with abstract vals, which don't have underlying fields and therefore ignore their annotations unless special measures are taken.
 *  See [[scala.annotation.meta.package]] for more information.
 *
 *  To get annotations attached to a type, simply pattern match that type against [[scala.reflect.api.Types#AnnotatedType]].

 *  {{{
 *  import scala.reflect.runtime.universe._
 *
 *  class S(x: Int, y: Int) extends scala.annotation.StaticAnnotation
 *  class J(x: Int, y: Int) extends scala.annotation.ClassfileAnnotation
 *
 *  object Test extends App {
 *    val x = 2
 *
 *    // Scala annotations are the most flexible with respect to
 *    // the richness of metadata they can store.
 *    // Arguments of such annotations are stored as abstract syntax trees,
 *    // so they can represent and persist arbitrary Scala expressions.
 *    @S(x, 2) class C
 *    val c = typeOf[C].typeSymbol
 *    println(c.annotations)                           // List(S(Test.this.x, 2))
 *    val tree = c.annotations(0).scalaArgs(0)
 *    println(showRaw(tree))                           // Select(..., newTermName("x"))
 *    println(tree.symbol.owner)                       // object Test
 *    println(showRaw(c.annotations(0).scalaArgs(1)))  // Literal(Constant(2))
 *
 *    // Java annotations are limited to predefined kinds of arguments:
 *    // literals (primitives and strings), arrays and nested annotations.
 *    @J(x = 2, y = 2) class D
 *    val d = typeOf[D].typeSymbol
 *    println(d.annotations)                           // List(J(x = 2, y = 2))
 *    println(d.annotations(0).javaArgs)               // Map(x -> 2, y -> 2)
 *  }
 *  }}}
 */
trait Annotations { self: Universe =>

  /** Information about an annotation. */
  type Annotation >: Null <: AnyRef with AnnotationApi

  /** A tag that preserves the identity of the `Annotation` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val AnnotationTag: ClassTag[Annotation]

   /** The constructor/deconstructor for `Annotation` instances. */
   val Annotation: AnnotationExtractor

  /** An extractor class to create and pattern match with syntax `Annotation(tpe, scalaArgs, javaArgs)`.
   *  Here, `tpe` is the annotation type, `scalaArgs` the payload of Scala annotations, and `javaArgs` the payload of Java annotations.
   */
  abstract class AnnotationExtractor {
    def apply(tpe: Type, scalaArgs: List[Tree], javaArgs: ListMap[Name, JavaArgument]): Annotation
    def unapply(ann: Annotation): Option[(Type, List[Tree], ListMap[Name, JavaArgument])]
  }

  /** The API of `Annotation` instances.
   *  The main source of information about annotations is the [[scala.reflect.api.Annotations]] page.
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

  /** A Java annotation argument */
  type JavaArgument >: Null <: AnyRef

  /** A tag that preserves the identity of the `JavaArgument` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
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

  /** The API of `LiteralArgument` instances.
   *  The main source of information about annotations is the [[scala.reflect.api.Annotations]] page.
   */
  trait LiteralArgumentApi {
    /** The underlying compile-time constant value. */
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

  /** API of `ArrayArgument` instances.
   *  The main source of information about annotations is the [[scala.reflect.api.Annotations]] page.
   */
  trait ArrayArgumentApi {
    /** The underlying array of Java annotation arguments. */
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

  /** API of `NestedArgument` instances.
   *  The main source of information about annotations is the [[scala.reflect.api.Annotations]] page.
   */
  trait NestedArgumentApi {
    /** The underlying nested annotation. */
    def annotation: Annotation
  }
}