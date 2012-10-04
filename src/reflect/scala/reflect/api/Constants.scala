/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package api

/** A slice of [[scala.reflect.api.Universe the Scala reflection cake]] that defines compile-time constants and operations on them.
 *  See [[scala.reflect.api.Universe]] for a description of how the reflection API is encoded with the cake pattern.
 *
 *  According to the section 6.24 "Constant Expressions" of the Scala language specification,
 *  certain expressions (dubbed ''constant expressions'') can be evaluated by the Scala compiler at compile-time.
 *
 *  [[scala.reflect.api.Constants#Constant]] instances represent certain kinds of these expressions
 *  (with values stored in the `value` field and its strongly-typed views named `booleanValue`, `intValue` etc.), namely:
 *    1. Literals of primitive value classes (bytes, shorts, ints, longs, floats, doubles, chars, booleans and voids).
 *    1. String literals.
 *    1. References to classes (typically constructed with [[scala.Predef#classOf]]).
 *    1. References to enumeration values.
 *
 *  Such constants are used to represent literals in abstract syntax trees (the [[scala.reflect.api.Trees#Literal]] node)
 *  and literal arguments for Java class file annotations (the [[scala.reflect.api.Annotations#LiteralArgument]] class).
 *
 *  === Example ===
 *
 *  The `value` field deserves some explanation. Primitive and string values are represented as themselves, whereas
 *  references to classes and enums are a bit roundabout.
 *
 *  Class references are represented as instances of [[scala.reflect.api.Types#Type]]
 *  (because when the Scala compiler processes a class reference, the underlying runtime class might not yet have been compiled).
 *  To convert such a reference to a runtime class, one should use the `runtimeClass` method of a mirror such as [[scala.reflect.api.Mirrors#RuntimeMirror]]
 *  (the simplest way to get such a mirror is using [[scala.reflect.runtime.package#currentMirror]]).
 *
 *  Enumeration value references are represented as instances of [[scala.reflect.api.Symbols#Symbol]], which on JVM point to methods
 *  that return underlying enum values. To inspect an underlying enumeration or to get runtime value of a reference to an enum,
 *  one should use a [[scala.reflect.api.Mirrors#RuntimeMirror]] (the simplest way to get such a mirror is again [[scala.reflect.runtime.package#currentMirror]]).

 *  {{{
 *  enum JavaSimpleEnumeration { FOO, BAR }
 *
 *  import java.lang.annotation.*;
 *  @Retention(RetentionPolicy.RUNTIME)
 *  @Target({ElementType.TYPE})
 *  public @interface JavaSimpleAnnotation {
 *    Class<?> classRef();
 *    JavaSimpleEnumeration enumRef();
 *  }
 *
 *  @JavaSimpleAnnotation(
 *    classRef = JavaAnnottee.class,
 *    enumRef = JavaSimpleEnumeration.BAR
 *  )
 *  public class JavaAnnottee {}
 *  }}}
 *  {{{
 *  import scala.reflect.runtime.universe._
 *  import scala.reflect.runtime.{currentMirror => cm}
 *
 *  object Test extends App {
 *    val jann = typeOf[JavaAnnottee].typeSymbol.annotations(0).javaArgs
 *    def jarg(name: String) = jann(newTermName(name)).asInstanceOf[LiteralArgument].value
 *
 *    val classRef = jarg("classRef").typeValue
 *    println(showRaw(classRef))             // TypeRef(ThisType(<empty>), JavaAnnottee, List())
 *    println(cm.runtimeClass(classRef))     // class JavaAnnottee
 *
 *    val enumRef = jarg("enumRef").symbolValue
 *    println(enumRef)                       // value BAR
 *
 *    val siblings = enumRef.owner.typeSignature.declarations
 *    val enumValues = siblings.filter(sym => sym.isVal && sym.isPublic)
 *    println(enumValues)                    // Scope{
 *                                           //   final val FOO: JavaSimpleEnumeration;
 *                                           //   final val BAR: JavaSimpleEnumeration
 *                                           // }
 *
 *    // doesn't work because of https://issues.scala-lang.org/browse/SI-6459
 *    // val enumValue = mirror.reflectField(enumRef.asTerm).get
 *    val enumClass = cm.runtimeClass(enumRef.owner.asClass)
 *    val enumValue = enumClass.getDeclaredField(enumRef.name.toString).get(null)
 *    println(enumValue)                     // BAR
 *  }
 *  }}}
 */
trait Constants {
  self: Universe =>

  /** The type of compile-time constants.
   */
  type Constant >: Null <: AnyRef with ConstantApi

  /** A tag that preserves the identity of the `Constant` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val ConstantTag: ClassTag[Constant]

  /** The constructor/deconstructor for `Constant` instances. */
  val Constant: ConstantExtractor

  /** An extractor class to create and pattern match with syntax `Constant(value)`
   *  where `value` is the Scala value of the constant.
   */
  abstract class ConstantExtractor {
    def apply(value: Any): Constant
    def unapply(arg: Constant): Option[Any]
  }

  /** The API of `Constant` instances.
   *  The main source of information about constants is the [[scala.reflect.api.Constants]] page.
   */
  abstract class ConstantApi {
    /** Payload of the constant. */
    val value: Any

    /** Scala type that describes the constant. */
    def tpe: Type
  }
}
