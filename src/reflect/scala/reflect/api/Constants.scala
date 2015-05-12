/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package reflect
package api

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
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
 *    def jarg(name: String) = jann(TermName(name)).asInstanceOf[LiteralArgument].value
 *
 *    val classRef = jarg("classRef").typeValue
 *    println(showRaw(classRef))             // TypeRef(ThisType(<empty>), JavaAnnottee, List())
 *    println(cm.runtimeClass(classRef))     // class JavaAnnottee
 *
 *    val enumRef = jarg("enumRef").symbolValue
 *    println(enumRef)                       // value BAR
 *
 *    val siblings = enumRef.owner.info.decls
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
 *
 *  @contentDiagram hideNodes "*Api"
 *  @group ReflectionAPI
 */
trait Constants {
  self: Universe =>

  /**
   *  This "virtual" case class represents the reflection interface for literal expressions which can not be further
   *  broken down or evaluated, such as "true", "0", "classOf[List]". Such values become parts of the Scala abstract
   *  syntax tree representing the program. The constants
   *  correspond to section 6.24 "Constant Expressions" of the
   *  [[http://www.scala-lang.org/files/archive/spec/2.11/ Scala Language Specification]].
   *
   *  Such constants are used to represent literals in abstract syntax trees (the [[scala.reflect.api.Trees#Literal]] node)
   *  and literal arguments for Java class file annotations (the [[scala.reflect.api.Annotations#LiteralArgument]] class).
   *
   *  Constants can be matched against and can be constructed directly, as if they were case classes:
   *  {{{
   *    assert(Constant(true).value == true)
   *    Constant(true) match {
   *      case Constant(s: String) =>  println("A string: " + s)
   *      case Constant(b: Boolean) => println("A boolean value: " + b)
   *      case Constant(x) =>          println("Something else: " + x)
   *    }
   *  }}}
   *
   *  `Constant` instances can wrap certain kinds of these expressions:
   *    1. Literals of primitive value classes ([[scala.Byte `Byte`]], [[scala.Short `Short`]], [[scala.Int `Int`]], [[scala.Long `Long`]], [[scala.Float `Float`]], [[scala.Double `Double`]], [[scala.Char `Char`]], [[scala.Boolean `Boolean`]] and [[scala.Unit `Unit`]]) - represented directly as the corresponding type
   *    1. String literals - represented as instances of the `String`.
   *    1. References to classes, typically constructed with [[scala.Predef#classOf]] - represented as [[scala.reflect.api.Types#Type types]].
   *    1. References to enumeration values - represented as [[scala.reflect.api.Symbols#Symbol symbols]].
   *
   *  Class references are represented as instances of [[scala.reflect.api.Types#Type]]
   *  (because when the Scala compiler processes a class reference, the underlying runtime class might not yet have
   *  been compiled). To convert such a reference to a runtime class, one should use the [[scala.reflect.api.Mirrors#RuntimeMirror#runtimeClass `runtimeClass`]] method of a
   *  mirror such as [[scala.reflect.api.Mirrors#RuntimeMirror `RuntimeMirror`]] (the simplest way to get such a mirror is using
   *  [[scala.reflect.runtime#currentMirror `scala.reflect.runtime.currentMirror`]]).
   *
   *  Enumeration value references are represented as instances of [[scala.reflect.api.Symbols#Symbol]], which on JVM point to methods
   *  that return underlying enum values. To inspect an underlying enumeration or to get runtime value of a reference to an enum,
   *  one should use a [[scala.reflect.api.Mirrors#RuntimeMirror]] (the simplest way to get such a mirror is again [[scala.reflect.runtime.package#currentMirror]]).
   *
   *  Usage example:
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
   *    def jarg(name: String) = jann(TermName(name)) match {
   *      // Constant is always wrapped into a Literal or LiteralArgument tree node
   *      case LiteralArgument(ct: Constant) => value
   *      case _ => sys.error("Not a constant")
   *    }
   *
   *    val classRef = jarg("classRef").value.asInstanceOf[Type]
   *                                           // ideally one should match instead of casting
   *    println(showRaw(classRef))             // TypeRef(ThisType(<empty>), JavaAnnottee, List())
   *    println(cm.runtimeClass(classRef))     // class JavaAnnottee
   *
   *    val enumRef = jarg("enumRef").value.asInstanceOf[Symbol]
   *                                           // ideally one should match instead of casting
   *    println(enumRef)                       // value BAR
   *
   *    val siblings = enumRef.owner.info.decls
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
   *  @template
   *  @group Constants
   */
  type Constant >: Null <: AnyRef with ConstantApi

  /** The constructor/extractor for `Constant` instances.
   *  @group Extractors
   */
  val Constant: ConstantExtractor

  /** An extractor class to create and pattern match with syntax `Constant(value)`
   *  where `value` is the Scala value of the constant.
   *  @group Extractors
   */
  abstract class ConstantExtractor {
    /** A factory method that produces [[Constant `Constant`]] instances.
     *
     *  Notice that not any value can be passed to a constant: it must be either a primitive, a `String`, a
     *  [[scala.reflect.api.Types#Type type]] or a [[scala.reflect.api.Symbols#Symbol symbol]].
     *  See [[Constant the `Constant` class]] for more information.
     */
    def apply(value: Any): Constant
    /** An extractor that enables writing pattern matches against the [[Constant `Constant`]] class. */
    def unapply(arg: Constant): Option[Any]
  }

  /** The API of [[Constant]] instances.
   *  @group API
   */
  abstract class ConstantApi {
    /** Payload of the constant, that can be accessed directly or pattern matched against. */
    val value: Any

    /** Scala type that describes the constant. It is generated automatically based on the type of the value. */
    def tpe: Type
  }
}
