/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package api

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  According to the section 6.24 "Constant Expressions" of the Scala language specification,
 *  certain expressions (dubbed ''constant expressions'') can be evaluated by the Scala compiler at compile-time.
 *  Examples include "true", "0" and "classOf[List]".
 *
 *  `Constant` instances can be matched against and can be constructed directly, as if they were case classes:
 *  {{{
 *    assert(Constant(true).value == true)
 *    Constant(true) match {
 *      case Constant(s: String) =>  println("A string: " + s)
 *      case Constant(b: Boolean) => println("A boolean value: " + b)
 *      case Constant(x) =>          println("Something else: " + x)
 *    }
 *  }}}
 *
 *  `Constant` instances can wrap the following kinds of expressions:
 *    1. Literals of primitive value classes ([[scala.Byte `Byte`]], [[scala.Short `Short`]], [[scala.Int `Int`]], [[scala.Long `Long`]], [[scala.Float `Float`]], [[scala.Double `Double`]], [[scala.Char `Char`]], [[scala.Boolean `Boolean`]] and [[scala.Unit `Unit`]]) - represented directly as the corresponding type
 *    1. String literals - represented as instances of `String`.
 *    1. References to classes, typically constructed with [[scala.Predef#classOf]] - represented as [[scala.reflect.api.Types#Type types]].
 *    1. References to enumeration values - represented as [[scala.reflect.api.Symbols#Symbol symbols]].
 *
 *  Instances are used to represent literals in abstract syntax trees, inside [[scala.reflect.api.Trees#Literal]] nodes.
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
 *
 *  {{{
 *  // File "JavaSimpleEnumeration.java"
 *  enum JavaSimpleEnumeration { FOO, BAR }
 *
 *  // File "JavaSimpleAnnotation.java"
 *  import java.lang.annotation.*;
 *  @Retention(RetentionPolicy.RUNTIME)
 *  @Target({ElementType.TYPE})
 *  public @interface JavaSimpleAnnotation {
 *    Class<?> classRef();
 *    JavaSimpleEnumeration enumRef();
 *  }
 *
 *  // File "JavaAnnottee.java"
 *  @JavaSimpleAnnotation(
 *    classRef = JavaAnnottee.class,
 *    enumRef = JavaSimpleEnumeration.BAR
 *  )
 *  public class JavaAnnottee {}
 *  }}}
 *  {{{
 *  val javaArgs = typeOf[JavaAnnottee].typeSymbol.annotations(0).tree.children.tail
 *
 *  def jArg[A](lhs: String): Option[A] = javaArgs
 *    .map { case NamedArg(lhs, Literal(const)) => (lhs.toString, const) }
 *    .find(_._1 == lhs)
 *    .map(_._2.value.asInstanceOf[A])
 *
 *  // class reference, cast to Type
 *  val classRef = jArg[Type]("classRef").get
 *  println(showRaw(classRef))             // TypeRef(ThisType(<empty>), JavaAnnottee, List())
 *  println(cm.runtimeClass(classRef))     // class JavaAnnottee
 *  // enum value reference, cast to Symbol
 *  val enumRef = jArg[Symbol]("enumRef").get
 *  println(enumRef)                       // value BAR
 *
 *  val siblings = enumRef.owner.info.decls
 *  val enumValues = siblings.filter(_.isJavaEnum)
 *  println(enumValues)                    // Scope{
 *                                         //   final val FOO: JavaSimpleEnumeration;
 *                                         //   final val BAR: JavaSimpleEnumeration
 *                                         // }
 *
 *  // doesn't work because of https://github.com/scala/bug/issues/6459
 *  // val enumValue = mirror.reflectField(enumRef.asTerm).get
 *  val enumClass = cm.runtimeClass(enumRef.owner.asClass)
 *  val enumValue = enumClass.getDeclaredField(enumRef.name.toString).get(null)
 *  println(enumValue)                     // BAR
 *  }}}
 *
 *  @contentDiagram hideNodes "*Api"
 *  @group ReflectionAPI
 */
trait Constants {
  self: Universe =>

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
