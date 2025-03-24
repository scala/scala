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

// DO NOT EDIT, CHANGES WILL BE LOST
// This auto-generated code can be modified in "project/GenerateAnyVals.scala".
// Afterwards, running "sbt generateSources" regenerates this source file.

package scala

/** `Boolean` (equivalent to Java's `boolean` primitive type) is a
 *  subtype of [[scala.AnyVal]]. Instances of `Boolean` are not
 *  represented by an object in the underlying runtime system.
 *
 *  There is an implicit conversion from [[scala.Boolean]] => [[scala.runtime.RichBoolean]]
 *  which provides useful non-primitive operations.
 */
final abstract class Boolean private extends AnyVal {
  /** Negates a Boolean expression.
    *
    * - `!a` results in `false` if and only if `a` evaluates to `true` and
    * - `!a` results in `true` if and only if `a` evaluates to `false`.
    *
    * @return the negated expression
    */
  def unary_! : Boolean

  /** Compares two Boolean expressions and returns `true` if they evaluate to the same value.
    *
    * `a == b` returns `true` if and only if
    *  - `a` and `b` are `true` or
    *  - `a` and `b` are `false`.
    */
  def ==(x: Boolean): Boolean

  /**
    * Compares two Boolean expressions and returns `true` if they evaluate to a different value.
    *
    * `a != b` returns `true` if and only if
    *  - `a` is `true` and `b` is `false` or
    *  - `a` is `false` and `b` is `true`.
    */
  def !=(x: Boolean): Boolean

  /** Compares two Boolean expressions and returns `true` if one or both of them evaluate to true.
    *
    * `a || b` returns `true` if and only if
    *  - `a` is `true` or
    *  - `b` is `true` or
    *  - `a` and `b` are `true`.
    *
    * @note This method uses 'short-circuit' evaluation and
    *       behaves as if it was declared as `def ||(x: => Boolean): Boolean`.
    *       If `a` evaluates to `true`, `true` is returned without evaluating `b`.
    */
  def ||(x: Boolean): Boolean

  /** Compares two Boolean expressions and returns `true` if both of them evaluate to true.
    *
    * `a && b` returns `true` if and only if
    *  - `a` and `b` are `true`.
    *
    * @note This method uses 'short-circuit' evaluation and
    *       behaves as if it was declared as `def &&(x: => Boolean): Boolean`.
    *       If `a` evaluates to `false`, `false` is returned without evaluating `b`.
    */
  def &&(x: Boolean): Boolean

  // Compiler won't build with these seemingly more accurate signatures
  // def ||(x: => Boolean): Boolean
  // def &&(x: => Boolean): Boolean

  /** Compares two Boolean expressions and returns `true` if one or both of them evaluate to true.
    *
    * `a | b` returns `true` if and only if
    *  - `a` is `true` or
    *  - `b` is `true` or
    *  - `a` and `b` are `true`.
    *
    * @note This method evaluates both `a` and `b`, even if the result is already determined after evaluating `a`.
    */
  def |(x: Boolean): Boolean

  /** Compares two Boolean expressions and returns `true` if both of them evaluate to true.
    *
    * `a & b` returns `true` if and only if
    *  - `a` and `b` are `true`.
    *
    * @note This method evaluates both `a` and `b`, even if the result is already determined after evaluating `a`.
    */
  def &(x: Boolean): Boolean

  /** Compares two Boolean expressions and returns `true` if they evaluate to a different value.
    *
    * `a ^ b` returns `true` if and only if
    *  - `a` is `true` and `b` is `false` or
    *  - `a` is `false` and `b` is `true`.
    */
  def ^(x: Boolean): Boolean

  // Provide a more specific return type for Scaladoc
  override def getClass(): Class[Boolean] = ???
}

object Boolean extends AnyValCompanion {

  /** Transform a value type into a boxed reference type.
   *
   *  Runtime implementation determined by `scala.runtime.BoxesRunTime.boxToBoolean`. See [[https://github.com/scala/scala src/library/scala/runtime/BoxesRunTime.java]].
   *
   *  @param  x   the Boolean to be boxed
   *  @return     a java.lang.Boolean offering `x` as its underlying value.
   */
  def box(x: Boolean): java.lang.Boolean = ???

  /** Transform a boxed type into a value type.  Note that this
   *  method is not typesafe: it accepts any Object, but will throw
   *  an exception if the argument is not a java.lang.Boolean.
   *
   *  Runtime implementation determined by `scala.runtime.BoxesRunTime.unboxToBoolean`. See [[https://github.com/scala/scala src/library/scala/runtime/BoxesRunTime.java]].
   *
   *  @param  x   the java.lang.Boolean to be unboxed.
   *  @throws     ClassCastException  if the argument is not a java.lang.Boolean
   *  @return     the Boolean resulting from calling booleanValue() on `x`
   */
  def unbox(x: java.lang.Object): Boolean = ???

  /** The String representation of the scala.Boolean companion object. */
  override def toString = "object scala.Boolean"

}

