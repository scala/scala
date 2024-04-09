/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala

/** Class `Any` is the root of the Scala class hierarchy.  Every class in a Scala
 *  execution environment inherits directly or indirectly from this class.
 *
 * Starting with Scala 2.10 it is possible to directly extend `Any` using ''universal traits''.
 * A ''universal trait'' is a trait that extends `Any`, only has `def`s as members, and does no initialization.
 *
 * The main use case for universal traits is to allow basic inheritance of methods for [[scala.AnyVal value classes]].
 * For example,
 *
 * {{{
 *     trait Printable extends Any {
 *       def print(): Unit = println(this)
 *     }
 *     class Wrapper(val underlying: Int) extends AnyVal with Printable
 *
 *     val w = new Wrapper(3)
 *     w.print()
 * }}}
 *
 * See the [[https://docs.scala-lang.org/overviews/core/value-classes.html Value Classes and Universal Traits]] for more
 * details on the interplay of universal traits and value classes.
 */
abstract class Any {
  /** Compares the receiver object (`this`) with the argument object (`that`) for equivalence.
   *
   *  Any implementation of this method should be an [[https://en.wikipedia.org/wiki/Equivalence_relation equivalence relation]]:
   *
   *  - It is reflexive: for any instance `x` of type `Any`, `x.equals(x)` should return `true`.
   *  - It is symmetric: for any instances `x` and `y` of type `Any`, `x.equals(y)` should return `true` if and
   *    only if `y.equals(x)` returns `true`.
   *  - It is transitive: for any instances `x`, `y`, and `z` of type `Any` if `x.equals(y)` returns `true` and
   *    `y.equals(z)` returns `true`, then `x.equals(z)` should return `true`.
   *
   *  If you override this method, you should verify that your implementation remains an equivalence relation.
   *  Additionally, when overriding this method it is usually necessary to override `hashCode` to ensure that
   *  objects which are "equal" (`o1.equals(o2)` returns `true`) hash to the same [[scala.Int]].
   *  (`o1.hashCode.equals(o2.hashCode)`).
   *
   *  @param  that    the object to compare against this object for equality.
   *  @return         `true` if the receiver object is equivalent to the argument; `false` otherwise.
   */
  def equals(that: Any): Boolean

  /** Calculate a hash code value for the object.
   *
   *  The default hashing algorithm is platform dependent.
   *
   *  Note that it is allowed for two objects to have identical hash codes (`o1.hashCode.equals(o2.hashCode)`) yet
   *  not be equal (`o1.equals(o2)` returns `false`).  A degenerate implementation could always return `0`.
   *  However, it is required that if two objects are equal (`o1.equals(o2)` returns `true`) that they have
   *  identical hash codes (`o1.hashCode.equals(o2.hashCode)`).  Therefore, when overriding this method, be sure
   *  to verify that the behavior is consistent with the `equals` method.
   *
   *  @return   the hash code value for this object.
   */
  def hashCode(): Int

  /** Returns a string representation of the object.
   *
   *  The default representation is platform dependent.
   *
   *  @return a string representation of the object.
   */
  def toString(): String

  /** Returns the runtime class representation of the object.
   *
   *  @return a class object corresponding to the runtime type of the receiver.
   */
  final def getClass(): Class[_] = sys.error("getClass")

  /** Test two objects for equality.
   *  The expression `x == that` is equivalent to `if (x eq null) that eq null else x.equals(that)`.
   *
   *  @param  that  the object to compare against this object for equality.
   *  @return       `true` if the receiver object is equivalent to the argument; `false` otherwise.
   */
  final def ==(that: Any): Boolean = this equals that

  /** Test two objects for inequality.
   *
   *  @param  that  the object to compare against this object for equality.
   *  @return       `true` if !(this == that), false otherwise.
   */
  final def != (that: Any): Boolean = !(this == that)

  /** Equivalent to `x.hashCode` except for boxed numeric types and `null`.
   *  For numerics, it returns a hash value which is consistent
   *  with value equality: if two value type instances compare
   *  as true, then ## will produce the same hash value for each
   *  of them.
   *  For `null` returns a hashcode where `null.hashCode` throws a
   *  `NullPointerException`.
   *
   *  @return   a hash value consistent with ==
   */
  final def ## : Int = sys.error("##")

  /** Test whether the dynamic type of the receiver object has the same erasure as `T0`.
   *
   *  Depending on what `T0` is, the test is done in one of the below ways:
   *
   *  - `T0` is a non-parameterized class type, e.g. `BigDecimal`: this method returns `true` if
   *    the value of the receiver object is a `BigDecimal` or a subtype of `BigDecimal`.
   *  - `T0` is a parameterized class type, e.g. `List[Int]`: this method returns `true` if
   *    the value of the receiver object is some `List[X]` for any `X`.
   *    For example, `List(1, 2, 3).isInstanceOf[List[String]]` will return true.
   *  - `T0` is some singleton type `x.type` or literal `x`: this method returns `this.eq(x)`.
   *    For example, `x.isInstanceOf[1]` is equivalent to `x.eq(1)`
   *  - `T0` is an intersection `X with Y` or `X & Y: this method is equivalent to `x.isInstanceOf[X] && x.isInstanceOf[Y]`
   *  - `T0` is a union `X | Y`: this method is equivalent to `x.isInstanceOf[X] || x.isInstanceOf[Y]`
   *  - `T0` is a type parameter or an abstract type member: this method is equivalent
   *    to `isInstanceOf[U]` where `U` is `T0`'s upper bound, `Any` if `T0` is unbounded.
   *    For example, `x.isInstanceOf[A]` where `A` is an unbounded type parameter
   *    will return true for any value of `x`.
   *
   *  This is exactly equivalent to the type pattern `_: T0`
   *
   *  @note due to the unexpectedness of `List(1, 2, 3).isInstanceOf[List[String]]` returning true and
   *  `x.isInstanceOf[A]` where `A` is a type parameter or abstract member returning true,
   *  these forms issue a warning.
   *
   *  @return `true` if the receiver object is an instance of erasure of type `T0`; `false` otherwise.
   */
  final def isInstanceOf[T0]: Boolean = sys.error("isInstanceOf")

  /** Forces the compiler to treat the receiver object as having type `T0`,
   *  even though doing so may violate type safety.
   *
   *  This method is useful when you believe you have type information the compiler doesn't,
   *  and it also isn't possible to check the type at runtime.
   *  In such situations, skipping type safety is the only option.
   *
   *  It is platform dependent whether `asInstanceOf` has any effect at runtime.
   *  It might do a runtime type test on the erasure of `T0`,
   *  insert a conversion (such as boxing/unboxing), fill in a default value, or do nothing at all.
   *
   *  In particular, `asInstanceOf` is not a type test. It does **not** mean:
   *  {{{
   *   this match {
   *    case x: T0 => x
   *    case _     => throw ClassCastException("...")
   *  }}}
   *  Use pattern matching or [[isInstanceOf]] for type testing instead.
   *
   *  Situations where `asInstanceOf` is useful:
   *  - when flow analysis fails to deduce `T0` automatically
   *  - when down-casting a type parameter or an abstract type member (which cannot be checked at runtime due to type erasure)
   *  If there is any doubt and you are able to type test instead, you should do so.
   *
   *  Be careful of using `asInstanceOf` when `T0` is a primitive type.
   *  When `T0` is primitive, `asInstanceOf` may insert a conversion instead of a type test.
   *  If your intent is to convert, use a `toT` method (`x.toChar`, `x.toByte`, etc.).
   *
   *  @throws ClassCastException if the receiver is not an instance of the erasure of `T0`,
   *    if that can be checked on this platform
   *  @return the receiver object.
   */
  final def asInstanceOf[T0]: T0 = sys.error("asInstanceOf")
}
