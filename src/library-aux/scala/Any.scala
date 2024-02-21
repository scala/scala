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

  /** Tell the compiler to treat the receiver object to be of type `T0`.
   *
   *  This operation should be considered unsafe and the outcome **undefined**. On a language level it merely 
   *  tells the compiler to forget any type information it has about the receiver object and treat it as if it
   *  had type `T0`. How the compiler manages to transition from the type of the receiver object to `T0` is 
   *  **undefined**.
   *
   *  Since the outcome is undefined, it is an implementation detail of the compiler backend whether this call 
   *  results in an operation in the target bytecode (e.g. Java bytecode) or not. 
   *  Some backends might emit a conversion, a runtime check or replace the receiver object with a default value.
   *  The decision to do so is mainly driven by the requirements of the target platform and its version.
   *  There is **absolutely no guarantee** that a certain implementation stays the same between different compiler 
   *  versions, especially regarding conversions, runtime checks and default values.
   *
   *  For instance, if the target is the JVM with version JDK8, then a conversion or a runtime check is added to
   *  the bytecode in most cases to comply with the java bytecode requirements. However, in some cases also a 
   *  *default value might be used instead which in turn could be ignored in the context of literal types.
   *  If a conversion or a runtime check in the form of a
   *  [checkcast](https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.checkcast) is added,
   *  then the outcome depends on the implementation of the JVM which should adhere to the rules as in the spec 
   *  defined in [JLS chapter 5](https://docs.oracle.com/javase/specs/jls/se8/html/jls-5.html). 
   *  This also means that widening and narrowing conversions take place for primitive types and that the scala 
   *  compiler has to pass erased types to `checkcast`. Furthermore, neither intersection nor union types nor literal
   *  types are supported in java bytecode. Whether the Scala compiler backend calculates a lowest upper bound in 
   *  such a case and passes it  to `checkcast` or `java.lang.Object` is used for simplicity reasons is again an 
   *  implementation detail.
   *
   *  It should be clear from that definition that relying on `asInstanceOf` to do type checking is a very bad idea.
   *  Use [isInstanceOf] instead for type testing and act as desired if the outcome is `false` (e.g. return a 
   *  [scala.util.Failure], throw an Exception etc.)
   *
   *  Following some examples targeting the JVM and JDK8 using Scala 3.4.0. We start off with statements where the
   *  outcome is most likely as you would expect and continue with more bizarre outcomes -- hoping that you really 
   *  refrain from using it as type testing utility, use [isInstanceOf] instead.
   *
   *  {{{
   *  // doesn't insert any operation 1 is statically known to be a subtype of Int
   *  1.asInstanceOf[Int]
   *  // inserts a narrowing primitive conversion from double to int
   *  1.0.asInstanceOf[Int]
   *  // inserts a widening primitive conversion from int to long (which could fail with an OutOfMemoryError)
   *  1.asInstanceOf[Long]
   *  // inserts a widening primitive conversion and boxing
   *  List[Int](1.0.asInstanceOf[Int])
   *  // inserts boxing and a checkcast to java.lang.Double, this fails as no conversion takes place
   *  1.asInstanceOf[java.lang.Double]
   *
   *  // doesn't insert any operation as B is statically know to be a subtype of A
   *  B().asInstanceOf[A]
   *
   *  // doesn't insert any operation, A is statically known to be a subtype of A
   *  (B(): A).asInstanceOf[A]
   *
   *  // inserts a checkcast to B
   *  (B(): A).asInstanceOf[B]
   *
   *  // inserts boxing but no checkcast because List[Int] and List[String] have both the type `List` after type erasure
   *  // and as we have seen before, if it is already known that a subtype relation holds, no checkcast is necessary
   *  (1 :: Nil).asInstanceOf[List[String]]
   *
   *  // inserts a checkcast for X which fails
   *  // as said, no guarantee that this stays this way.
   *  // also checkcast to java.lang.Object or no operation at all would be have been valid solutions
   *  "a".asInstanceOf[ Y | Z ]
   *
   *  // doesn't insert any operation, outputs "a"
   *  println("a".asInstanceOf[ Y | Int ])
   *
   *  // boxes and inserts a checkcast to java.lang.Object
   *  1.asInstanceOf [ Y | Float ]
   *
   *  // doesn't inserts any operation
   *  1.asInstanceOf[ String & Int ]
   *
   *  // inserts an unboxingToInt which uses a checkcast to java.lang.Integer internally which fails
   *  "a".asInstanceOf[ String & Int ]
   *
   *  // inserts an unboxingToInt where 0 is used as default value
   *  println(null.asInstanceOf[Int])
   *
   *  // inserts an unboxingToInt where 0 is used as default value, prints out 0
   *  println(null.asInstanceOf[2])
   *
   *  // inserts an unboxingToInt where 0 is used as default value
   *  val two: 2 = null.asInstanceOf[2]
   *  // value of two is ignored and the compiler computes a constant expression (2 + 1) instead
   *  // i.e. the result is still 3
   *  println(two + 1)
   *
   *  val twoFromJava: java.lang.Integer = null
   *  // doesn't insert any operation, outputs `null`
   *  println(twoFromJava.asInstanceOf[2])
   *
   *  // doesn't insert any operation
   *  val one = 2.asInstanceOf[1]
   *
   *  }}}
   *
   *  @throws ClassCastException if the receiver object is not an instance of the erasure of type `T0`.
   *  @throws OutOfMemoryError if the receiver object is widened and there is not enough memory available.
   *
   *  @return the receiver object.
   */
  final def asInstanceOf[T0]: T0 = sys.error("asInstanceOf")
}
