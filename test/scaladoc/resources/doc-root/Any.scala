/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** Class `Any` is the root of the Scala class hierarchy.  Every class in a Scala
 *  execution environment inherits directly or indirectly from this class.
 */
abstract class Any {
  /** Compares the receiver object (`this`) with the argument object (`that`) for equivalence.
   *
   *  Any implementation of this method should be an [[http://en.wikipedia.org/wiki/Equivalence_relation equivalence relation]]:
   *
   *  - It is reflexive: for any instance `x` of type `Any`, `x.equals(x)` should return `true`.
   *  - It is symmetric: for any instances `x` and `y` of type `Any`, `x.equals(y)` should return `true` if and
   *    only if `y.equals(x)` returns `true`.
   *  - It is transitive: for any instances `x`, `y`, and `z` of type `AnyRef` if `x.equals(y)` returns `true` and
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
  def getClass(): Class[_]

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
  final def ##(): Int = sys.error("##")

  /** Test whether the dynamic type of the receiver object is `T0`.
   *
   *  Note that the result of the test is modulo Scala's erasure semantics.
   *  Therefore the expression `1.isInstanceOf[String]` will return `false`, while the
   *  expression `List(1).isInstanceOf[List[String]]` will return `true`.
   *  In the latter example, because the type argument is erased as part of compilation it is
   *  not possible to check whether the contents of the list are of the specified type.
   *
   *  @return `true` if the receiver object is an instance of erasure of type `T0`; `false` otherwise.
   */
  def isInstanceOf[T0]: Boolean = sys.error("isInstanceOf")

  /** Cast the receiver object to be of type `T0`.
   *
   *  Note that the success of a cast at runtime is modulo Scala's erasure semantics.
   *  Therefore the expression `1.asInstanceOf[String]` will throw a `ClassCastException` at
   *  runtime, while the expression `List(1).asInstanceOf[List[String]]` will not.
   *  In the latter example, because the type argument is erased as part of compilation it is
   *  not possible to check whether the contents of the list are of the requested type.
   *
   *  @throws ClassCastException if the receiver object is not an instance of the erasure of type `T0`.
   *  @return the receiver object.
   */
  def asInstanceOf[T0]: T0 = sys.error("asInstanceOf")
}
