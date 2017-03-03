/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** Class `AnyRef` is the root class of all ''reference types''.
 *  All types except the value types descend from this class.
 *  @template
 */
trait AnyRef extends Any {

  /** The equality method for reference types.  Default implementation delegates to `eq`.
   *
   *  See also `equals` in [[scala.Any]].
   *
   *  @param  that    the object to compare against this object for equality.
   *  @return         `true` if the receiver object is equivalent to the argument; `false` otherwise.
   */
  def equals(that: Any): Boolean = this eq that

  /** The hashCode method for reference types.  See hashCode in [[scala.Any]].
   *
   *  @return   the hash code value for this object.
   */
  def hashCode: Int = sys.error("hashCode")

  /** Creates a String representation of this object.  The default
   *  representation is platform dependent.  On the java platform it
   *  is the concatenation of the class name, "@", and the object's
   *  hashcode in hexadecimal.
   *
   *  @return     a String representation of the object.
   */
  def toString: String = sys.error("toString")

  /** Executes the code in `body` with an exclusive lock on `this`.
   *
   *  @param    body    the code to execute
   *  @return           the result of `body`
   */
  def synchronized[T](body: => T): T

  /** Tests whether the argument (`that`) is a reference to the receiver object (`this`).
   *
   *  The `eq` method implements an [[http://en.wikipedia.org/wiki/Equivalence_relation equivalence relation]] on
   *  non-null instances of `AnyRef`, and has three additional properties:
   *
   *   - It is consistent: for any non-null instances `x` and `y` of type `AnyRef`, multiple invocations of
   *     `x.eq(y)` consistently returns `true` or consistently returns `false`.
   *   - For any non-null instance `x` of type `AnyRef`, `x.eq(null)` and `null.eq(x)` returns `false`.
   *   - `null.eq(null)` returns `true`.
   *
   *  When overriding the `equals` or `hashCode` methods, it is important to ensure that their behavior is
   *  consistent with reference equality.  Therefore, if two objects are references to each other (`o1 eq o2`), they
   *  should be equal to each other (`o1 == o2`) and they should hash to the same value (`o1.hashCode == o2.hashCode`).
   *
   *  @param  that    the object to compare against this object for reference equality.
   *  @return         `true` if the argument is a reference to the receiver object; `false` otherwise.
   */
  final def eq(that: AnyRef): Boolean = sys.error("eq")

  /** Equivalent to `!(this eq that)`.
   *
   *  @param  that    the object to compare against this object for reference equality.
   *  @return         `true` if the argument is not a reference to the receiver object; `false` otherwise.
   */
  final def ne(that: AnyRef): Boolean = !(this eq that)

  /** The expression `x == that` is equivalent to `if (x eq null) that eq null else x.equals(that)`.
   *
   *  @param    that  the object to compare against this object for equality.
   *  @return         `true` if the receiver object is equivalent to the argument; `false` otherwise.
   */
  final def ==(that: Any): Boolean =
    if (this eq null) that.asInstanceOf[AnyRef] eq null
    else this equals that

  /** Create a copy of the receiver object.
   *
   *  The default implementation of the `clone` method is platform dependent.
   *
   *  @note   not specified by SLS as a member of AnyRef
   *  @return a copy of the receiver object.
   */
  protected def clone(): AnyRef

  /** Called by the garbage collector on the receiver object when there
   *  are no more references to the object.
   *
   *  The details of when and if the `finalize` method is invoked, as
   *  well as the interaction between `finalize` and non-local returns
   *  and exceptions, are all platform dependent.
   *
   *  @note   not specified by SLS as a member of AnyRef
   */
  protected def finalize(): Unit

  /** Wakes up a single thread that is waiting on the receiver object's monitor.
   *
   *  @note   not specified by SLS as a member of AnyRef
   */
  final def notify(): Unit

  /** Wakes up all threads that are waiting on the receiver object's monitor.
   *
   *  @note   not specified by SLS as a member of AnyRef
   */
  final def notifyAll(): Unit

  /** Causes the current Thread to wait until another Thread invokes
   *  the notify() or notifyAll() methods.
   *
   *  @note   not specified by SLS as a member of AnyRef
   */
  final def wait (): Unit
  final def wait (timeout: Long, nanos: Int): Unit
  final def wait (timeout: Long): Unit
}
