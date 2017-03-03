/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2016, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package collection
package mutable

/** `ReusableBuilder` is a marker trait that indicates that a `Builder`
 *  can be reused to build more than one instance of a collection.  In
 *  particular, calling `result` followed by `clear` will produce a
 *  collection and reset the builder to begin building a new collection
 *  of the same type.
 *
 *  It is up to subclasses to implement this behavior, and to document any
 *  other behavior that varies from standard `ReusableBuilder` usage
 *  (e.g. operations being well-defined after a call to `result`, or allowing
 *  multiple calls to result to obtain different snapshots of a collection under
 *  construction).
 *
 *  @tparam  Elem  the type of elements that get added to the builder.
 *  @tparam  To    the type of collection that it produced.
 *
 *  @since 2.12
 */
trait ReusableBuilder[-Elem, +To] extends Builder[Elem, To] {
  /** Clears the contents of this builder.
   *  After execution of this method, the builder will contain no elements.
   *
   *  If executed immediately after a call to `result`, this allows a new
   *  instance of the same type of collection to be built.
   */
  override def clear(): Unit    // Note: overriding for Scaladoc only!

  /** Produces a collection from the added elements.
   *
   *  After a call to `result`, the behavior of all other methods is undefined
   *  save for `clear`.  If `clear` is called, then the builder is reset and
   *  may be used to build another instance.
   *
   *  @return a collection containing the elements added to this builder.
   */
  override def result(): To    // Note: overriding for Scaladoc only!
}
