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
package collection
package mutable


/** `ReusableBuilder` is a marker trait that indicates that a `Builder`
  *  can be reused to build more than one instance of a collection.  In
  *  particular, calling `result()` followed by `clear()` will produce a
  *  collection and reset the builder to begin building a new collection
  *  of the same type.
  *
  *  In general no method other than `clear()` may be called after `result()`.
  *  It is up to subclasses to implement and to document other allowed sequences
  *  of operations (e.g. calling other methods after `result()` in order to obtain
  *  different snapshots of a collection under construction).
  *
  *  @tparam  Elem  the type of elements that get added to the builder.
  *  @tparam  To    the type of collection that it produced.
  *
  *  @define multipleResults
  *
  *     This Builder can be reused after calling `result()` without an
  *     intermediate call to `clear()` in order to build multiple related results.
  */
trait ReusableBuilder[-Elem, +To] extends Builder[Elem, To] {
  /** Clears the contents of this builder.
    *  After execution of this method, the builder will contain no elements.
    *
    *  If executed immediately after a call to `result()`, this allows a new
    *  instance of the same type of collection to be built.
    */
  override def clear(): Unit    // Note: overriding for Scaladoc only!

  /** Produces a collection from the added elements.
    *
    *  After a call to `result`, the behavior of all other methods is undefined
    *  save for `clear()`.  If `clear()` is called, then the builder is reset and
    *  may be used to build another instance.
    *
    *  @return a collection containing the elements added to this builder.
    */
  override def result(): To    // Note: overriding for Scaladoc only!
}
