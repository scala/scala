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

package scala.collection

/** A template trait that contains just the `map`, `flatMap`, `foreach` and `withFilter` methods
  * of trait `Iterable`.
  *
  * @tparam A Element type (e.g. `Int`)
  * @tparam CC Collection type constructor (e.g. `List`)
  *
  * @define coll collection
  */
@SerialVersionUID(3L)
abstract class WithFilter[+A, +CC[_]] extends Serializable {

  /** Builds a new collection by applying a function to all elements of the
    * `filtered` outer $coll.
    *
    *  @param f      the function to apply to each element.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new $coll resulting from applying
    *                the given function `f` to each element of the filtered outer $coll
    *                and collecting the results.
    */
  def map[B](f: A => B): CC[B]

  /** Builds a new collection by applying a function to all elements of the
    * `filtered` outer $coll containing this `WithFilter` instance that satisfy
    *
    *  @param f      the function to apply to each element.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new $coll resulting from applying
    *                the given collection-valued function `f` to each element
    *                of the filtered outer $coll and
    *                concatenating the results.
    */
  def flatMap[B](f: A => IterableOnce[B]): CC[B]

  /** Applies a function `f` to all elements of the `filtered` outer $coll.
    *
    *  @param  f   the function that is applied for its side-effect to every element.
    *              The result of function `f` is discarded.
    *
    *  @tparam  U  the type parameter describing the result of function `f`.
    *              This result will always be ignored. Typically `U` is `Unit`,
    *              but this is not necessary.
    */
  def foreach[U](f: A => U): Unit

  /** Further refines the filter for this `filtered` $coll.
    *
    *  @param q   the predicate used to test elements.
    *  @return    an object of class `WithFilter`, which supports
    *             `map`, `flatMap`, `foreach`, and `withFilter` operations.
    *             All these operations apply to those elements of this $coll which
    *             also satisfy both `p` and `q` predicates.
    */
  def withFilter(q: A => Boolean): WithFilter[A, CC]

}
