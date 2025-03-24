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

package scala.collection.mutable

import scala.collection.{IterableFactory, IterableFactoryDefaults, IterableOps}

/** Base trait for mutable sets */
trait Set[A]
  extends Iterable[A]
    with collection.Set[A]
    with SetOps[A, Set, Set[A]]
    with IterableFactoryDefaults[A, Set] {

  override def iterableFactory: IterableFactory[Set] = Set
}

/**
  * @define coll mutable set
  * @define Coll `mutable.Set`
  */
trait SetOps[A, +CC[X], +C <: SetOps[A, CC, C]]
  extends collection.SetOps[A, CC, C]
    with IterableOps[A, CC, C] // only needed so we can use super[IterableOps] below
    with Cloneable[C]
    with Builder[A, C]
    with Growable[A]
    with Shrinkable[A] {

  def result(): C = coll

  /** Check whether the set contains the given element, and add it if not.
   *
   *  @param elem  the element to be added
   *  @return true if the element was added
   */
  def add(elem: A): Boolean =
    !contains(elem) && {
      coll += elem; true
    }

  /** Updates the presence of a single element in this set.
    *
    * This method allows one to add or remove an element `elem`
    *  from this set depending on the value of parameter `included`.
    *  Typically, one would use the following syntax:
    *  {{{
    *     set(elem) = true  // adds element
    *     set(elem) = false // removes element
    *  }}}
    *
    *  @param elem     the element to be added or removed
    *  @param included a flag indicating whether element should be included or excluded.
    */
  def update(elem: A, included: Boolean): Unit = {
    if (included) add(elem)
    else remove(elem)
  }

  /** Removes an element from this set.
   *
   *  @param elem     the element to be removed
   *  @return true if this set contained the element before it was removed
   */
  def remove(elem: A): Boolean = {
    val res = contains(elem)
    coll -= elem
    res
  }

  def diff(that: collection.Set[A]): C =
    foldLeft(empty)((result, elem) => if (that contains elem) result else result += elem)

  @deprecated("Use filterInPlace instead", "2.13.0")
  @inline final def retain(p: A => Boolean): Unit = filterInPlace(p)

  /** Removes all elements from the set for which do not satisfy a predicate.
    *  @param  p  the predicate used to test elements. Only elements for
    *             which `p` returns `true` are retained in the set; all others
    *             are removed.
    */
  def filterInPlace(p: A => Boolean): this.type = {
    if (nonEmpty) {
      val array = this.toArray[Any] // scala/bug#7269 toArray avoids ConcurrentModificationException
      val arrayLength = array.length
      var i = 0
      while (i < arrayLength) {
        val elem = array(i).asInstanceOf[A]
        if (!p(elem)) {
          this -= elem
        }
        i += 1
      }
    }
    this
  }

  override def clone(): C = empty ++= this

  override def knownSize: Int = super[IterableOps].knownSize
}

/**
  * $factoryInfo
  * @define coll mutable set
  * @define Coll `mutable.Set`
  */
@SerialVersionUID(3L)
object Set extends IterableFactory.Delegate[Set](HashSet)


/** Explicit instantiation of the `Set` trait to reduce class file size in subclasses. */
abstract class AbstractSet[A] extends scala.collection.AbstractSet[A] with Set[A]
