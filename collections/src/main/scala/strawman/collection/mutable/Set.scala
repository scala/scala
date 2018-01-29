package strawman.collection.mutable

import strawman.collection
import strawman.collection.{IterableFactory, IterableOnce}

import scala.{Boolean, Int, None, Option, Some, Unit, `inline`, deprecated}

/** Base trait for mutable sets */
trait Set[A]
  extends Iterable[A]
    with collection.Set[A]
    with SetOps[A, Set, Set[A]]

/**
  * @define coll mutable set
  * @define Coll `mutable.Set`
  */
trait SetOps[A, +CC[X], +C <: SetOps[A, CC, C]]
  extends IterableOps[A, CC, C]
    with collection.SetOps[A, CC, C]
    with Cloneable[C]
    with Growable[A]
    with Shrinkable[A] {

  def mapInPlace(f: A => A): this.type = {
    val toAdd = Set[A]()
    val toRemove = Set[A]()
    for (elem <- this) {
      val mapped = f(elem)
      if (!contains(mapped)) {
        toAdd += mapped
        toRemove -= elem
      }
    }
    for (elem <- toRemove) coll -= elem
    for (elem <- toAdd) coll += elem
    this
  }

  /**
    * @return The reference of the contained element that is equal to `elem`, if found, otherwise `None`
    * @param elem The element to get.
    */
  def get(elem: A): Option[A]

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

  def remove(elem: A): Option[A] = {
    val res = get(elem)
    coll -= elem
    res
  }

  def diff(that: collection.Set[A]): C =
    toIterable.foldLeft(empty)((result, elem) => if (that contains elem) result else result += elem)

  def flatMapInPlace(f: A => IterableOnce[A]): this.type = {
    val toAdd = Set[A]()
    val toRemove = Set[A]()
    for (elem <- this)
      for (mapped <- f(elem).iterator())
        if (!contains(mapped)) {
          toAdd += mapped
          toRemove -= elem
        }
    for (elem <- toRemove) coll -= elem
    for (elem <- toAdd) coll += elem
    this
  }

  def filterInPlace(p: A => Boolean): this.type = {
    val toRemove = Set[A]()
    for (elem <- this)
      if (!p(elem)) toRemove += elem
    for (elem <- toRemove)
      coll -= elem
    this
  }

  /** Retains only those elements for which the predicate
    *  `p` returns `true`.
    *
    * @param p  The test predicate
    */
  @deprecated("Use .filterInPlace instead of .retain", "2.13.0")
  @`inline` final def retain(p: A => Boolean): this.type = filterInPlace(p)

  override def clone(): C = empty ++= toIterable

}

/**
  * $factoryInfo
  * @define coll mutable set
  * @define Coll `mutable.Set`
  */
object Set extends IterableFactory.Delegate[Set](HashSet)

/** Explicit instantiation of the `Set` trait to reduce class file size in subclasses. */
abstract class AbstractSet[A] extends AbstractIterable[A] with Set[A]
