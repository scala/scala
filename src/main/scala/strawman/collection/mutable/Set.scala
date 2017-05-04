package strawman.collection.mutable

import strawman.collection
import strawman.collection.{IterableFactory, IterableOnce}

import scala.{Boolean, Int, None, Option, Some, Unit, `inline`}

/** Base trait for mutable sets */
trait Set[A]
  extends GrowableIterable[A]
    with collection.Set[A]
    with SetOps[A, Set, Set[A]]
    with Shrinkable[A]

trait SetOps[A, +CC[X], +C <: Set[A]]
  extends IterableOps[A, CC, C]
    with collection.SetOps[A, CC, C] {

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

  def insert(elem: A): Boolean =
    !contains(elem) && {
      coll += elem; true
    }

  def remove(elem: A): Option[A] = {
    val res = get(elem)
    coll -= elem
    res
  }

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

  override def clone(): C = empty ++= coll

}

object Set extends IterableFactory.Delegate[Set](HashSet)
