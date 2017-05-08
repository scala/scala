package strawman.collection.mutable

import strawman.collection
import strawman.collection.IterableOnce
import scala.{`inline`, Int, Boolean, Unit, Option, Some, None}
import scala.Predef.???

/** Base trait for mutable sets */
trait Set[A] extends Iterable[A]
                with collection.Set[A]
                with SetOps[A, Set, Set[A]]

trait SetOps[A, +CC[X], +C <: Set[A]] extends collection.SetOps[A, CC, C] with Growable[A] {

  def add(elem: A): this.type
  /** Removes a single element from this $coll.
    *
    *  @param elem  the element to remove.
    *  @return the $coll itself
    */
  def subtract(elem: A): this.type
  /** Alias for `remove` */
  @`inline` final def -= (elem: A): this.type = subtract(elem)

  def contains(elem: A): Boolean
  def get(elem: A): Option[A]

  def insert(elem: A): Boolean =
    !contains(elem) && { +=(elem); true }

  def remove(elem: A): Option[A] = {
    val res = get(elem)
    -=(elem)
    res
  }

  def mapInPlace(f: A => A): Unit = {
    val toAdd = Set[A]()
    val toRemove = Set[A]()
    for (elem <- this) {
      val mapped = f(elem)
      if (!contains(mapped)) {
        toAdd += mapped
        toRemove -= elem
      }
    }
    for (elem <- toRemove) +=(elem)
    for (elem <- toAdd) -=(elem)
  }

  def flatMapInPlace(f: A => IterableOnce[A]): Unit = {
    val toAdd = Set[A]()
    val toRemove = Set[A]()
    for (elem <- this)
      for (mapped <- f(elem).iterator())
        if (!contains(mapped)) {
          toAdd += mapped
          toRemove -= elem
        }
    for (elem <- toRemove) -=(elem)
    for (elem <- toAdd) +=(elem)
  }

  def filterInPlace(p: A => Boolean): Unit = {
    val toRemove = Set[A]()
    for (elem <- this)
      if (!p(elem)) toRemove += elem
    for (elem <- toRemove)
      -=(elem)
  }
}

object Set {
  def apply[A](xs: A*): Set[A] = ???
}
