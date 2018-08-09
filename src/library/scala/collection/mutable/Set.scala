package scala.collection.mutable

import scala.collection.IterableFactory
import scala.language.higherKinds

/** Base trait for mutable sets */
trait Set[A]
  extends Iterable[A]
    with collection.Set[A]
    with SetOps[A, Set, Set[A]] {

  override def iterableFactory: IterableFactory[IterableCC] = Set
}

/**
  * @define coll mutable set
  * @define Coll `mutable.Set`
  */
trait SetOps[A, +CC[X], +C <: SetOps[A, CC, C]]
  extends collection.SetOps[A, CC, C]
    with Cloneable[C]
    with Builder[A, C]
    with Growable[A]
    with Shrinkable[A] {

  def result(): C = coll

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

  def remove(elem: A): Boolean = {
    val res = contains(elem)
    coll -= elem
    res
  }

  def diff(that: collection.Set[A]): C =
    toIterable.foldLeft(empty)((result, elem) => if (that contains elem) result else result += elem)

  @deprecated("Use filterInPlace instead", "2.13.0")
  @inline final def retain(p: A => Boolean): Unit = filterInPlace(p)

  /** Removes all elements from the set for which do not satisfy a predicate.
    *  @param  p  the predicate used to test elements. Only elements for
    *             which `p` returns `true` are retained in the set; all others
    *             are removed.
    */
  def filterInPlace(p: A => Boolean): this.type = {
    for (elem <- this.toList) // scala/bug#7269 toList avoids ConcurrentModificationException
      if (!p(elem)) this -= elem
    this
  }

  override def clone(): C = empty ++= toIterable

}

/**
  * $factoryInfo
  * @define coll mutable set
  * @define Coll `mutable.Set`
  */
@SerialVersionUID(3L)
object Set extends IterableFactory.Delegate[Set](HashSet)


/** Explicit instantiation of the `Set` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractSet[A] extends scala.collection.AbstractSet[A] with Set[A]
