package strawman.collection.mutable

import strawman.collection.IterableOnce
import scala.{inline, Unit}
import scala.annotation.tailrec
import strawman.collection.{toOldSeq, toNewSeq}

/** This trait forms part of collections that can be augmented
 *  using a `+=` operator and that can be cleared of all elements using
 *  a `clear` method.
 */
trait Growable[-A] {

  /** ${Add}s a single element to this $coll.
   *
   *  @param elem  the element to $add.
   *  @return the $coll itself
   */
  def addInPlace(elem: A): this.type

  /** Alias for `addInPlace` */
  @inline final def += (elem: A): this.type = addInPlace(elem)

  /** ${Add}s two or more elements to this $coll.
   *
   *  @param elem1 the first element to $add.
   *  @param elem2 the second element to $add.
   *  @param elems the remaining elements to $add.
   *  @return the $coll itself
   */
  @inline final def +=(elem1: A, elem2: A, elems: A*): this.type = this += elem1 += elem2 ++= (elems.toStrawman: IterableOnce[A])

  /** ${Add}s all elements produced by a TraversableOnce to this $coll.
   *
   *  @param xs   the TraversableOnce producing the elements to $add.
   *  @return  the $coll itself.
   */
  def addAllInPlace(xs: IterableOnce[A]): this.type = {
    @tailrec def loop(xs: scala.collection.LinearSeq[A]): Unit = {
      if (xs.nonEmpty) {
        this += xs.head
        loop(xs.tail)
      }
    }
    xs match {
      case xs: scala.collection.LinearSeq[_] => loop(xs.asInstanceOf[scala.collection.LinearSeq[A]])
      case xs => xs.iterator() foreach += // Deviation: IterableOnce does not define `foreach`.
    }
    // @ichoran writes: Right now, this actually isn't any faster than using an iterator
    // for List. Maybe we should just simplify the code by deferring to iterator foreach?
    this
  }

  /** Alias for `addAllInPlace` */
  @inline final def ++= (xs: IterableOnce[A]): this.type = addAllInPlace(xs)

  /** Clears the $coll's contents. After this operation, the
   *  $coll is empty.
   */
  def clear(): Unit
}
