package scala
package collection
package mutable

import scala.collection.IterableOnce

/** This trait forms part of collections that can be augmented
  * using a `+=` operator and that can be cleared of all elements using
  * a `clear` method.
  *
  * @define coll growable collection
  * @define Coll `Growable`
  * @define add add
  * @define Add Add
  */
trait Growable[-A] extends Clearable {

  /** ${Add}s a single element to this $coll.
   *
   *  @param elem  the element to $add.
   *  @return the $coll itself
   */
  def += (elem: A): this.type

  /** Alias for `+=` */
  @`inline` final def addOne(elem: A): this.type = this += elem

  //TODO This causes a conflict in StringBuilder; looks like a compiler bug
  //@deprecated("Use addOne or += instead of append", "2.13.0")
  //@`inline` final def append(elem: A): Unit = addOne(elem)

  /** ${Add}s two or more elements to this $coll.
   *
   *  @param elem1 the first element to $add.
   *  @param elem2 the second element to $add.
   *  @param elems the remaining elements to $add.
   *  @return the $coll itself
   */
  @deprecated("Use ++= with a collection instead of += with varargs", "2.13.0")
  @`inline` final def += (elem1: A, elem2: A, elems: A*): this.type = this += elem1 += elem2 ++= (elems: IterableOnce[A])

  /** ${Add}s all elements produced by an IterableOnce to this $coll.
   *
   *  @param xs   the IterableOnce producing the elements to $add.
   *  @return  the $coll itself.
   */
  def ++= (xs: IterableOnce[A]): this.type = {
    val it = xs.iterator
    while (it.hasNext) {
      addOne(it.next())
    }
    this
  }

  /** Alias for `++=` */
  @`inline` final def addAll(xs: IterableOnce[A]): this.type = this ++= xs
}

object Growable {

  /**
    * Fills a `Growable` instance with the elements of a given iterable
    * @param empty Instance to fill
    * @param it Elements to add
    * @tparam A Element type
    * @return The filled instance
    */
  def from[A](empty: Growable[A], it: collection.IterableOnce[A]): empty.type = empty ++= it

}

/** This trait forms part of collections that can be cleared
  *  with a clear() call.
  *
  *  @define coll collection
  */
trait Clearable {
  /** Clears the $coll's contents. After this operation, the
    *  $coll is empty.
    */
  def clear(): Unit
}
