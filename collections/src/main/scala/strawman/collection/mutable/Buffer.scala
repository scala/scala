package strawman.collection
package mutable

import scala.`inline`

trait Buffer[A] extends Seq[A] with Shrinkable[A] {
  //TODO Prepend is a logical choice for a readable name of `+=:` but it conflicts with the renaming of `append` to `add`
  /** Prepends a single element at the front of this $coll.
    *
    *  @param elem  the element to $add.
    *  @return the $coll itself
    */
  def prepend(elem: A): this.type

  /** Alias for `prepend` */
  @`inline` final def +=: (elem: A): this.type = prepend(elem)
}

/** Explicit instantiation of the `Buffer` trait to reduce class file size in subclasses. */
abstract class AbstractBuffer[A] extends AbstractSeq[A] with Buffer[A]
