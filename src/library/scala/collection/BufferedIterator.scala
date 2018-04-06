package scala.collection


/** Buffered iterators are iterators which provide a method `head`
 *  that inspects the next element without discarding it.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait BufferedIterator[+A] extends Iterator[A] {

  /** Returns next element of iterator without advancing beyond it.
   */
  def head: A

  /** Returns an option of the next element of an iterator without advancing beyond it.
    * @return  the next element of this iterator if it has a next element
    *           `None` if it does not
    */
  def headOption : Option[A] = if (hasNext) Some(head) else None

  override def buffered: this.type = this
}
