/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $

package scalax.collection.generic

/** A non-strict projection of an iterable.
 * @author Sean McDirmid
 * @author Martin Odersky
 * @note this should really be a virtual class of SequenceFactory
 */
trait OrderedIterableView[+UC[/*+*/B] <: Iterable[B], /*+*/A] extends IterableView[UC, A] with OrderedIterable[A] { self =>

  val origin: OrderedIterable[_]

  override def newBuilder[A] = underlying.newBuilder[A].asInstanceOf[Builder[OrderedIterable, A]]

  /** Builds a new view object. This method needs to be overridden in subclasses
   *  which refine in IterableView type
   */
  protected override def newView[B](elems: Iterator[B]) = new OrderedIterableView[UC, B] {
    val origin = if (self.isDelay) self.origin else self
    val elements = elems
  }

  /** Non-strict variant of @see IterableLike.++ */
  override def ++[B >: A](that: Iterator[B]): OrderedIterableView[UC, B] = newView(elements ++ that)

  /** Non-strict variant of @see IterableLike.++ */
  override def ++[B >: A](that: Iterable[B]): OrderedIterableView[UC, B] = newView(elements ++ that.elements)

  /** Non-strict variant of @see IterableLike.map */
  override def map[B](f: A => B): OrderedIterableView[UC, B] = newView(elements map f)

  /** Non-strict variant of @see IterableLike.flatMap */
  override def flatMap[B](f: A => Iterable[B]): OrderedIterableView[UC, B] = newView(elements flatMap (f(_).elements))

  /** Non-strict variant of @see IterableLike.filter */
  override def filter(p: A => Boolean): OrderedIterableView[UC, A] = newView(elements filter p)

  /** Non-strict variant of @see IterableLike.partition */
  override def partition(p: A => Boolean): (OrderedIterableView[UC, A], OrderedIterableView[UC, A]) = {
    val (li, ri) = elements partition p
    (newView(li), newView(ri))
  }

  /** Non-strict variant of @see IterableLike.zip */
  override def zip[B](other: Iterable[B]): OrderedIterableView[UC, (A, B)] = newView(elements zip other.elements)

  /** Non-strict variant of @see IterableLike.zipWithIndex */
  override def zipWithIndex: OrderedIterableView[UC, (A, Int)] = newView(elements.zipWithIndex)

  /* Non-strict variant of @see IterableLike.zipAll
   *  This is not enabled because it can't be specialized in VectorView:
   *  VectorView is not covariant, yet must maintain updatability. Impossible to do this
   *  with this type signature.
  override def zipAll[B, A1 >: A, B1 >: B](that: Iterable[B], thisElem: A1, thatElem: B1): OrderedIterableView[UC, (A1, B1)] =
    newView(elements zipAll (that.elements, thisElem, thatElem))
   */

  /** Non-strict variant of @see Iterable.take */
  override def take(n: Int): OrderedIterableView[UC, A] = newView(elements take n)

  /** Non-strict variant of @see Iterable.drop */
  override def drop(n: Int): OrderedIterableView[UC, A] = newView(elements drop n)

  /** Non-strict variant of @see Iterable.splitAt */
  override def splitAt(n: Int): (OrderedIterableView[UC, A], OrderedIterableView[UC, A]) = (take(n), drop(n))

  /** Non-strict variant of @see Iterable.slice */
  override def slice(from: Int, until: Int): OrderedIterableView[UC, A] = newView(elements slice (from, until))

  /** Non-strict variant of @see Iterable.takeWhile */
  override def takeWhile(p: A => Boolean): OrderedIterableView[UC, A] = newView(elements takeWhile p)

  /** Non-strict variant of @see Iterable.dropWhile */
  override def dropWhile(p: A => Boolean): OrderedIterableView[UC, A] = newView(elements dropWhile p)

  /** Non-strict variant of @see Iterable.span */
  override def span(p: A => Boolean): (OrderedIterableView[UC, A], OrderedIterableView[UC, A]) = (takeWhile(p), dropWhile(p))
}
