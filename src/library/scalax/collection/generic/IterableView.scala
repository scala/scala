/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2008, LAMP/EPFL             **
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
trait IterableView[+UC[/*+*/B] <: Iterable[B], /*+*/A] extends Iterable[A] { self =>

  val origin: Iterable[_]
  def elements: Iterator[A]

  val underlying: Iterable[_] = origin match {
    case v: IterableView[t, _] => v.underlying
    case _ => origin
  }

  private def isDelay = elements eq underlying.elements

  private[this] var forced: UC[A] = _
  private[this] var wasForced = false

  def force: UC[A] = {
    if (!wasForced) {
      forced = {
        val b = underlying.newBuilder[A]
        for (x <- elements)
          b += x
        b.result
      }.asInstanceOf[UC[A]]
      wasForced = true
    }
    forced
  }

  def newBuilder[A] = underlying.newBuilder[A]

  /** Builds a new view object. This method needs to be overridden in subclasses
   *  which refine in IterableView type
   */
  protected def newView[B](elems: Iterator[B]) = new IterableView[UC, B] {
    val origin = if (self.isDelay) self.origin else self
    val elements = elems
  }

  /** Non-strict variant of @see IterableLike.++ */
  override def ++[B >: A](that: Iterator[B]): IterableView[UC, B] = newView(elements ++ that)

  /** Non-strict variant of @see IterableLike.++ */
  override def ++[B >: A](that: Iterable[B]): IterableView[UC, B] = newView(elements ++ that.elements)

  /** Non-strict variant of @see IterableLike.map */
  override def map[B](f: A => B): IterableView[UC, B] = newView(elements map f)

  /** Non-strict variant of @see IterableLike.flatMap */
  override def flatMap[B](f: A => Iterable[B]): IterableView[UC, B] = newView(elements flatMap (f(_).elements))

  /** Non-strict variant of @see IterableLike.filter */
  override def filter(p: A => Boolean): IterableView[UC, A] = newView(elements filter p)

  /** Non-strict variant of @see IterableLike.partition */
  override def partition(p: A => Boolean): (IterableView[UC, A], IterableView[UC, A]) = {
    val (li, ri) = elements partition p
    (newView(li), newView(ri))
  }

  /** Non-strict variant of @see IterableLike.zip */
  override def zip[B](other: Iterable[B]): IterableView[UC, (A, B)] = newView(elements zip other.elements)

  /** Non-strict variant of @see IterableLike.zipWithIndex */
  override def zipWithIndex: IterableView[UC, (A, Int)] = newView(elements.zipWithIndex)

  /* Non-strict variant of @see IterableLike.zipAll
   *  This is not enabled because it can't be specialized in VectorView:
   *  VectorView is not covariant, yet must maintain updatability. Impossible to do this
   *  with this type signature.
  override def zipAll[B, A1 >: A, B1 >: B](that: Iterable[B], thisElem: A1, thatElem: B1): IterableView[UC, (A1, B1)] =
    newView(elements zipAll (that.elements, thisElem, thatElem))
   */

  /** Non-strict variant of @see Iterable.take */
  override def take(n: Int): IterableView[UC, A] = newView(elements take n)

  /** Non-strict variant of @see Iterable.drop */
  override def drop(n: Int): IterableView[UC, A] = newView(elements drop n)

  /** Non-strict variant of @see Iterable.splitAt */
  override def splitAt(n: Int): (IterableView[UC, A], IterableView[UC, A]) = (take(n), drop(n))

  /** Non-strict variant of @see Iterable.slice */
  override def slice(from: Int, until: Int): IterableView[UC, A] = newView(elements slice (from, until))

  /** Non-strict variant of @see Iterable.takeWhile */
  override def takeWhile(p: A => Boolean): IterableView[UC, A] = newView(elements takeWhile p)

  /** Non-strict variant of @see Iterable.dropWhile */
  override def dropWhile(p: A => Boolean): IterableView[UC, A] = newView(elements dropWhile p)

  /** Non-strict variant of @see Iterable.span */
  override def span(p: A => Boolean): (IterableView[UC, A], IterableView[UC, A]) = (takeWhile(p), dropWhile(p))

  /** The projection resulting from the concatenation of this projection with the <code>rest</code> projection.
   *  @param rest   The projection that gets appended to this projection
   *  @deprecated   Use ++ instead
   */
  @deprecated def append[B >: A](rest : => Iterable[B]): IterableView[UC, B] = this ++ rest.elements

  override def stringPrefix = origin.stringPrefix+"V"


}
