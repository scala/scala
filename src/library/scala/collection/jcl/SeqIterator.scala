/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** An iterator for a sequence that can move both forwards and backwards.
 *  over a set of ordered keys.
 *
 *  @author Sean McDirmid
 */
trait SeqIterator[K,A] extends MutableIterator[A] {
  /** @returns The index at the iterator's cursor. */
  def nextIndex: K;

  /** @returns The index of the element before the iterator's cursor. */
  def previousIndex: K;

  /** @return The previous element, will move the iterator's cursor backwards. */
  def previous: A;

  /** @return True if and only if the iterator's cursor is not at the beging of the iteration. */
  def hasPrevious : Boolean;

  /** Winds the iteration forward until index "idx" is found */
  def seek(idx: K) = {
    while (nextIndex != idx) next;
    next;
  }
  /** finds the index of the next "a" in this iteration.
   *
   *  @param  a ..
   *  @return <code>None</code> if "a" is not found in the iteration.
   */
  def indexOf(a: A): Option[K] = {
    while (hasNext) {
      val ret = next;
      if (ret == a) return Some(previousIndex);
    }
    return None;
  }

  override def map[B](f: A => B) : SeqIterator[K,B] = new Map[B](f);
  class Map[B](f: A => B) extends super.Map[B](f) with SeqIterator[K,B] {
    override def hasPrevious = SeqIterator.this.hasPrevious;
    override def previous = f(SeqIterator.this.previous);
    override def previousIndex = SeqIterator.this.previousIndex;
    override def nextIndex = SeqIterator.this.nextIndex;
    override def map[C](g : B => C) : SeqIterator[K,C] =
      SeqIterator.this.map(a => g(f(a)));
  }
}
