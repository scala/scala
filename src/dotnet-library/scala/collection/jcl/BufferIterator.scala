/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** An iterator for a buffer that supports element update and insertion.
 *
 *  @author Sean McDirmid
 */
trait BufferIterator[K,A] extends SeqIterator[K,A] {

  /** Sets the element before this iterator's cursor to "a."
   *  Replaces either the last element returned by "next" or,
   *  if previous was called,
   *  the next element that would be return by "previous."
   */
  def set(a: A): Unit;

  /** Inserts "a" after the iterator's cursor.
   *  If next was last called, "a" is inserted after the element returned.
   *  If previous was last called, "a" is inserted before the element returned.
   */
  def add(a: A): Unit;

}
