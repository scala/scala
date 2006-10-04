/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

/** This class is used internally. It implements the mutable <code>Map</code>
 *  class in terms of three functions: <code>findEntry</code>,
 *  <code>addEntry</code>, and <code>entries</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
trait DefaultMapModel[A, B] extends AnyRef with Map[A, B] {

  protected type Entry = DefaultEntry[A,B]

  protected def findEntry(key: A): Option[Entry]

  protected def addEntry(e: Entry): Unit

  protected def entries: Iterator[Entry]

  def get(key: A) = findEntry(key) match {
    case None => None
    case Some(e) => Some(e.value);
  }

  def update(key: A, value: B) = findEntry(key) match {
    case None => addEntry(new Entry(key, value));
    case Some(e) => e.value = value;
  }

  def elements = new Iterator[Pair[A, B]] {
    val iter = entries
    def hasNext = iter.hasNext
    def next = iter.next.toPair
  }
}

[serializable]
protected class DefaultEntry[A,B](k: A, v: B) extends AnyRef {
  def key = k
  var value = v
  def toPair = Pair(k, value)
  override def toString() = k.toString() + " -> " + value
}
