/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.mutable

/** This class...
 *
 *  @author Sean McDirmid
 *  @version 1.0
 */
class LinkedHashSet[A](private val set0 : java.util.LinkedHashSet) extends Set[A] {

  def this() = this(new java.util.LinkedHashSet)

  private def this(set1 : java.util.Set, b : Boolean) =
    this(new java.util.LinkedHashSet(set1))

  def contains(elem: A): Boolean = set0.contains(elem)

  def +=(elem: A): Unit = set0.add(elem)

  def ++=(set: LinkedHashSet[A]) = set0.addAll(set.set0)
  def --=(set: LinkedHashSet[A]) = set0.removeAll(set.set0)

  def -=(elem: A): Unit = set0.remove(elem)

  def elements = new Iterator[A] {
    val i = set0.iterator
    def hasNext = i.hasNext()
    def next = i.next().asInstanceOf[A]
  }

  override def clear() = set0.clear()

  def size = set0.size()

  override def toString() = set0.toString()

  override def clone(): Set[A] = {
    val res = new LinkedHashSet[A](set0, true)
    res ++= this
    res
  }

}
