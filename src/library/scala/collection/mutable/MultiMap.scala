/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


/** This class is typically used as a mixin. It turns maps which map <code>A</code>
 *  to <code>Set[B]</code> objects into multi maps which map <code>A</code> to
 *  <code>B</code> objects.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
trait MultiMap[A, B] extends Map[A, Set[B]] {
  protected def makeSet: Set[B] = new HashSet[B]

  def add(key: A, value: B): Unit = get(key) match {
    case None =>
      val set = makeSet
      set += value
      this(key) = set
    case Some(set) =>
      set += value
  }

  def remove(key: A, value: B) = get(key) match {
    case None =>
    case Some(set) => set -= value
  }

  def entryExists(key: A, p: B => Boolean): Boolean = get(key) match {
    case None => false
    case Some(set) => set exists p
  }
}
