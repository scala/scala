/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

/** This class implements mutable sets using a hashtable.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
[serializable]
class HashSet[A] extends Set[A] with HashTable[A] {

  def contains(elem: A): Boolean = findEntry(elem) != null

  def +=(elem: A) { if (findEntry(elem) == null) addEntry(new SetEntry(elem)) }

  def -=(elem: A) { removeEntry(elem) }

  def elements = entries map (.key)

  def clear {initTable(); tableSize = 0 }

  protected type Entry = SetEntry[A]

  override def clone(): Set[A] = new HashSet[A] ++ this
}

[serializable]
final class SetEntry[A](val key: A) extends HashEntry[A, SetEntry[A]]
