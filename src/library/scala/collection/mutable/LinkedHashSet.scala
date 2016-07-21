/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package collection
package mutable

import generic._

/** This class implements mutable sets using a hashtable.
 *  The iterator and all traversal methods of this class visit elements in the order they were inserted.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @author  Pavel Pavlov
 *  @version 2.0, 31/12/2006
 *  @since   1
 *
 *  @tparam A     the type of the elements contained in this set.
 *
 *  @define Coll `LinkedHashSet`
 *  @define coll linked hash set
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is always `LinkedHashSet[B]` because an implicit of type `CanBuildFrom[LinkedHashSet, B, LinkedHashSet[B]]`
 *    is defined in object `LinkedHashSet`.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`. This is usually the `canBuildFrom` value
 *    defined in object `LinkedHashSet`.
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *  @define orderDependent
 *  @define orderDependentFold
 */
@SerialVersionUID(1L)
class LinkedHashSet[A] extends AbstractSet[A]
                          with Set[A]
                          with GenericSetTemplate[A, LinkedHashSet]
                          with SetLike[A, LinkedHashSet[A]]
                          with HashTable[A, LinkedHashSet.Entry[A]]
                          with Serializable
{
  override def companion: GenericCompanion[LinkedHashSet] = LinkedHashSet

  type Entry = LinkedHashSet.Entry[A]

  @transient protected var firstEntry: Entry = null
  @transient protected var lastEntry: Entry = null

  override def size: Int = tableSize

  def contains(elem: A): Boolean = findEntry(elem) ne null

  @deprecatedOverriding("+= should not be overridden so it stays consistent with add.", "2.11.0")
  def += (elem: A): this.type = { add(elem); this }

  @deprecatedOverriding("-= should not be overridden so it stays consistent with remove.", "2.11.0")
  def -= (elem: A): this.type = { remove(elem); this }

  override def add(elem: A): Boolean = findOrAddEntry(elem, null) eq null

  override def remove(elem: A): Boolean = {
    val e = removeEntry(elem)
    if (e eq null) false
    else {
      if (e.earlier eq null) firstEntry = e.later
      else e.earlier.later = e.later
      if (e.later eq null) lastEntry = e.earlier
      else e.later.earlier = e.earlier
      e.earlier = null // Null references to prevent nepotism
      e.later = null
      true
    }
  }

  def iterator: Iterator[A] = new AbstractIterator[A] {
    private var cur = firstEntry
    def hasNext = cur ne null
    def next =
      if (hasNext) { val res = cur.key; cur = cur.later; res }
      else Iterator.empty.next()
  }

  override def foreach[U](f: A => U) {
    var cur = firstEntry
    while (cur ne null) {
      f(cur.key)
      cur = cur.later
    }
  }

  protected override def foreachEntry[U](f: Entry => U) {
    var cur = firstEntry
    while (cur ne null) {
      f(cur)
      cur = cur.later
    }
  }

  protected def createNewEntry[B](key: A, dummy: B): Entry = {
    val e = new Entry(key)
    if (firstEntry eq null) firstEntry = e
    else { lastEntry.later = e; e.earlier = lastEntry }
    lastEntry = e
    e
  }

  override def clear() {
    clearTable()
    firstEntry = null
    lastEntry = null
  }

  private def writeObject(out: java.io.ObjectOutputStream) {
    serializeTo(out, { e => out.writeObject(e.key) })
  }

  private def readObject(in: java.io.ObjectInputStream) {
    firstEntry = null
    lastEntry = null
    init(in, createNewEntry(in.readObject().asInstanceOf[A], null))
  }
}

/** $factoryInfo
 *  @define Coll `LinkedHashSet`
 *  @define coll linked hash set
 */
object LinkedHashSet extends MutableSetFactory[LinkedHashSet] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LinkedHashSet[A]] = setCanBuildFrom[A]
  override def empty[A]: LinkedHashSet[A] = new LinkedHashSet[A]

  /** Class for the linked hash set entry, used internally.
   *  @since 2.10
   */
  private[scala] final class Entry[A](val key: A) extends HashEntry[A, Entry[A]] with Serializable {
    var earlier: Entry[A] = null
    var later: Entry[A] = null
  }
}

