package strawman
package collection
package mutable

import scala.{AnyRef, Boolean, Int, None, Option, Serializable, SerialVersionUID, Some, transient, Unit}

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
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *  @define orderDependent
 *  @define orderDependentFold
 */
@SerialVersionUID(1L)
class LinkedHashSet[A]
  extends Set[A]
    with SetOps[A, LinkedHashSet, LinkedHashSet[A]]
    with Serializable {

  def iterableFactory: IterableFactory[LinkedHashSet] = LinkedHashSet

  type Entry = LinkedHashSet.Entry[A]

  @transient protected var firstEntry: Entry = null
  @transient protected var lastEntry: Entry = null

  private[this] var table: HashTable[A, AnyRef, Entry] =
    new HashTable[A, AnyRef, Entry] {
      def createNewEntry(key: A, value: AnyRef) = {
        val e = new Entry(key)
        if (firstEntry eq null) firstEntry = e
        else { lastEntry.later = e; e.earlier = lastEntry }
        lastEntry = e
        e
      }
      override def foreachEntry[U](f: Entry => U): Unit = {
        var cur = firstEntry
        while (cur ne null) {
          f(cur)
          cur = cur.later
        }
      }
    }

  def get(elem: A): Option[A] = {
    val entry = table.findEntry(elem)
    if (entry != null) Some(entry.key) else None
  }

  def empty: LinkedHashSet[A] = LinkedHashSet.empty

  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]) = fromIterable(coll)
  protected[this] def newSpecificBuilder() = iterableFactory.newBuilder()

  override def size: Int = table.tableSize

  def contains(elem: A): Boolean = table.findEntry(elem) ne null

  def addOne(elem: A): this.type = {
    table.findOrAddEntry(elem, null)
    this
  }

  def subtractOne(elem: A): this.type = {
    remove(elem)
    this
  }

  override def remove(elem: A): Option[A] = {
    val e = table.removeEntry(elem)
    if (e eq null) None
    else {
      if (e.earlier eq null) firstEntry = e.later
      else e.earlier.later = e.later
      if (e.later eq null) lastEntry = e.earlier
      else e.later.earlier = e.earlier
      e.earlier = null // Null references to prevent nepotism
      e.later = null
      Some(e.key)
    }
  }

  def iterator(): Iterator[A] = new Iterator[A] {
    private var cur = firstEntry
    def hasNext = cur ne null
    def next() =
      if (hasNext) { val res = cur.key; cur = cur.later; res }
      else Iterator.empty.next()
  }

  override def foreach[U](f: A => U): Unit = {
    var cur = firstEntry
    while (cur ne null) {
      f(cur.key)
      cur = cur.later
    }
  }

  override def clear(): Unit = {
    table.clearTable()
    firstEntry = null
    lastEntry = null
  }

  private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    table.serializeTo(out, { e => out.writeObject(e.key) })
  }

  private def readObject(in: java.io.ObjectInputStream): Unit = {
    firstEntry = null
    lastEntry = null
    table.init(in, table.createNewEntry(in.readObject().asInstanceOf[A], null))
  }

}

/** $factoryInfo
 *  @define Coll `LinkedHashSet`
 *  @define coll linked hash set
 */
object LinkedHashSet extends IterableFactory[LinkedHashSet] {

  override def empty[A]: LinkedHashSet[A] = new LinkedHashSet[A]

  def from[E](it: collection.IterableOnce[E]) =
    it match {
      case lhs: LinkedHashSet[E] => lhs
      case _ => Growable.from(empty[E], it)
    }

  def newBuilder[A]() = new GrowableBuilder(empty[A])

  /** Class for the linked hash set entry, used internally.
   *  @since 2.10
   */
  private[strawman] final class Entry[A](val key: A) extends HashEntry[A, Entry[A]] with Serializable {
    var earlier: Entry[A] = null
    var later: Entry[A] = null
  }

}

