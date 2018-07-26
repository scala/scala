package scala
package collection
package mutable


/** This class implements mutable sets using a hashtable.
 *  The iterator and all traversal methods of this class visit elements in the order they were inserted.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @author  Pavel Pavlov
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
class LinkedHashSet[A]
  extends AbstractSet[A]
    with SetOps[A, LinkedHashSet, LinkedHashSet[A]]
    with StrictOptimizedIterableOps[A, LinkedHashSet, LinkedHashSet[A]] {

  override def iterableFactory: IterableFactory[LinkedHashSet] = LinkedHashSet

  type Entry = LinkedHashSet.Entry[A]

  @transient protected var firstEntry: Entry = null
  @transient protected var lastEntry: Entry = null
  @transient private[this] var table: HashTable[A, AnyRef, Entry] = newHashTable

  // Used by scala-java8-compat (private[mutable] erases to public, so Java code can access it)
  private[mutable] def getTable: HashTable[A, AnyRef, Entry] = table

  private def newHashTable =
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

  override def size: Int = table.tableSize
  override def knownSize: Int = size
  override def isEmpty: Boolean = size == 0
  def contains(elem: A): Boolean = table.findEntry(elem) ne null

  def addOne(elem: A): this.type = {
    table.findOrAddEntry(elem, null)
    this
  }

  def subtractOne(elem: A): this.type = {
    remove(elem)
    this
  }

  override def remove(elem: A): Boolean = {
    val e = table.removeEntry(elem)
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
    private[this] var cur = firstEntry
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
    out.defaultWriteObject()
    table.serializeTo(out, { e => out.writeObject(e.key) })
  }

  private def readObject(in: java.io.ObjectInputStream): Unit = {
    in.defaultReadObject()
    table = newHashTable
    table.init(in, table.createNewEntry(in.readObject().asInstanceOf[A], null))
  }

  override protected[this] def stringPrefix = "LinkedHashSet"
}

/** $factoryInfo
 *  @define Coll `LinkedHashSet`
 *  @define coll linked hash set
 */
@SerialVersionUID(3L)
object LinkedHashSet extends IterableFactory[LinkedHashSet] {

  override def empty[A]: LinkedHashSet[A] = new LinkedHashSet[A]

  def from[E](it: collection.IterableOnce[E]) =
    it match {
      case lhs: LinkedHashSet[E] => lhs
      case _ => Growable.from(empty[E], it)
    }

  def newBuilder[A] = new GrowableBuilder(empty[A])

  /** Class for the linked hash set entry, used internally.
   *  @since 2.10
   */
  private[mutable] final class Entry[A](val key: A) extends HashEntry[A, Entry[A]] {
    var earlier: Entry[A] = null
    var later: Entry[A] = null
  }
}

