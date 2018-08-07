package scala
package collection
package mutable


/** $factoryInfo
 *  @define Coll `LinkedHashMap`
 *  @define coll linked hash map
 */
@SerialVersionUID(3L)
object LinkedHashMap extends MapFactory[LinkedHashMap] {

  def empty[A, B] = new LinkedHashMap[A, B]

  def from[K, V](it: collection.IterableOnce[(K, V)]) =
    it match {
      case lhm: LinkedHashMap[K, V] => lhm
      case _ => Growable.from(empty[K, V], it)
    }

  def newBuilder[K, V] = new GrowableBuilder(empty[K, V])

  /** Class for the linked hash map entry, used internally.
    *  @since 2.8
    */
  private[mutable] final class LinkedEntry[K, V](val key: K, var value: V)
    extends HashEntry[K, LinkedEntry[K, V]] {
    var earlier: LinkedEntry[K, V] = null
    var later: LinkedEntry[K, V] = null
  }

}

/** This class implements mutable maps using a hashtable.
 *  The iterator and all traversal methods of this class visit elements in the order they were inserted.
 *
 *  @tparam K    the type of the keys contained in this hash map.
 *  @tparam V    the type of the values assigned to keys in this hash map.
 *
 *  @define Coll `LinkedHashMap`
 *  @define coll linked hash map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *  @define orderDependent
 *  @define orderDependentFold
 */
class LinkedHashMap[K, V]
  extends AbstractMap[K, V]
    with SeqMap[K, V]
    with MapOps[K, V, LinkedHashMap, LinkedHashMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, LinkedHashMap[K, V]]
    with StrictOptimizedMapOps[K, V, LinkedHashMap, LinkedHashMap[K, V]] {

  override def mapFactory: MapFactory[LinkedHashMap] = LinkedHashMap

  private[mutable] type Entry = LinkedHashMap.LinkedEntry[K, V]

  @transient protected var firstEntry: Entry = null
  @transient protected var lastEntry: Entry = null
  @transient private[this] var table: HashTable[K, V, Entry] = newHashTable

  // Used by scala-java8-compat (private[mutable] erases to public, so Java code can access it)
  private[mutable] def getTable: HashTable[K, V, Entry] = table

  private def newHashTable =
    new HashTable[K, V, Entry] {
      def createNewEntry(key: K, value: V): Entry = {
        val e = new Entry(key, value.asInstanceOf[V])
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

  override def empty = LinkedHashMap.empty[K, V]
  override def size = table.tableSize
  override def knownSize: Int = size
  override def isEmpty: Boolean = table.tableSize == 0
  def get(key: K): Option[V] = {
    val e = table.findEntry(key)
    if (e == null) None
    else Some(e.value)
  }

  override def put(key: K, value: V): Option[V] = {
    val e = table.findOrAddEntry(key, value)
    if (e eq null) None
    else { val v = e.value; e.value = value; Some(v) }
  }

  override def update(key: K, value: V): Unit = {
    val e = table.findOrAddEntry(key, value)
    if (e ne null) e.value = value
  }

  override def remove(key: K): Option[V] = {
    val e = table.removeEntry(key)
    if (e eq null) None
    else {
      if (e.earlier eq null) firstEntry = e.later
      else e.earlier.later = e.later
      if (e.later eq null) lastEntry = e.earlier
      else e.later.earlier = e.earlier
      e.earlier = null // Null references to prevent nepotism
      e.later = null
      Some(e.value)
    }
  }

  def addOne(kv: (K, V)): this.type = { put(kv._1, kv._2); this }

  def subtractOne(key: K): this.type = { remove(key); this }

  def iterator: Iterator[(K, V)] = new AbstractIterator[(K, V)] {
    private[this] var cur = firstEntry
    def hasNext = cur ne null
    def next() =
      if (hasNext) { val res = (cur.key, cur.value); cur = cur.later; res }
      else Iterator.empty.next()
  }

  protected class LinkedKeySet extends KeySet {
    override def iterableFactory: IterableFactory[collection.Set] = LinkedHashSet
  }

  override def keySet: collection.Set[K] = new LinkedKeySet

  override def keysIterator: Iterator[K] = new AbstractIterator[K] {
    private[this] var cur = firstEntry
    def hasNext = cur ne null
    def next() =
      if (hasNext) { val res = cur.key; cur = cur.later; res }
      else Iterator.empty.next()
  }

  override def valuesIterator: Iterator[V] = new AbstractIterator[V] {
    private[this] var cur = firstEntry
    def hasNext = cur ne null
    def next() =
      if (hasNext) { val res = cur.value; cur = cur.later; res }
      else Iterator.empty.next()
  }

  override def foreach[U](f: ((K, V)) => U): Unit = {
    var cur = firstEntry
    while (cur ne null) {
      f((cur.key, cur.value))
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
    table.serializeTo(out, { entry =>
      out.writeObject(entry.key)
      out.writeObject(entry.value)
    })
  }

  private def readObject(in: java.io.ObjectInputStream): Unit = {
    in.defaultReadObject()
    table = newHashTable
    table.init(in, table.createNewEntry(in.readObject().asInstanceOf[K], in.readObject().asInstanceOf[V]))
  }

  override protected[this] def stringPrefix = "LinkedHashMap"
}

