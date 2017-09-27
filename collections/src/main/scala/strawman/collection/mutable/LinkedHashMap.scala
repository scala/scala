package strawman
package collection
package mutable

import scala.{Unit, None, Option, Serializable, SerialVersionUID, Some, transient}

/** $factoryInfo
 *  @define Coll `LinkedHashMap`
 *  @define coll linked hash map
 */
object LinkedHashMap extends MapFactory[LinkedHashMap] {

  def empty[A, B] = new LinkedHashMap[A, B]

  def from[K, V](it: collection.IterableOnce[(K, V)]) =
    it match {
      case lhm: LinkedHashMap[K, V] => lhm
      case _ => Growable.from(empty[K, V], it)
    }

  def newBuilder[K, V]() = new GrowableBuilder(empty[K, V])

  /** Class for the linked hash map entry, used internally.
    *  @since 2.8
    */
  final class LinkedEntry[K, V](val key: K, var value: V)
    extends HashEntry[K, LinkedEntry[K, V]]
      with Serializable {
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
@SerialVersionUID(1L)
class LinkedHashMap[K, V]
  extends Map[K, V]
    with MapOps[K, V, LinkedHashMap, LinkedHashMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, LinkedHashMap[K, V]]
    with Serializable {

  type Entry = LinkedHashMap.LinkedEntry[K, V]

  private[this] val table: HashTable[K, V, Entry] =
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

  @transient protected var firstEntry: Entry = null
  @transient protected var lastEntry: Entry = null

  def mapFactory = LinkedHashMap

  protected[this] def fromSpecificIterable(coll: collection.Iterable[(K, V)]) = mapFromIterable(coll)
  protected[this] def mapFromIterable[K2, V2](it: collection.Iterable[(K2, V2)]) = mapFactory.from(it)

  protected[this] def newSpecificBuilder() = mapFactory.newBuilder()

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

  def add(kv: (K, V)): this.type = { put(kv._1, kv._2); this }

  def subtract(key: K): this.type = { remove(key); this }

  def iterator(): Iterator[(K, V)] = new Iterator[(K, V)] {
    private var cur = firstEntry
    def hasNext = cur ne null
    def next() =
      if (hasNext) { val res = (cur.key, cur.value); cur = cur.later; res }
      else Iterator.empty.next()
  }

  protected class LinkedKeySet extends KeySet {
    override def iterableFactory: IterableFactory[collection.Set] = LinkedHashSet
  }

  override def keySet: collection.Set[K] = new LinkedKeySet

  override def keysIterator(): Iterator[K] = new Iterator[K] {
    private var cur = firstEntry
    def hasNext = cur ne null
    def next() =
      if (hasNext) { val res = cur.key; cur = cur.later; res }
      else Iterator.empty.next()
  }

  override def valuesIterator(): Iterator[V] = new Iterator[V] {
    private var cur = firstEntry
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
    table.serializeTo(out, { entry =>
      out.writeObject(entry.key)
      out.writeObject(entry.value)
    })
  }

  private def readObject(in: java.io.ObjectInputStream): Unit = {
    firstEntry = null
    lastEntry = null
    table.init(in, table.createNewEntry(in.readObject().asInstanceOf[K], in.readObject().asInstanceOf[V]))
  }

}

