package scala
package collection.mutable

import scala.collection.{Iterator, MapFactory, StrictOptimizedIterableOps, StrictOptimizedMapOps}

/** This class implements mutable maps using a hashtable.
  *
  *  @since 1
  *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#hash-tables "Scala's Collection Library overview"]]
  *  section on `Hash Tables` for more information.
  *
  *  @tparam K    the type of the keys contained in this hash map.
  *  @tparam V    the type of the values assigned to keys in this hash map.
  *
  *  @define Coll `mutable.HashMap`
  *  @define coll mutable hash map
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
class HashMap[K, V]
  extends AbstractMap[K, V]
    with MapOps[K, V, HashMap, HashMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, HashMap[K, V]]
    with StrictOptimizedMapOps[K, V, HashMap, HashMap[K, V]] {

  override def mapFactory: MapFactory[HashMap] = HashMap

  private[mutable] type Entry = DefaultEntry[K, V]

  @transient private[this] var table: HashTable[K, V, Entry] = newHashTable

  // Used by scala-java8-compat (private[mutable] erases to public, so Java code can access it)
  private[mutable] def getTable: HashTable[K, V, Entry] = table

  private def newHashTable: HashTable[K, V, Entry] =
    new HashTable[K, V, Entry] {
      def createNewEntry(key: K, value: V): Entry = new Entry(key, value)
    }

  override def isEmpty: Boolean = table.size == 0
  override def knownSize: Int = table.size

  def iterator: Iterator[(K, V)] = {
    if (isEmpty) Iterator.empty
    else table.entriesIterator.map(e => (e.key, e.value))
  }

  override def keysIterator: Iterator[K] = {
    if (isEmpty) Iterator.empty
    else table.entriesIterator.map(_.key)
  }
  override def valuesIterator: Iterator[V] = {
    if (isEmpty) Iterator.empty
    else table.entriesIterator.map(_.value)
  }

  def get(key: K): Option[V] = {
    val e = table.findEntry(key)
    if (e eq null) None else Some(e.value)
  }

  def addOne(kv: (K, V)): this.type = {
    val e = table.findOrAddEntry(kv._1, kv._2)
    if (e ne null) e.value = kv._2
    this
  }

  override def clear(): Unit = table.clearTable()

  def subtractOne(key: K): this.type = { table.removeEntry(key); this }

  override def size: Int = table.size

  override def contains(key: K): Boolean = table.findEntry(key) != null

  @throws[NoSuchElementException]
  override def apply(key: K): V = {
    val e = table.findEntry(key)
    if (e eq null) default(key)
    else e.value
  }

  override def foreach[U](f: ((K, V)) => U): Unit = table.foreachEntry(e => f((e.key, e.value)))

  override def put(key: K, value: V): Option[V] = {
    val e = table.findOrAddEntry(key, value)
    if (e eq null) None
    else { val v = e.value; e.value = value; Some(v) }
  }

  override def update(key: K, value: V): Unit = {
    val e = table.findOrAddEntry(key, value)
    if (e ne null) e.value = value
  }

  override def getOrElseUpdate(key: K, defaultValue: => V): V = {
    if (getClass != classOf[HashMap[_, _]]) {
      // subclasses of HashMap might customise `get` ...
      super.getOrElseUpdate(key, defaultValue)
    } else {
      val hash = table.elemHashCode(key)
      val i = table.index(hash)
      val firstEntry = table.findEntry0(key, i)
      if (firstEntry != null) firstEntry.value
      else {
        val table0 = table.table
        val default = defaultValue
        // Avoid recomputing index if the `defaultValue()` hasn't triggered
        // a table resize.
        val newEntryIndex = if (table0 eq table.table) i else table.index(hash)
        val e = table.createNewEntry(key, default)
        // Repeat search
        // because evaluation of `default` can bring entry with `key`
        val secondEntry = table.findEntry0(key, newEntryIndex)
        if (secondEntry == null) table.addEntry0(e, newEntryIndex)
        else secondEntry.value = default
        default
      }
    }
  }

  override def getOrElse[V1 >: V](key: K, default: => V1): V1 = {
    if (getClass != classOf[HashMap[_, _]]) {
      // subclasses of HashMap might customise `get` ...
      super.getOrElse(key, default)
    } else {
      // .. but in the common case, we can avoid the Option boxing.
      val e = table.findEntry(key)
      if (e eq null) default else e.value
    }
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

  override protected[this] def stringPrefix = "HashMap"
}

/**
  * $factoryInfo
  *  @define Coll `mutable.HashMap`
  *  @define coll mutable hash map
  */
@SerialVersionUID(3L)
object HashMap extends MapFactory[HashMap] {

  def empty[K, V]: HashMap[K, V] = new HashMap[K, V]

  def from[K, V](it: collection.IterableOnce[(K, V)]): HashMap[K, V] = Growable.from(empty[K, V], it)

  def newBuilder[K, V]: Builder[(K, V), HashMap[K, V]] = new GrowableBuilder(HashMap.empty[K, V])

}

/** Class used internally for default map model.
  *  @since 2.3
  */
private[mutable] final class DefaultEntry[A, B](val key: A, var value: B)
  extends HashEntry[A, DefaultEntry[A, B]] {

  override def toString = chainString

  def chainString: String = {
    "(kv: " + key + ", " + value + ")" + (if (next != null) " -> " + next.toString else "")
  }
}
