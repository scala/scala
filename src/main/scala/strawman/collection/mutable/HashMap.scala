package strawman
package collection.mutable

import strawman.collection.{Iterator, MapFactory, MapFactoryWithBuilder, StrictOptimizedIterableOps}

import scala.{Boolean, Int, None, Option, SerialVersionUID, Serializable, Some, Unit}
import java.lang.String

/** This class implements mutable maps using a hashtable.
  *
  *  @since 1
  *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#hash_tables "Scala's Collection Library overview"]]
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
@SerialVersionUID(1L)
final class HashMap[K, V] private[collection] (contents: HashTable.Contents[K, DefaultEntry[K, V]])
  extends Map[K, V]
    with MapOps[K, V, HashMap, HashMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), HashMap[K, V]]
    with Serializable {

  private[this] val table: HashTable[K, V, DefaultEntry[K, V]] =
    new HashTable[K, V, DefaultEntry[K, V]] {
      def createNewEntry(key: K, value: V): DefaultEntry[K, V] = new Entry(key, value)
    }

  table.initWithContents(contents)

  type Entry = DefaultEntry[K, V]

  def this() = this(null)

  protected[this] def fromSpecificIterable(coll: collection.Iterable[(K, V)]): HashMap[K, V] = HashMap.fromIterable(coll)
  protected[this] def mapFromIterable[K2, V2](it: collection.Iterable[(K2, V2)]): HashMap[K2, V2] = HashMap.fromIterable(it)

  protected[this] def newSpecificBuilder(): Builder[(K, V), HashMap[K, V]] =  new GrowableBuilder(HashMap.empty[K, V])

  def iterator(): Iterator[(K, V)] = table.entriesIterator.map(e => (e.key, e.value))

  def empty: HashMap[K, V] = HashMap.empty

  def get(key: K): Option[V] = {
    val e = table.findEntry(key)
    if (e eq null) None else Some(e.value)
  }

  def add(kv: (K, V)): this.type = {
    val e = table.findOrAddEntry(kv._1, kv._2)
    if (e ne null) e.value = kv._2
    this
  }

  def clear(): Unit = table.clearTable()

  def subtract(key: K): this.type = { table.removeEntry(key); this }

  override def size: Int = table.size

  override def contains(key: K): Boolean = table.findEntry(key) != null

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

  override def getOrElseUpdate(key: K, defaultValue: => V): V = {
    val hash = table.elemHashCode(key)
    val i = table.index(hash)
    val entry = table.findEntry0(key, i)
    if (entry != null) entry.value
    else {
      val table0 = table
      val default = defaultValue
      // Avoid recomputing index if the `defaultValue()` hasn't triggered
      // a table resize.
      val newEntryIndex = if (table0 eq table) i else table.index(hash)
      table.addEntry0(table.createNewEntry(key, default), newEntryIndex)
      default
    }
  }

  private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    table.serializeTo(out, { entry =>
      out.writeObject(entry.key)
      out.writeObject(entry.value)
    })
  }

  private def readObject(in: java.io.ObjectInputStream): Unit = {
    table.init(in, table.createNewEntry(in.readObject().asInstanceOf[K], in.readObject().asInstanceOf[V]))
  }

}

object HashMap extends MapFactory[HashMap] {

  def empty[K, V]: HashMap[K, V] = new HashMap[K, V]

  def fromIterable[K, V](it: collection.Iterable[(K, V)]): HashMap[K, V] = Growable.fromIterable(empty[K, V], it)

}

/** Class used internally for default map model.
  *  @since 2.3
  */
final class DefaultEntry[A, B](val key: A, var value: B)
  extends HashEntry[A, DefaultEntry[A, B]]
    with Serializable {

  override def toString = chainString

  def chainString: String = {
    "(kv: " + key + ", " + value + ")" + (if (next != null) " -> " + next.toString else "")
  }
}
