package strawman
package collection.mutable

import strawman.collection.{Iterator, MapFactory}

import scala.{Boolean, Int, None, Option, SerialVersionUID, Serializable, Some, Unit}

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
    with HashTable[K, DefaultEntry[K, V]]
    with Serializable {

  initWithContents(contents)

  type Entry = DefaultEntry[K, V]

  def this() = this(null)

  protected[this] def fromSpecificIterable(coll: collection.Iterable[(K, V)]): HashMap[K, V] = HashMap.fromIterable(coll)
  protected[this] def mapFromIterable[K2, V2](it: collection.Iterable[(K2, V2)]): HashMap[K2, V2] = HashMap.fromIterable(it)

  def iterator(): Iterator[(K, V)] = entriesIterator.map(e => (e.key, e.value))

  def empty: HashMap[K, V] = HashMap.empty

  def get(key: K): Option[V] = {
    val e = findEntry(key)
    if (e eq null) None else Some(e.value)
  }

  def add(kv: (K, V)): this.type = {
    val e = findOrAddEntry(kv._1, kv._2)
    if (e ne null) e.value = kv._2
    this
  }

  def clear(): Unit = clearTable()

  def remove(key: K): this.type = { removeEntry(key); this }

  protected def createNewEntry[V2](key: K, value: V2): Entry = {
    new Entry(key, value.asInstanceOf[V])
  }

  override def size: Int = tableSize

  override def contains(key: K): Boolean = findEntry(key) != null

  override def apply(key: K): V = {
    val e = findEntry(key)
    if (e eq null) default(key)
    else e.value
  }

  override def foreach[U](f: ((K, V)) => U): Unit = foreachEntry(e => f((e.key, e.value)))

  override def put(key: K, value: V): Option[V] = {
    val e = findOrAddEntry(key, value)
    if (e eq null) None
    else { val v = e.value; e.value = value; Some(v) }
  }

  private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    serializeTo(out, { entry =>
      out.writeObject(entry.key)
      out.writeObject(entry.value)
    })
  }

  private def readObject(in: java.io.ObjectInputStream): Unit = {
    init(in, createNewEntry(in.readObject().asInstanceOf[K], in.readObject()))
  }

}

object HashMap extends MapFactory[HashMap] {

  def empty[K, V]: HashMap[K, V] = new HashMap[K, V]

  def fromIterable[K, V](it: collection.Iterable[(K, V)]): HashMap[K, V] =
    it match {
      case hm: HashMap[K, V] => hm
      case _ => empty ++= it
    }

}

/** Class used internally for default map model.
  *  @since 2.3
  */
final class DefaultEntry[A, B](val key: A, var value: B)
  extends HashEntry[A, DefaultEntry[A, B]]
    with Serializable {

  override def toString = chainString

  def chainString = {
    "(kv: " + key + ", " + value + ")" + (if (next != null) " -> " + next.toString else "")
  }
}
