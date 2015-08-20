/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package mutable

import generic._
import scala.collection.parallel.mutable.ParHashMap

/** This class implements mutable maps using a hashtable.
 *
 *  @since 1
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#hash_tables "Scala's Collection Library overview"]]
 *  section on `Hash Tables` for more information.
 *
 *  @tparam A    the type of the keys contained in this hash map.
 *  @tparam B    the type of the values assigned to keys in this hash map.
 *
 *  @define Coll `mutable.HashMap`
 *  @define coll mutable hash map
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is always `HashMap[A, B]` if the elements contained in the resulting collection are
 *    pairs of type `(A, B)`. This is because an implicit of type `CanBuildFrom[HashMap, (A, B), HashMap[A, B]]`
 *    is defined in object `HashMap`. Otherwise, `That` resolves to the most specific type that doesn't have
 *    to contain pairs of type `(A, B)`, which is `Iterable`.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`. This is usually the `canBuildFrom` value
 *    defined in object `HashMap`.
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(1L)
class HashMap[A, B] private[collection] (contents: HashTable.Contents[A, DefaultEntry[A, B]])
extends AbstractMap[A, B]
   with Map[A, B]
   with MapLike[A, B, HashMap[A, B]]
   with HashTable[A, DefaultEntry[A, B]]
   with CustomParallelizable[(A, B), ParHashMap[A, B]]
   with Serializable
{
  initWithContents(contents)

  type Entry = DefaultEntry[A, B]

  override def empty: HashMap[A, B] = HashMap.empty[A, B]
  override def clear() { clearTable() }
  override def size: Int = tableSize

  def this() = this(null)

  override def par = new ParHashMap[A, B](hashTableContents)

  // contains and apply overridden to avoid option allocations.
  override def contains(key: A): Boolean = findEntry(key) != null

  override def apply(key: A): B = {
    val result = findEntry(key)
    if (result eq null) default(key)
    else result.value
  }

  def get(key: A): Option[B] = {
    val e = findEntry(key)
    if (e eq null) None
    else Some(e.value)
  }

  override def put(key: A, value: B): Option[B] = {
    val e = findOrAddEntry(key, value)
    if (e eq null) None
    else { val v = e.value; e.value = value; Some(v) }
  }

  override def update(key: A, value: B): Unit = put(key, value)

  override def remove(key: A): Option[B] = {
    val e = removeEntry(key)
    if (e ne null) Some(e.value)
    else None
  }

  def += (kv: (A, B)): this.type = {
    val e = findOrAddEntry(kv._1, kv._2)
    if (e ne null) e.value = kv._2
    this
  }

  def -=(key: A): this.type = { removeEntry(key); this }

  def iterator = entriesIterator map (e => ((e.key, e.value)))

  override def foreach[U](f: ((A, B)) => U): Unit = foreachEntry(e => f((e.key, e.value)))

  /* Override to avoid tuple allocation in foreach */
  override def keySet: scala.collection.Set[A] = new DefaultKeySet {
    override def foreach[U](f: A => U) = foreachEntry(e => f(e.key))
  }

  /* Override to avoid tuple allocation in foreach */
  override def values: scala.collection.Iterable[B] = new DefaultValuesIterable {
    override def foreach[U](f: B => U) = foreachEntry(e => f(e.value))
  }

  /* Override to avoid tuple allocation */
  override def keysIterator: Iterator[A] = new AbstractIterator[A] {
    val iter    = entriesIterator
    def hasNext = iter.hasNext
    def next()  = iter.next().key
  }

  /* Override to avoid tuple allocation */
  override def valuesIterator: Iterator[B] = new AbstractIterator[B] {
    val iter    = entriesIterator
    def hasNext = iter.hasNext
    def next()  = iter.next().value
  }

  /** Toggles whether a size map is used to track hash map statistics.
   */
  def useSizeMap(t: Boolean) = if (t) {
    if (!isSizeMapDefined) sizeMapInitAndRebuild()
  } else sizeMapDisable()

  protected def createNewEntry[B1](key: A, value: B1): Entry = {
    new Entry(key, value.asInstanceOf[B])
  }

  private def writeObject(out: java.io.ObjectOutputStream) {
    serializeTo(out, { entry =>
      out.writeObject(entry.key)
      out.writeObject(entry.value)
    })
  }

  private def readObject(in: java.io.ObjectInputStream) {
    init(in, createNewEntry(in.readObject().asInstanceOf[A], in.readObject()))
  }

}

/** $factoryInfo
 *  @define Coll `mutable.HashMap`
 *  @define coll mutable hash map
 */
object HashMap extends MutableMapFactory[HashMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), HashMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B]: HashMap[A, B] = new HashMap[A, B]
}
