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

/** $factoryInfo
 *  @define Coll `LinkedHashMap`
 *  @define coll linked hash map
 */
object LinkedHashMap extends MutableMapFactory[LinkedHashMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), LinkedHashMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B] = new LinkedHashMap[A, B]
}

/** This class implements mutable maps using a hashtable.
 *  The iterator and all traversal methods of this class visit elements in the order they were inserted.
 *
 *  @tparam A    the type of the keys contained in this hash map.
 *  @tparam B    the type of the values assigned to keys in this hash map.
 *
 *  @define Coll `LinkedHashMap`
 *  @define coll linked hash map
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is always `LinkedHashMap[A, B]` if the elements contained in the resulting collection are
 *    pairs of type `(A, B)`. This is because an implicit of type `CanBuildFrom[LinkedHashMap, (A, B), LinkedHashMap[A, B]]`
 *    is defined in object `LinkedHashMap`. Otherwise, `That` resolves to the most specific type that doesn't have
 *    to contain pairs of type `(A, B)`, which is `Iterable`.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`. This is usually the `canBuildFrom` value
 *    defined in object `LinkedHashMap`.
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *  @define orderDependent
 *  @define orderDependentFold
 */
@SerialVersionUID(1L)
class LinkedHashMap[A, B] extends AbstractMap[A, B]
                             with Map[A, B]
                             with MapLike[A, B, LinkedHashMap[A, B]]
                             with HashTable[A, LinkedEntry[A, B]]
                             with Serializable
{

  override def empty = LinkedHashMap.empty[A, B]
  override def size = tableSize

  type Entry = LinkedEntry[A, B]

  @transient protected var firstEntry: Entry = null
  @transient protected var lastEntry: Entry = null

  def get(key: A): Option[B] = {
    val e = findEntry(key)
    if (e == null) None
    else Some(e.value)
  }

  override def put(key: A, value: B): Option[B] = {
    val e = findOrAddEntry(key, value)
    if (e eq null) None
    else { val v = e.value; e.value = value; Some(v) }
  }

  override def remove(key: A): Option[B] = {
    val e = removeEntry(key)
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

  @deprecatedOverriding("+= should not be overridden so it stays consistent with put.", "2.11.0")
  def += (kv: (A, B)): this.type = { put(kv._1, kv._2); this }

  @deprecatedOverriding("-= should not be overridden so it stays consistent with remove.", "2.11.0")
  def -=(key: A): this.type = { remove(key); this }

  def iterator: Iterator[(A, B)] = new AbstractIterator[(A, B)] {
    private var cur = firstEntry
    def hasNext = cur ne null
    def next =
      if (hasNext) { val res = (cur.key, cur.value); cur = cur.later; res }
      else Iterator.empty.next()
  }

  protected class FilteredKeys(p: A => Boolean) extends super.FilteredKeys(p) {
    override def empty = LinkedHashMap.empty
  }

  override def filterKeys(p: A => Boolean): scala.collection.Map[A, B] = new FilteredKeys(p)

  protected class MappedValues[C](f: B => C) extends super.MappedValues[C](f) {
    override def empty = LinkedHashMap.empty
  }

  override def mapValues[C](f: B => C): scala.collection.Map[A, C] = new MappedValues(f)

  protected class DefaultKeySet extends super.DefaultKeySet {
    override def empty = LinkedHashSet.empty
  }

  override def keySet: scala.collection.Set[A] = new DefaultKeySet

  override def keysIterator: Iterator[A] = new AbstractIterator[A] {
    private var cur = firstEntry
    def hasNext = cur ne null
    def next =
      if (hasNext) { val res = cur.key; cur = cur.later; res }
      else Iterator.empty.next()
  }

  override def valuesIterator: Iterator[B] = new AbstractIterator[B] {
    private var cur = firstEntry
    def hasNext = cur ne null
    def next =
      if (hasNext) { val res = cur.value; cur = cur.later; res }
      else Iterator.empty.next()
  }

  override def foreach[U](f: ((A, B)) => U) {
    var cur = firstEntry
    while (cur ne null) {
      f((cur.key, cur.value))
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

  protected def createNewEntry[B1](key: A, value: B1): Entry = {
    val e = new Entry(key, value.asInstanceOf[B])
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
    serializeTo(out, { entry =>
      out.writeObject(entry.key)
      out.writeObject(entry.value)
    })
  }

  private def readObject(in: java.io.ObjectInputStream) {
    firstEntry = null
    lastEntry = null
    init(in, createNewEntry(in.readObject().asInstanceOf[A], in.readObject()))
  }
}
