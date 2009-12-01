/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package immutable

import generic._
import annotation.unchecked.uncheckedVariance

/** <p>
 *    This class implements immutable maps using a hash table.
 *  </p>
 *  <p>
 *    It is optimized for sequential accesses where the last updated table is
 *    accessed most often. It supports with reasonable efficiency accesses to
 *    previous versions of the table by keeping a change log that's regularly
 *    compacted. It needs to synchronize most methods, so it is less suitable
 *    for highly concurrent accesses.
 *  </p>
 *
 * @note the builder of a hash map returns specialized representations EmptyMap,Map1,..., Map4
 * for maps of size <= 4.
 *
 *  @author  Martin Odersky
 *  @version 2.0, 19/01/2007
 *  @since   2.3
 */
@serializable @SerialVersionUID(1L)
class HashMap[A, +B] extends Map[A,B] with MapLike[A, B, HashMap[A, B]] with mutable.HashTable[A] {

  type Entry = scala.collection.mutable.DefaultEntry[A, Any]

  @transient protected var later: HashMap[A, B @uncheckedVariance] = null
  @transient protected var oldKey: A = _
  @transient protected var oldValue: Option[B @uncheckedVariance] = _
  @transient protected var deltaSize: Int = _

  override def empty = HashMap.empty[A, B]

  def get(key: A): Option[B] = synchronized {
    var m: HashMap[A, _ >: B] = this
    var cnt = 0
    while (m.later != null) {
      if (key == m.oldKey) return m.oldValue.asInstanceOf[Option[B]]
      cnt += 1
      m = m.later
    }
    if (cnt > logLimit) makeCopy(m)
    val e = m.findEntry(key)
    if (e == null) None
    else Some(getValue(e))
  }

  override def updated [B1 >: B] (key: A, value: B1): HashMap[A, B1] = synchronized {
    makeCopyIfUpdated()
    val e = findEntry(key)
    if (e == null) {
      markUpdated(key, None, 1)
      later.addEntry(new Entry(key, value))
    } else {
      markUpdated(key, Some(getValue(e)), 0)
      e.value = value
    }
    later.asInstanceOf[HashMap[A, B1]]
  }

  /** Add a key/value pair to this map.
   *  @param    kv the key/value pair
   *  @return   A new map with the new binding added to this map
   */
  override def + [B1 >: B] (kv: (A, B1)): HashMap[A, B1] = updated(kv._1, kv._2)

  /** Adds two or more elements to this collection and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the added elements.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): HashMap[A, B1] =
    this + elem1 + elem2 ++ elems

  def - (key: A): HashMap[A, B] = synchronized {
    makeCopyIfUpdated()
    val e = findEntry(key)
    if (e == null) this
    else {
      markUpdated(key, Some(getValue(e)), -1)
      later removeEntry key
      later.asInstanceOf[HashMap[A, B]]
    }
  }

  override def size: Int = synchronized {
    var m: HashMap[A, _ >: B] = this
    var cnt = 0
    var s = 0
    while (m.later != null) {
      s -= m.deltaSize
      cnt += 1
      m = m.later
    }
    s += m.tableSize
    if (cnt > logLimit) makeCopy(m)
    s
  }

  def iterator = synchronized {
    makeCopyIfUpdated()
    entriesIterator map {e => (e.key, getValue(e))}
  }

  private def getValue(e: Entry) =
    e.value.asInstanceOf[B]

  private def logLimit: Int = math.sqrt(table.length).toInt

  private[this] def markUpdated(key: A, ov: Option[B], delta: Int) {
    val lf = loadFactor
    later = new HashMap[A, B] {
      override def initialSize = 0
      /* We need to do this to avoid a reference to the outer HashMap */
      _loadFactor = lf
      override def loadFactor = _loadFactor
      table     = HashMap.this.table
      tableSize = HashMap.this.tableSize
      threshold = HashMap.this.threshold
    }
    oldKey = key
    oldValue = ov
    deltaSize = delta
  }

  private def makeCopy(last: HashMap[A, _ >: B]) {
    def undo(m: HashMap[A, _ >: B]) {
      if (m ne last) {
        undo(m.later)
        if (m.deltaSize == 1) removeEntry(m.oldKey)
        else if (m.deltaSize == 0) findEntry(m.oldKey).value = m.oldValue.get
        else if (m.deltaSize == -1) addEntry(new Entry(m.oldKey, m.oldValue.get))
      }
    }
    def copy(e: Entry): Entry =
      if (e == null) null
      else {
        val rest = copy(e.next)
        val result = new Entry(e.key, e.value)
        result.next = rest
        result
      }
    val ltable = last.table
    val s = ltable.length
    table = new scala.Array[collection.mutable.HashEntry[A, Entry]](s)
    var i = 0
    while (i < s) {
      table(i) = copy(ltable(i).asInstanceOf[Entry])
      i += 1
    }
    tableSize = last.tableSize
    threshold = last.threshold
    undo(this)
    later = null
  }

  private def makeCopyIfUpdated() {
    var m: HashMap[A, _ >: B] = this
    while (m.later != null) m = m.later
    if (m ne this) makeCopy(m)
  }

  private def writeObject(out: java.io.ObjectOutputStream) {
    serializeTo(out, _.value)
  }

  private def readObject(in: java.io.ObjectInputStream) {
    init[B](in, new Entry(_, _))
  }
}

/** A factory object for immutable HashMaps.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.3
 */
object HashMap extends ImmutableMapFactory[HashMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), HashMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B]: HashMap[A, B] = new HashMap
}

