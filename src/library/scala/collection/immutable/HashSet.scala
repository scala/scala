/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package immutable

import generic._

/** <p>
 *    This class implements immutable sets using a hash table.
 *  </p>
 *  <p>
 *    It is optimized for sequential accesses where the last updated table is
 *    accessed most often. It supports with reasonable efficiency accesses to
 *    previous versions of the table by keeping a change log that's regularly
 *    compacted. It needs to synchronize most methods, so it is less suitable
 *    for highly concurrent accesses.
 *  </p>
 *
 * @note the builder of a hash set returns specialized representations EmptySet,Set1,..., Set4
 * for sets of size <= 4.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.3
 */
@serializable @SerialVersionUID(1L)
class HashSet[A] extends Set[A]
                    with GenericSetTemplate[A, HashSet]
                    with SetLike[A, HashSet[A]]
                    with mutable.FlatHashTable[A] {
  override def companion: GenericCompanion[HashSet] = HashSet

  @transient protected var later: HashSet[A] = null
  @transient protected var changedElem: A = _
  @transient protected var deleted: Boolean = _

  def contains(elem: A): Boolean = synchronized {
    var m = this
    var cnt = 0
    while (m.later != null) {
      if (elem == m.changedElem) return m.deleted
      cnt += 1
      m = m.later
    }
    if (cnt > logLimit) makeCopy(m)
    m.containsEntry(elem)
  }

  def + (elem: A): HashSet[A] = synchronized {
    makeCopyIfUpdated()
    if (containsEntry(elem)) this
    else {
      markUpdated(elem, false)
      later addEntry elem
      later
    }
  }

  def - (elem: A): HashSet[A] = synchronized {
    makeCopyIfUpdated()
    if (!containsEntry(elem)) this
    else {
      markUpdated(elem, true)
      later removeEntry elem
      later
    }
  }

  override def size: Int = synchronized {
    var m = this
    var cnt = 0
    var s = 0
    while (m.later != null) {
      if (m.deleted) s += 1 else s -= 1
      cnt += 1
      m = m.later
    }
    s += m.tableSize
    if (cnt > logLimit) makeCopy(m)
    s
  }

  override def iterator = synchronized {
    makeCopyIfUpdated()
    // note need to cache because (later versions of) set might be mutated while elements are traversed.
    val cached = new mutable.ArrayBuffer() ++= super.iterator
    cached.iterator
  }

  private def logLimit: Int = math.sqrt(table.length).toInt

  private def markUpdated(elem: A, del: Boolean) {
    val lf = loadFactor
    later = new HashSet[A] {
      override def initialSize = 0
      /* We need to do this to avoid a reference to the outer HashMap */
      def _newLoadFactor = lf
      override def loadFactor = _newLoadFactor
      table     = HashSet.this.table
      tableSize = HashSet.this.tableSize
      threshold = HashSet.this.threshold
    }
    changedElem = elem
    deleted = del
  }

  private def makeCopy(last: HashSet[A]) {
    def undo(m: HashSet[A]) {
      if (m ne last) {
        undo(m.later)
        if (m.deleted) addEntry(m.changedElem)
        else removeEntry(m.changedElem)
      }
    }
    table = new scala.Array[AnyRef](last.table.length)
    scala.Array.copy(last.table, 0, table, 0, table.length)
    tableSize = last.tableSize
    threshold = last.threshold
    undo(this)
    later = null
  }

  private def makeCopyIfUpdated() {
    var m = this
    while (m.later != null) m = m.later
    if (m ne this) makeCopy(m)
  }

  private def writeObject(s: java.io.ObjectOutputStream) {
    serializeTo(s)
  }

  private def readObject(in: java.io.ObjectInputStream) {
    init(in, x => x)
  }
}

/** A factory object for immutable HashSets.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.3
 */
object HashSet extends SetFactory[HashSet] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, HashSet[A]] = setCanBuildFrom[A]
  override def empty[A]: HashSet[A] = new HashSet
}

