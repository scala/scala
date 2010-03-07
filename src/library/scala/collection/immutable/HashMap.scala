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
import annotation.unchecked.uncheckedVariance

/** <p>
 *    This class implements immutable maps using a hash trie.
 *  </p>
 *
 * @note the builder of a hash map returns specialized representations EmptyMap,Map1,..., Map4
 * for maps of size <= 4.
 *
 *  @author  Martin Odersky
 *  @author  Tiark Rompf
 *  @since   2.3
 */
@serializable @SerialVersionUID(2L)
class HashMap[A, +B] extends Map[A,B] with MapLike[A, B, HashMap[A, B]] {

  override def size: Int = 0

  override def empty = HashMap.empty[A, B]

  def iterator: Iterator[(A,B)] = Iterator.empty

  override def foreach[U](f: ((A, B)) =>  U): Unit = { }

  def get(key: A): Option[B] =
    get0(key, computeHash(key), 0)

  override def updated [B1 >: B] (key: A, value: B1): HashMap[A, B1] =
    updated0(key, computeHash(key), 0, value, null)

  override def + [B1 >: B] (kv: (A, B1)): HashMap[A, B1] =
    updated0(kv._1, computeHash(kv._1), 0, kv._2, kv)

  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): HashMap[A, B1] =
    this + elem1 + elem2 ++ elems
    // TODO: optimize (might be able to use mutable updates)

  def - (key: A): HashMap[A, B] =
    removed0(key, computeHash(key), 0)

  protected def elemHashCode(key: A) = if (key == null) 0 else key.hashCode()

  protected final def improve(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  protected def computeHash(key: A) = improve(elemHashCode(key))

  protected def get0(key: A, hash: Int, level: Int): Option[B] = None

  protected def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1)): HashMap[A, B1] =
    new HashMap.HashMap1(key, hash, value, kv)

  protected def removed0(key: A, hash: Int, level: Int): HashMap[A, B] = this

}

/** A factory object for immutable HashMaps.
 *
 *  @author  Martin Odersky
 *  @author  Tiark Rompf
 *  @version 2.8
 *  @since   2.3
 */
object HashMap extends ImmutableMapFactory[HashMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), HashMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B]: HashMap[A, B] = EmptyHashMap.asInstanceOf[HashMap[A, B]]

  private object EmptyHashMap extends HashMap[Any,Nothing] {

  }

  // TODO: add HashMap2, HashMap3, ...

  class HashMap1[A,+B](private var key: A, private[HashMap] var hash: Int, private var value: (B @uncheckedVariance), private var kv: (A,B @uncheckedVariance)) extends HashMap[A,B] {
    override def size = 1

    override def get0(key: A, hash: Int, level: Int): Option[B] =
      if (hash == this.hash && key == this.key) Some(value) else None

    override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1)): HashMap[A, B1] =
      if (hash == this.hash && key == this.key) new HashMap1(key, hash, value, kv)
      else {
        // TODO: handle 32-bit hash collisions (rare, but not impossible)
        assert(hash != this.hash, "32-bit hash collisions not handled yet");
        //new HashTrieMap[A,B1](level+5, this, new HashMap1(key, hash, value, kv))
        val m = new HashTrieMap[A,B1](0,new Array[HashMap[A,B1]](0),0) // TODO: could save array alloc
        m.updated0(this.key, this.hash, level, this.value, this.kv).updated0(key, hash, level, value, kv)
      }

    override def removed0(key: A, hash: Int, level: Int): HashMap[A, B] =
      if (hash == this.hash && key == this.key) HashMap.empty[A,B] else this

    override def iterator: Iterator[(A,B)] = Iterator(ensurePair)
    override def foreach[U](f: ((A, B)) => U): Unit = f(ensurePair)
    private def ensurePair: (A,B) = if (kv ne null) kv else { kv = (key, value); kv }

    private def writeObject(out: java.io.ObjectOutputStream) {
      out.writeObject(key)
      out.writeObject(value)
    }

    private def readObject(in: java.io.ObjectInputStream) {
      key = in.readObject().asInstanceOf[A]
      value = in.readObject().asInstanceOf[B]
      hash = computeHash(key)
    }

  }

  class HashTrieMap[A,+B](private var bitmap: Int, private var elems: Array[HashMap[A,B @uncheckedVariance]],
      private var size0: Int) extends HashMap[A,B] {
/*
    def this (level: Int, m1: HashMap1[A,B], m2: HashMap1[A,B]) = {
      this(((m1.hash >>> level) & 0x1f) | ((m2.hash >>> level) & 0x1f), {
        val idx1 = (m1.hash >>> level) & 0x1f
        val idx2 = (m2.hash >>> level) & 0x1f
        assert(idx1 != idx2, m1.hash + "==" + m2.hash + " at level " + level) // TODO
        val elems = new Array[HashMap[A,B]](2)
        if (idx1 < idx2) {
          elems(0) = m1
          elems(1) = m2
        } else {
          elems(0) = m2
          elems(1) = m1
        }
        elems
      }, 2)
    }
*/
    override def size = size0

    override def get0(key: A, hash: Int, level: Int): Option[B] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      if (bitmap == - 1) {
        elems(index & 0x1f).get0(key, hash, level + 5)
      } else if ((bitmap & mask) != 0) {
        val offset = Integer.bitCount(bitmap & (mask-1))
        elems(offset).get0(key, hash, level + 5)
      } else
        None
    }

    override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1)): HashMap[A, B1] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask-1))
      if ((bitmap & mask) != 0) {
        val elemsNew = new Array[HashMap[A,B1]](elems.length)
        Array.copy(elems, 0, elemsNew, 0, elems.length)
        val sub = elems(offset)
        val subNew = sub.updated0(key, hash, level + 5, value, kv)
        elemsNew(offset) = subNew
        new HashTrieMap(bitmap, elemsNew, size + (subNew.size - sub.size))
      } else {
        assert(offset <= elems.length, offset +">"+elems.length)
        val elemsNew = new Array[HashMap[A,B1]](elems.length + 1)
        Array.copy(elems, 0, elemsNew, 0, offset)
        elemsNew(offset) = new HashMap1(key, hash, value, kv)
        Array.copy(elems, offset, elemsNew, offset + 1, elems.length - offset)
        val bitmapNew = bitmap | mask
        new HashTrieMap(bitmapNew, elemsNew, size + 1)
      }
    }

    override def removed0(key: A, hash: Int, level: Int): HashMap[A, B] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask-1))
      if (((bitmap >>> index) & 1) == 1) {
        val elemsNew = new Array[HashMap[A,B]](elems.length)
        Array.copy(elems, 0, elemsNew, 0, elems.length)
        val sub = elems(offset)
        val subNew = sub.removed0(key, hash, level + 5)
        elemsNew(offset) = subNew
        // TODO: handle shrinking
        val sizeNew = size + (subNew.size - sub.size)
        if (sizeNew > 0)
          new HashTrieMap(bitmap, elemsNew, size + (subNew.size - sub.size))
        else
          HashMap.empty[A,B]
      } else {
        this
      }
    }


    override def iterator = {   // TODO: optimize (use a stack to keep track of pos)
      var it: Iterator[(A,B)] = Iterator.empty
      if (bitmap != 0)
        elems foreach ((m: Map[_,_]) => it = it ++ m.asInstanceOf[Map[A,B]].iterator)
      it
    }

/*
    override def iterator = new Iterator[A,B] {
      var depth = 0
      var nodeStack = new Array[HashTrieMap[A,B]](6)
      var indexStack = new Array[Int](6)

      var curNode = this[HashTrieMap]
      var curIndex = 0
      var maxIndex = curNode.elems.length

      def hasNext = curIndex < maxIndex

      def next(): (A,B) = {
        val sub = curNode.elems(curIndex)
        sub match {
          case sub: HashTrieMap =>
            // go down
          case sub: HashMap1 =>
            // produce value
        }

      }
*/

    override def foreach[U](f: ((A, B)) =>  U): Unit = {
      var i = 0;
      while (i < elems.length) {
        elems(i).foreach(f)
        i += 1
      }
    }


    private def writeObject(out: java.io.ObjectOutputStream) {
      // no out.defaultWriteObject()
      out.writeInt(size)
      foreach { p =>
        out.writeObject(p._1)
        out.writeObject(p._2)
      }
    }

    private def readObject(in: java.io.ObjectInputStream) {
      val size = in.readInt
      var index = 0
      var m = HashMap.empty[A,B]
      while (index < size) {
        // TODO: optimize (use mutable update)
        m = m + ((in.readObject.asInstanceOf[A], in.readObject.asInstanceOf[B]))
        index += 1
      }
      var tm = m.asInstanceOf[HashTrieMap[A,B]]
      bitmap = tm.bitmap
      elems = tm.elems
      size0 = tm.size0
    }

  }

}

