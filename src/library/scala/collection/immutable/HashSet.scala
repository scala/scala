/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package immutable

import generic._
import annotation.unchecked.uncheckedVariance

import collection.parallel.immutable.ParHashSet

/** This class implements immutable sets using a hash trie.
 *
 *  '''Note:''' the builder of a hash set returns specialized representations `EmptySet`,`Set1`,..., `Set4`
 *  for sets of `size <= 4`.
 *
 *  @tparam A      the type of the elements contained in this hash set.
 *
 *  @author  Martin Odersky
 *  @author  Tiark Rompf
 *  @version 2.8
 *  @since   2.3
 *  @define Coll immutable.HashSet
 *  @define coll immutable hash set
 */
@serializable @SerialVersionUID(2L)
class HashSet[A] extends Set[A]
                    with GenericSetTemplate[A, HashSet]
                    with SetLike[A, HashSet[A]]
                    with Parallelizable[ParHashSet[A]]
{
  override def companion: GenericCompanion[HashSet] = HashSet

  //class HashSet[A] extends Set[A] with SetLike[A, HashSet[A]] {

  override def size: Int = 0

  override def empty = HashSet.empty[A]

  def iterator: Iterator[A] = Iterator.empty

  override def foreach[U](f: A =>  U): Unit = { }

  def contains(e: A): Boolean = get0(e, computeHash(e), 0)

  override def + (e: A): HashSet[A] = updated0(e, computeHash(e), 0)

  override def + (elem1: A, elem2: A, elems: A*): HashSet[A] =
    this + elem1 + elem2 ++ elems
    // TODO: optimize (might be able to use mutable updates)

  def - (e: A): HashSet[A] =
    removed0(e, computeHash(e), 0)

  def par = ParHashSet.fromTrie(this)

  protected def elemHashCode(key: A) = if (key == null) 0 else key.##

  protected final def improve(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  private[collection] def computeHash(key: A) = improve(elemHashCode(key))

  protected def get0(key: A, hash: Int, level: Int): Boolean = false

  def updated0(key: A, hash: Int, level: Int): HashSet[A] =
    new HashSet.HashSet1(key, hash)

  protected def removed0(key: A, hash: Int, level: Int): HashSet[A] = this

  protected def writeReplace(): AnyRef = new HashSet.SerializationProxy(this)
}

/** $factoryInfo
 *  @define Coll immutable.HashSet
 *  @define coll immutable hash set
 *
 *  @author  Tiark Rompf
 *  @since   2.3
 *  @define Coll immutable.HashSet
 *  @define coll immutable hash set
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
object HashSet extends ImmutableSetFactory[HashSet] {
  /** $setCanBuildFromInfo */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, HashSet[A]] = setCanBuildFrom[A]
  override def empty[A]: HashSet[A] = EmptyHashSet.asInstanceOf[HashSet[A]]

  private object EmptyHashSet extends HashSet[Any] {
  }

  // TODO: add HashSet2, HashSet3, ...

  class HashSet1[A](private[HashSet] var key: A, private[HashSet] var hash: Int) extends HashSet[A] {
    override def size = 1

    override def get0(key: A, hash: Int, level: Int): Boolean =
      (hash == this.hash && key == this.key)

    override def updated0(key: A, hash: Int, level: Int): HashSet[A] =
      if (hash == this.hash && key == this.key) this
      else {
        if (hash != this.hash) {
          //new HashTrieSet[A](level+5, this, new HashSet1(key, hash))
          val m = new HashTrieSet[A](0,new Array[HashSet[A]](0),0) // TODO: could save array alloc
          m.updated0(this.key, this.hash, level).updated0(key, hash, level)
        } else {
          // 32-bit hash collision (rare, but not impossible)
          new HashSetCollision1(hash, ListSet.empty + this.key + key)
        }
      }

    override def removed0(key: A, hash: Int, level: Int): HashSet[A] =
      if (hash == this.hash && key == this.key) HashSet.empty[A] else this

    override def iterator: Iterator[A] = Iterator(key)
    override def foreach[U](f: A => U): Unit = f(key)
  }

  private class HashSetCollision1[A](private[HashSet] var hash: Int, var ks: ListSet[A]) extends HashSet[A] {
    override def size = ks.size

    override def get0(key: A, hash: Int, level: Int): Boolean =
      if (hash == this.hash) ks.contains(key) else false

    override def updated0(key: A, hash: Int, level: Int): HashSet[A] =
      if (hash == this.hash) new HashSetCollision1(hash, ks + key)
      else {
        var m: HashSet[A] = new HashTrieSet[A](0,new Array[HashSet[A]](0),0)
        // might be able to save some ops here, but it doesn't seem to be worth it
        for (k <- ks)
          m = m.updated0(k, this.hash, level)
        m.updated0(key, hash, level)
      }

    override def removed0(key: A, hash: Int, level: Int): HashSet[A] =
      if (hash == this.hash) {
        val ks1 = ks - key
        if (!ks1.isEmpty)
          new HashSetCollision1(hash, ks1)
        else
          HashSet.empty[A]
      } else this

    override def iterator: Iterator[A] = ks.iterator
    override def foreach[U](f: A => U): Unit = ks.foreach(f)

    private def writeObject(out: java.io.ObjectOutputStream) {
      // this cannot work - reading things in might produce different
      // hash codes and remove the collision. however this is never called
      // because no references to this class are ever handed out to client code
      // and HashTrieSet serialization takes care of the situation
      error("cannot serialize an immutable.HashSet where all items have the same 32-bit hash code")
      //out.writeObject(kvs)
    }

    private def readObject(in: java.io.ObjectInputStream) {
      error("cannot deserialize an immutable.HashSet where all items have the same 32-bit hash code")
      //kvs = in.readObject().asInstanceOf[ListSet[A]]
      //hash = computeHash(kvs.)
    }

  }


  class HashTrieSet[A](private var bitmap: Int, private[HashSet] var elems: Array[HashSet[A]],
      private var size0: Int) extends HashSet[A] {

    override def size = size0

    override def get0(key: A, hash: Int, level: Int): Boolean = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      if (bitmap == - 1) {
        elems(index & 0x1f).get0(key, hash, level + 5)
      } else if ((bitmap & mask) != 0) {
        val offset = Integer.bitCount(bitmap & (mask-1))
        // TODO: might be worth checking if sub is HashTrieSet (-> monomorphic call site)
        elems(offset).get0(key, hash, level + 5)
      } else
        false
    }

    override def updated0(key: A, hash: Int, level: Int): HashSet[A] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask-1))
      if ((bitmap & mask) != 0) {
        val elemsNew = new Array[HashSet[A]](elems.length)
        Array.copy(elems, 0, elemsNew, 0, elems.length)
        val sub = elems(offset)
        // TODO: might be worth checking if sub is HashTrieSet (-> monomorphic call site)
        val subNew = sub.updated0(key, hash, level + 5)
        elemsNew(offset) = subNew
        new HashTrieSet(bitmap, elemsNew, size + (subNew.size - sub.size))
      } else {
        val elemsNew = new Array[HashSet[A]](elems.length + 1)
        Array.copy(elems, 0, elemsNew, 0, offset)
        elemsNew(offset) = new HashSet1(key, hash)
        Array.copy(elems, offset, elemsNew, offset + 1, elems.length - offset)
        val bitmapNew = bitmap | mask
        new HashTrieSet(bitmapNew, elemsNew, size + 1)
      }
    }

    override def removed0(key: A, hash: Int, level: Int): HashSet[A] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask-1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        // TODO: might be worth checking if sub is HashTrieMap (-> monomorphic call site)
        val subNew = sub.removed0(key, hash, level + 5)
        if (subNew.isEmpty) {
          val bitmapNew = bitmap ^ mask
          if (bitmapNew != 0) {
            val elemsNew = new Array[HashSet[A]](elems.length - 1)
            Array.copy(elems, 0, elemsNew, 0, offset)
            Array.copy(elems, offset + 1, elemsNew, offset, elems.length - offset - 1)
            val sizeNew = size - sub.size
            new HashTrieSet(bitmapNew, elemsNew, sizeNew)
          } else
            HashSet.empty[A]
        } else {
          val elemsNew = new Array[HashSet[A]](elems.length)
          Array.copy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          val sizeNew = size + (subNew.size - sub.size)
          new HashTrieSet(bitmap, elemsNew, sizeNew)
        }
      } else {
        this
      }
    }

    override def iterator = new TrieIterator[A](elems)

/*

import collection.immutable._
def time(block: =>Unit) = { val t0 = System.nanoTime; block; println("elapsed: " + (System.nanoTime - t0)/1000000.0) }
var mOld = OldHashSet.empty[Int]
var mNew = HashSet.empty[Int]
time { for (i <- 0 until 100000) mOld = mOld + i }
time { for (i <- 0 until 100000) mOld = mOld + i }
time { for (i <- 0 until 100000) mOld = mOld + i }
time { for (i <- 0 until 100000) mNew = mNew + i }
time { for (i <- 0 until 100000) mNew = mNew + i }
time { for (i <- 0 until 100000) mNew = mNew + i }
time { mOld.iterator.foreach( p => ()) }
time { mOld.iterator.foreach( p => ()) }
time { mOld.iterator.foreach( p => ()) }
time { mNew.iterator.foreach( p => ()) }
time { mNew.iterator.foreach( p => ()) }
time { mNew.iterator.foreach( p => ()) }

*/

    override def foreach[U](f: A =>  U): Unit = {
      var i = 0;
      while (i < elems.length) {
        elems(i).foreach(f)
        i += 1
      }
    }
  }


  class TrieIterator[A](elems: Array[HashSet[A]]) extends Iterator[A] {
    private[this] var depth = 0
    private[this] var arrayStack = new Array[Array[HashSet[A]]](6)
    private[this] var posStack = new Array[Int](6)

    private[this] var arrayD = elems
    private[this] var posD = 0

    private[this] var subIter: Iterator[A] = null // to traverse collision nodes

    def hasNext = (subIter ne null) || depth >= 0

    def next: A = {
      if (subIter ne null) {
        val el = subIter.next
        if (!subIter.hasNext)
          subIter = null
        el
      } else
        next0(arrayD, posD)
    }

    @scala.annotation.tailrec private[this] def next0(elems: Array[HashSet[A]], i: Int): A = {
      if (i == elems.length - 1) { // reached end of level, pop stack
        depth -= 1
        if (depth >= 0) {
          arrayD = arrayStack(depth)
          posD = posStack(depth)
          arrayStack(depth) = null
        } else {
          arrayD = null
          posD = 0
        }
      } else
        posD += 1

      elems(i) match {
        case m: HashTrieSet[A] => // push current pos onto stack and descend
          if (depth >= 0) {
            arrayStack(depth) = arrayD
            posStack(depth) = posD
          }
          depth += 1
          arrayD = m.elems
          posD = 0
          next0(m.elems, 0)
        case m: HashSet1[A] => m.key
        case m =>
          subIter = m.iterator
          next
      }
    }

    // assumption: contains 2 or more elements
    // splits this iterator into 2 iterators
    // returns the 1st iterator, its number of elements, and the second iterator
    def split: ((Iterator[A], Int), Iterator[A]) = {
      def collisionToArray(c: HashSetCollision1[_]) =
        c.asInstanceOf[HashSetCollision1[A]].ks.toList map { HashSet() + _ } toArray
      def arrayToIterators(arr: Array[HashSet[A]]) = {
        val (fst, snd) = arr.splitAt(arr.length / 2)
        val szsnd = snd.foldLeft(0)(_ + _.size)
        ((new TrieIterator(snd), szsnd), new TrieIterator(fst))
      }
      def splitArray(ad: Array[HashSet[A]]): ((Iterator[A], Int), Iterator[A]) = if (ad.length > 1) {
        arrayToIterators(ad)
      } else ad(0) match {
        case c: HashSetCollision1[a] => arrayToIterators(collisionToArray(c.asInstanceOf[HashSetCollision1[A]]))
        case hm: HashTrieSet[a] => splitArray(hm.elems.asInstanceOf[Array[HashSet[A]]])
      }

      // 0) simple case: no elements have been iterated - simply divide arrayD
      if (arrayD != null && depth == 0 && posD == 0) {
        return splitArray(arrayD)
      }

      // otherwise, some elements have been iterated over
      // 1) collision case: if we have a subIter, we return subIter and elements after it
      if (subIter ne null) {
        val buff = subIter.toBuffer
        subIter = null
        ((buff.iterator, buff.length), this)
      } else {
        // otherwise find the topmost array stack element
        if (depth > 0) {
          // 2) topmost comes before (is not) arrayD
          //    steal a portion of top to create a new iterator
          val topmost = arrayStack(0)
          if (posStack(0) == arrayStack(0).length - 1) {
            // 2a) only a single entry left on top
            // this means we have to modify this iterator - pop topmost
            val snd = Array(arrayStack(0).last)
            val szsnd = snd(0).size
            // modify this - pop
            depth -= 1
            arrayStack = arrayStack.tail ++ Array[Array[HashSet[A]]](null)
            posStack = posStack.tail ++ Array[Int](0)
            // we know that `this` is not empty, since it had something on the arrayStack and arrayStack elements are always non-empty
            ((new TrieIterator[A](snd), szsnd), this)
          } else {
            // 2b) more than a single entry left on top
            val (fst, snd) = arrayStack(0).splitAt(arrayStack(0).length - (arrayStack(0).length - posStack(0) + 1) / 2)
            arrayStack(0) = fst
            val szsnd = snd.foldLeft(0)(_ + _.size)
            ((new TrieIterator[A](snd), szsnd), this)
          }
        } else {
          // 3) no topmost element (arrayD is at the top)
          //    steal a portion of it and update this iterator
          if (posD == arrayD.length - 1) {
            // 3a) positioned at the last element of arrayD
            val arr: Array[HashSet[A]] = arrayD(posD) match {
              case c: HashSetCollision1[a] => collisionToArray(c).asInstanceOf[Array[HashSet[A]]]
              case ht: HashTrieSet[_] => ht.asInstanceOf[HashTrieSet[A]].elems
              case _ => error("cannot divide single element")
            }
            arrayToIterators(arr)
          } else {
            // 3b) arrayD has more free elements
            val (fst, snd) = arrayD.splitAt(arrayD.length - (arrayD.length - posD + 1) / 2)
            arrayD = fst
            val szsnd = snd.foldLeft(0)(_ + _.size)
            ((new TrieIterator[A](snd), szsnd), this)
          }
        }
      }
    }
  }

  @serializable  @SerialVersionUID(2L) private class SerializationProxy[A,B](@transient private var orig: HashSet[A]) {
    private def writeObject(out: java.io.ObjectOutputStream) {
      val s = orig.size
      out.writeInt(s)
      for (e <- orig) {
        out.writeObject(e)
      }
    }

    private def readObject(in: java.io.ObjectInputStream) {
      orig = empty
      val s = in.readInt()
      for (i <- 0 until s) {
        val e = in.readObject().asInstanceOf[A]
        orig = orig + e
      }
    }

    private def readResolve(): AnyRef = orig
  }

}

