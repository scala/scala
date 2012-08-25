/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package immutable

import annotation.unchecked.{ uncheckedVariance => uV }
import generic._
import collection.parallel.immutable.ParHashSet

/** This class implements immutable sets using a hash trie.
 *
 *  '''Note:''' The builder of this hash set may return specialized representations for small sets.
 *
 *  @tparam A      the type of the elements contained in this hash set.
 *
 *  @author  Martin Odersky
 *  @author  Tiark Rompf
 *  @version 2.8
 *  @since   2.3
 *  @define Coll `immutable.HashSet`
 *  @define coll immutable hash set
 */
@SerialVersionUID(2L)
class HashSet[A] extends AbstractSet[A]
                    with Set[A]
                    with GenericSetTemplate[A, HashSet]
                    with SetLike[A, HashSet[A]]
                    with CustomParallelizable[A, ParHashSet[A]]
                    with Serializable
{
  override def companion: GenericCompanion[HashSet] = HashSet

  //class HashSet[A] extends Set[A] with SetLike[A, HashSet[A]] {

  override def par = ParHashSet.fromTrie(this)

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

  protected def elemHashCode(key: A) = key.##

  protected final def improve(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  private[collection] def computeHash(key: A) = improve(elemHashCode(key))

  override def filter(p: (A) => Boolean) = {
    val result = filter0(p, new HashSet.BufferPool())
    if(result eq null) HashSet.empty[A] else result
  }

  protected def filter0(p: A => Boolean, pool:HashSet.BufferPool[A]) = this

  protected def get0(key: A, hash: Int, level: Int): Boolean = false

  def updated0(key: A, hash: Int, level: Int): HashSet[A] =
    new HashSet.HashSet1(key, hash)

  protected def removed0(key: A, hash: Int, level: Int): HashSet[A] = this

  protected def writeReplace(): AnyRef = new HashSet.SerializationProxy(this)

}

/** $factoryInfo
 *  @define Coll `immutable.HashSet`
 *  @define coll immutable hash set
 *
 *  @author  Tiark Rompf
 *  @since   2.3
 *  @define Coll `immutable.HashSet`
 *  @define coll immutable hash set
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
object HashSet extends ImmutableSetFactory[HashSet] {

  /** $setCanBuildFromInfo */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, HashSet[A]] = setCanBuildFrom[A]
  override def empty[A]: HashSet[A] = EmptyHashSet.asInstanceOf[HashSet[A]]

  // helper class for caching buffers for bulk operations
  private final class BufferPool[A] {

    private[this] var depth = 0

    private[this] var buffers : Array[Array[HashSet[A]]] = null

    private[this] def make(buffer:Array[HashSet[A]], length: Int, bitmap: Int, size: Int) =
      if(length==0)
        null
      else {
        if(length==1 && !buffer(0).isInstanceOf[HashTrieSet[_]])
          buffer(0)
        else {
          val elems = new Array[HashSet[A]](length)
          var i=0
          while(i<elems.length) {
            elems(i) = buffer(i)
            i+=1
          }     
          new HashTrieSet(bitmap, elems, size)
        }
      }

    private[this] def allEq(buffer:Array[HashSet[A]], proto:Array[HashSet[A]]) : Boolean = {
      var i = 0
      while(i<proto.length) {
        if(buffer(i) ne proto(i))
          return false
        i+=1
      }
      return true
    }

    def alloc(): Array[HashSet[A]] = {
      if(buffers eq null)
        buffers = new Array[Array[HashSet[A]]](7)
      if(buffers(depth) eq null) {
        // println("alloc "+ depth)
        buffers(depth) = new Array[HashSet[A]](32)
      }
      val result = buffers(depth)
      depth += 1
      result
    }

    def level: Int = depth * 5

    def result(length: Int, bitmap: Int, size: Int) : HashSet[A] = {
      depth -= 1
      make(buffers(depth), length, bitmap, size)
    }

    def result(length: Int, bitmap: Int, size: Int, a: HashTrieSet[A]) = {
      depth -= 1
      val buffer = buffers(depth)
      if(bitmap==a.bitmap && allEq(buffer, a.elems)) {
        // println("reuse " + length + " at depth " + depth)
        a
      }
      else {
        // println("make " + length + " at depth " + depth)
	make(buffer, length, bitmap, size)
      }
    }

    def result(length: Int, bitmap: Int, size: Int, a: HashTrieSet[A], b: HashTrieSet[A]) = {
      depth -= 1
      val buffer = buffers(depth)
      if(bitmap==a.bitmap && allEq(buffer, a.elems))
        a
      else if(bitmap==b.bitmap && allEq(buffer, b.elems))
        b
      else
	make(buffer, length, bitmap, size)
    }
  }

  private object EmptyHashSet extends HashSet[Any] { }

  // TODO: add HashSet2, HashSet3, ...

  class HashSet1[A](private[HashSet] val key: A, private[HashSet] val hash: Int) extends HashSet[A] {
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

    protected override def filter0(p: A => Boolean, pool:BufferPool[A]) : HashSet[A] =
      if(!p(key)) null else this

    override def iterator: Iterator[A] = Iterator(key)
    override def foreach[U](f: A => U): Unit = f(key)
  }

  private[immutable] class HashSetCollision1[A](private[HashSet] val hash: Int, val ks: ListSet[A])
            extends HashSet[A] {

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
        if(ks1.isEmpty)
          HashSet.empty[A]
        else if(ks1.tail.isEmpty)
          new HashSet1(ks1.head, hash)
        else
          new HashSetCollision1(hash, ks1)
      } else this

    protected override def filter0(p: A => Boolean, pool:BufferPool[A]) : HashSet[A] = {
      val ks1 = ks.filter(p)
      if (ks1.isEmpty)
        null
      else if(ks1.tail.isEmpty)
        new HashSet1(ks1.head, hash)
      else
        new HashSetCollision1(hash, ks1)
    }

    override def iterator: Iterator[A] = ks.iterator
    override def foreach[U](f: A => U): Unit = ks.foreach(f)

    private def writeObject(out: java.io.ObjectOutputStream) {
      // this cannot work - reading things in might produce different
      // hash codes and remove the collision. however this is never called
      // because no references to this class are ever handed out to client code
      // and HashTrieSet serialization takes care of the situation
      sys.error("cannot serialize an immutable.HashSet where all items have the same 32-bit hash code")
      //out.writeObject(kvs)
    }

    private def readObject(in: java.io.ObjectInputStream) {
      sys.error("cannot deserialize an immutable.HashSet where all items have the same 32-bit hash code")
      //kvs = in.readObject().asInstanceOf[ListSet[A]]
      //hash = computeHash(kvs.)
    }

  }

  class HashTrieSet[A](private[collection] val bitmap: Int, private[collection] val elems: Array[HashSet[A]], private val size0: Int)
        extends HashSet[A] {

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
        // TODO: might be worth checking if sub is HashTrieSet (-> monomorphic call site)
        val sub = elems(offset)
        val subNew = sub.updated0(key, hash, level + 5)
        if (sub eq subNew) this
        else {
          val elemsNew = new Array[HashSet[A]](elems.length)
          Array.copy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          new HashTrieSet(bitmap, elemsNew, size + (subNew.size - sub.size))
        }
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
        if (sub eq subNew) this
        else if (subNew.isEmpty) {
          val bitmapNew = bitmap ^ mask
          if (bitmapNew != 0) {
            val elemsNew = new Array[HashSet[A]](elems.length - 1)
            Array.copy(elems, 0, elemsNew, 0, offset)
            Array.copy(elems, offset + 1, elemsNew, offset, elems.length - offset - 1)
            val sizeNew = size - sub.size
            // if we have only one child, which is not a HashTrieSet but a self-contained set like
            // HashSet1 or HashSetCollision1, return the child instead
            if (elemsNew.length == 1 && !elemsNew(0).isInstanceOf[HashTrieSet[_]])
              elemsNew(0)
            else
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

    protected override def filter0(p: A => Boolean, pool:BufferPool[A]): HashSet[A] = {
      var offset = 0
      var tgtOffset = 0
      var bitmapNew = 0
      val elemsNew = pool.alloc()
      var sizeNew = 0
      // copy members into local vals
      val elems = this.elems
      val bitmap = this.bitmap
      // iterate over all 32 masks even though just a few of them might be occupied.
      // this iteration is much cheaper than doing calling Integer.bitCount
      var mask = 1
      while (mask != 0) {
        if ((bitmap & mask) != 0) {
          val sub = elems(offset)
          val subNew = sub.filter0(p, pool)
          if (subNew ne null) {
            elemsNew(tgtOffset) = subNew
            sizeNew += subNew.size
            bitmapNew |= mask
            tgtOffset += 1
          }
          // increase the offset for each occupied slot. That way we don't have to invoke Integer.bitCount
          offset += 1
        }
        mask <<= 1
      }

      pool.result(tgtOffset, bitmapNew, sizeNew, this)
/*
      // from here on nothing is written to elemsNew, so it is safe to use elems
      if (elemsNew == null)
        elemsNew = elems

      if (tgtOffset == 0)
      // all subNew were empty
        null
      else if (tgtOffset == 1 && !elemsNew(0).isInstanceOf[HashTrieSet[_]])
      // we don't need a HashTrieSet with one element unless the child is a HashTrieSet as well.
        elemsNew(0)
      else if (tgtOffset == offset)
      // use elemsNew as is, or return this if nothing has changed
        if (elemsNew eq elems) this else new HashTrieSet[A](bitmapNew, elemsNew, sizeNew)
      else {
        // resize elemsNew
        val elemsNew2 = new Array[HashSet[A]](tgtOffset)
        Array.copy(elemsNew, 0, elemsNew2, 0, tgtOffset)
        new HashTrieSet[A](bitmapNew, elemsNew2, sizeNew)
      }*/
    }

    override def iterator = new TrieIterator[A](elems.asInstanceOf[Array[Iterable[A]]]) {
      final override def getElem(cc: AnyRef): A = cc.asInstanceOf[HashSet1[A]].key
    }
/*

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

  @SerialVersionUID(2L) private class SerializationProxy[A,B](@transient private var orig: HashSet[A]) extends Serializable {
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

