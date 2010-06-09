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

/** This class implements immutable maps using a hash trie.
 *
 *  '''Note:''' the builder of a hash map returns specialized representations EmptyMap,Map1,..., Map4
 *  for maps of size <= 4.
 *
 *  @tparam A      the type of the keys contained in this hash map.
 *  @tparam B      the type of the values associated with the keys.
 *
 *  @author  Martin Odersky
 *  @author  Tiark Rompf
 *  @version 2.8
 *  @since   2.3
 *  @define Coll immutable.HashMap
 *  @define coll immutable hash map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
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

  protected def elemHashCode(key: A) = if (key == null) 0 else key.##

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

  protected def writeReplace(): AnyRef = new HashMap.SerializationProxy(this)

  def split: Seq[HashMap[A, B]] = Seq(this)

  def combine[B1 >: B](that: HashMap[A, B1]): HashMap[A, B1] = that

}

/** $factoryInfo
 *  @define Coll immutable.HashMap
 *  @define coll immutable hash map
 *
 *  @author  Tiark Rompf
 *  @since   2.3
 */
object HashMap extends ImmutableMapFactory[HashMap] {
  /** $mapCanBuildFromInfo */
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
        if (hash != this.hash) {
          //new HashTrieMap[A,B1](level+5, this, new HashMap1(key, hash, value, kv))
          val m = new HashTrieMap[A,B1](0,new Array[HashMap[A,B1]](0),0) // TODO: could save array alloc
          m.updated0(this.key, this.hash, level, this.value, this.kv).updated0(key, hash, level, value, kv)
        } else {
          // 32-bit hash collision (rare, but not impossible)
          new HashMapCollision1(hash, ListMap.empty.updated(this.key,this.value).updated(key,value))
        }
      }

    override def removed0(key: A, hash: Int, level: Int): HashMap[A, B] =
      if (hash == this.hash && key == this.key) HashMap.empty[A,B] else this

    override def iterator: Iterator[(A,B)] = Iterator(ensurePair)
    override def foreach[U](f: ((A, B)) => U): Unit = f(ensurePair)
    private[HashMap] def ensurePair: (A,B) = if (kv ne null) kv else { kv = (key, value); kv }
    override def combine[B1 >: B](that: HashMap[A, B1]): HashMap[A, B1] = that + (ensurePair)
  }

  private class HashMapCollision1[A,+B](private[HashMap] var hash: Int, var kvs: ListMap[A,B @uncheckedVariance]) extends HashMap[A,B] {
    override def size = kvs.size

    override def get0(key: A, hash: Int, level: Int): Option[B] =
      if (hash == this.hash) kvs.get(key) else None

    override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1)): HashMap[A, B1] =
      if (hash == this.hash) new HashMapCollision1(hash, kvs.updated(key, value))
      else {
        var m: HashMap[A,B1] = new HashTrieMap[A,B1](0,new Array[HashMap[A,B1]](0),0)
        // might be able to save some ops here, but it doesn't seem to be worth it
        for ((k,v) <- kvs)
          m = m.updated0(k, this.hash, level, v, null)
        m.updated0(key, hash, level, value, kv)
      }

    override def removed0(key: A, hash: Int, level: Int): HashMap[A, B] =
      if (hash == this.hash) {
        val kvs1 = kvs - key
        if (!kvs1.isEmpty)
          new HashMapCollision1(hash, kvs1)
        else
          HashMap.empty[A,B]
      } else this

    override def iterator: Iterator[(A,B)] = kvs.iterator
    override def foreach[U](f: ((A, B)) => U): Unit = kvs.foreach(f)
    override def split: Seq[HashMap[A, B]] = {
      val (x, y) = kvs.splitAt(kvs.size / 2)
      def newhm(lm: ListMap[A, B @uncheckedVariance]) = new HashMapCollision1(hash, lm)
      List(newhm(x), newhm(y))
    }
    override def combine[B1 >: B](that: HashMap[A, B1]): HashMap[A, B1] = {
      // TODO this can be made more efficient by passing the entire ListMap at once
      var m = that
      for ((k, v) <- kvs) m = m.updated0(k, this.hash, 0, v, null)
      m
    }
  }

  var dives = 0
  class HashTrieMap[A,+B](private[HashMap] var bitmap: Int, private[HashMap] var elems: Array[HashMap[A,B @uncheckedVariance]],
      private[HashMap] var size0: Int) extends HashMap[A,B] {
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
        // TODO: might be worth checking if sub is HashTrieMap (-> monomorphic call site)
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
        // TODO: might be worth checking if sub is HashTrieMap (-> monomorphic call site)
        val subNew = sub.updated0(key, hash, level + 5, value, kv)
        elemsNew(offset) = subNew
        new HashTrieMap(bitmap, elemsNew, size + (subNew.size - sub.size))
      } else {
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
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        // TODO: might be worth checking if sub is HashTrieMap (-> monomorphic call site)
        val subNew = sub.removed0(key, hash, level + 5)
        if (subNew.isEmpty) {
          val bitmapNew = bitmap ^ mask
          if (bitmapNew != 0) {
            val elemsNew = new Array[HashMap[A,B]](elems.length - 1)
            Array.copy(elems, 0, elemsNew, 0, offset)
            Array.copy(elems, offset + 1, elemsNew, offset, elems.length - offset - 1)
            val sizeNew = size - sub.size
            new HashTrieMap(bitmapNew, elemsNew, sizeNew)
          } else
            HashMap.empty[A,B]
        } else {
          val elemsNew = new Array[HashMap[A,B]](elems.length)
          Array.copy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          val sizeNew = size + (subNew.size - sub.size)
          new HashTrieMap(bitmap, elemsNew, sizeNew)
        }
      } else {
        this
      }
    }

/*
    override def iterator = {   // TODO: optimize (use a stack to keep track of pos)

      def iter(m: HashTrieMap[A,B], k: => Stream[(A,B)]): Stream[(A,B)] = {
        def horiz(elems: Array[HashMap[A,B]], i: Int, k: => Stream[(A,B)]): Stream[(A,B)] = {
          if (i < elems.length) {
            elems(i) match {
              case m: HashTrieMap[A,B] => iter(m, horiz(elems, i+1, k))
              case m: HashMap1[A,B] => new Stream.Cons(m.ensurePair, horiz(elems, i+1, k))
            }
          } else k
        }
        horiz(m.elems, 0, k)
      }
      iter(this, Stream.empty).iterator
    }
*/


    override def iterator = new Iterator[(A,B)] {
      private[this] var depth = 0
      private[this] var arrayStack = new Array[Array[HashMap[A,B]]](6)
      private[this] var posStack = new Array[Int](6)

      private[this] var arrayD = elems
      private[this] var posD = 0

      private[this] var subIter: Iterator[(A,B)] = null // to traverse collision nodes

      def hasNext = (subIter ne null) || depth >= 0

      def next: (A,B) = {
        if (subIter ne null) {
          val el = subIter.next
          if (!subIter.hasNext)
            subIter = null
          el
        } else
          next0(arrayD, posD)
      }

      @scala.annotation.tailrec private[this] def next0(elems: Array[HashMap[A,B]], i: Int): (A,B) = {
        if (i == elems.length-1) { // reached end of level, pop stack
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
          case m: HashTrieMap[A,B] => // push current pos onto stack and descend
            if (depth >= 0) {
              arrayStack(depth) = arrayD
              posStack(depth) = posD
            }
            depth += 1
            arrayD = m.elems
            posD = 0
            next0(m.elems, 0)
          case m: HashMap1[A,B] => m.ensurePair
          case m =>
            subIter = m.iterator
            subIter.next
        }
      }
    }

/*

import collection.immutable._
def time(block: =>Unit) = { val t0 = System.nanoTime; block; println("elapsed: " + (System.nanoTime - t0)/1000000.0) }
var mOld = OldHashMap.empty[Int,Int]
var mNew = HashMap.empty[Int,Int]
time { for (i <- 0 until 100000) mOld = mOld.updated(i,i) }
time { for (i <- 0 until 100000) mOld = mOld.updated(i,i) }
time { for (i <- 0 until 100000) mOld = mOld.updated(i,i) }
time { for (i <- 0 until 100000) mNew = mNew.updated(i,i) }
time { for (i <- 0 until 100000) mNew = mNew.updated(i,i) }
time { for (i <- 0 until 100000) mNew = mNew.updated(i,i) }
time { mOld.iterator.foreach( p => ()) }
time { mOld.iterator.foreach( p => ()) }
time { mOld.iterator.foreach( p => ()) }
time { mNew.iterator.foreach( p => ()) }
time { mNew.iterator.foreach( p => ()) }
time { mNew.iterator.foreach( p => ()) }

*/


    override def foreach[U](f: ((A, B)) =>  U): Unit = {
      var i = 0;
      while (i < elems.length) {
        elems(i).foreach(f)
        i += 1
      }
    }

    private def printBitmap(bm: Int) {
      var i = 31
      var b = bm
      while (i != 0) {
	print((b & 1) + " ")
	b = b >>> 1
	i -= 1
      }
      println
    }

    private def subtreeCount(bm: Int) = {
      var c = 0
      var b = bm
      while (b != 0) {
	if ((b & 1) != 0) c += 1
	b = b >>> 1
      }
      c
    }

    private def posOf(n: Int, bm: Int) = {
      var left = n
      var i = -1
      var b = bm
      while (left >= 0) {
	i += 1
	if ((b & 1) != 0) left -= 1
	b = b >>> 1
      }
      i
    }

    override def split: Seq[HashMap[A, B]] = {
      // printBitmap(bitmap)
      // println(elems.toList)

      // println("subtrees: " + subtreeCount(bitmap))
      // println("will split at: " + posOf(subtreeCount(bitmap) / 2, bitmap))
      val splitpoint = posOf(subtreeCount(bitmap) / 2, bitmap)
      val bm1 = bitmap & (-1 << splitpoint)
      val bm2 = bitmap & (-1 >>> (32 - splitpoint))
      // printBitmap(bm1)
      // printBitmap(bm2)
      val (e1, e2) = elems.splitAt(splitpoint)
      // println(e1.toList)
      // println(e2.toList)
      val hm1 = new HashTrieMap(bm1, e1, e1.foldLeft(0)(_ + _.size))
      val hm2 = new HashTrieMap(bm2, e2, e2.foldLeft(0)(_ + _.size))

      List(hm1, hm2)
    }

    override def combine[B1 >: B](that: HashMap[A, B1]): HashMap[A, B1] = that match {
      case hm: HashMap1[_, _] => this ++ hm.asInstanceOf[HashMap[A, B1]]
      case hm: HashMapCollision1[_, _] => this ++ hm.asInstanceOf[HashMap[A, B1]]
      case hm: HashTrieMap[_, _] =>
        val that = hm.asInstanceOf[HashTrieMap[A, B1]]
        val thiselems = this.elems
        val thatelems = that.elems
        val thisbm = this.bitmap
        val thatbm = that.bitmap
        val combinedbitmap = thisbm | thatbm
        val bothbitmap = thisbm & thatbm

        // determine the necessary size for the array
	val subcount = subtreeCount(combinedbitmap)
        //printBitmap(combinedbitmap)
        //println(subcount)

	// construct a new array of appropriate size
        val combined = new Array[HashMap[A, B1 @uncheckedVariance]](subcount)

	// run through both bitmaps and add elements to it
        var pos = 1
        var i = 0
        var thisi = 0
        var thati = 0
        while (pos != 0) {
          if ((bothbitmap & pos) != 0) {
            combined(i) = thiselems(thisi) combine thatelems(thati)
            dives += 1
            i += 1
            thisi += 1
            thati += 1
          } else if ((thisbm & pos) != 0) {
            combined(i) = thiselems(thisi)
            i += 1
            thisi += 1
          } else if ((thatbm & pos) != 0) {
            combined(i) = thatelems(thati)
            i += 1
            thati += 1
          }
          pos = pos << 1
        }
        //println(combined.toList)

	new HashTrieMap[A, B1](combinedbitmap, combined, this.size + that.size)
      case empty: HashMap[_, _] => this
      case _ => error("section supposed to be unreachable.")
    }

  }

  @serializable  @SerialVersionUID(2L) private class SerializationProxy[A,B](@transient private var orig: HashMap[A, B]) {
    private def writeObject(out: java.io.ObjectOutputStream) {
      val s = orig.size
      out.writeInt(s)
      for ((k,v) <- orig) {
        out.writeObject(k)
        out.writeObject(v)
      }
    }

    private def readObject(in: java.io.ObjectInputStream) {
      orig = empty
      val s = in.readInt()
      for (i <- 0 until s) {
        val key = in.readObject().asInstanceOf[A]
        val value = in.readObject().asInstanceOf[B]
        orig = orig.updated(key, value)
      }
    }

    private def readResolve(): AnyRef = orig
  }

}

