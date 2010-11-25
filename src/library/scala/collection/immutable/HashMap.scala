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


import parallel.immutable.ParHashMap


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
class HashMap[A, +B] extends Map[A,B] with MapLike[A, B, HashMap[A, B]] with Parallelizable[ParHashMap[A, B]] {

  override def size: Int = 0

  override def empty = HashMap.empty[A, B]

  def iterator: Iterator[(A,B)] = Iterator.empty

  override def foreach[U](f: ((A, B)) =>  U): Unit = { }

  def get(key: A): Option[B] =
    get0(key, computeHash(key), 0)

  override def updated [B1 >: B] (key: A, value: B1): HashMap[A, B1] =
    updated0(key, computeHash(key), 0, value, null, null)

  override def + [B1 >: B] (kv: (A, B1)): HashMap[A, B1] =
    updated0(kv._1, computeHash(kv._1), 0, kv._2, kv, null)

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

  private[collection] def computeHash(key: A) = improve(elemHashCode(key))

  protected type Merger[B1] = ((A, B1), (A, B1)) => (A, B1)

  protected def get0(key: A, hash: Int, level: Int): Option[B] = None

  private[collection] def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[B1]): HashMap[A, B1] =
    new HashMap.HashMap1(key, hash, value, kv)

  protected def removed0(key: A, hash: Int, level: Int): HashMap[A, B] = this

  protected def writeReplace(): AnyRef = new HashMap.SerializationProxy(this)

  def split: Seq[HashMap[A, B]] = Seq(this)

  def merge[B1 >: B](that: HashMap[A, B1], merger: Merger[B1] = null): HashMap[A, B1] = merge0(that, 0, merger)

  protected def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[B1]): HashMap[A, B1] = that

  def par = ParHashMap.fromTrie(this)

  override def toParIterable = par

  private type C = (A, B)
  override def toParMap[D, E](implicit ev: C <:< (D, E)) = par.asInstanceOf[ParHashMap[D, E]]

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

  class HashMap1[A,+B](private[HashMap] var key: A, private[HashMap] var hash: Int, private[HashMap] var value: (B @uncheckedVariance), private[HashMap] var kv: (A,B @uncheckedVariance)) extends HashMap[A,B] {
    override def size = 1

    private[collection] def getKey = key
    private[collection] def getHash = hash
    private[collection] def computeHashFor(k: A) = computeHash(k)

    override def get0(key: A, hash: Int, level: Int): Option[B] =
      if (hash == this.hash && key == this.key) Some(value) else None

    // override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1)): HashMap[A, B1] =
    //   if (hash == this.hash && key == this.key) new HashMap1(key, hash, value, kv)
    //   else {
    //     var thatindex = (hash >>> level) & 0x1f
    //     var thisindex = (this.hash >>> level) & 0x1f
    //     if (hash != this.hash) {
    //       --new HashTrieMap[A,B1](level+5, this, new HashMap1(key, hash, value, kv))
    //       val m = new HashTrieMap[A,B1](0,new Array[HashMap[A,B1]](0),0) // TODO: could save array alloc
    //       m.updated0(this.key, this.hash, level, this.value, this.kv).updated0(key, hash, level, value, kv)  TODO and it will
    //     } else {
    //        32-bit hash collision (rare, but not impossible)
    //       new HashMapCollision1(hash, ListMap.empty.updated(this.key,this.value).updated(key,value))
    //     }
    //   }

    override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[B1]): HashMap[A, B1] =
      if (hash == this.hash && key == this.key ) {
        if (merger eq null) new HashMap1(key, hash, value, kv)
        else new HashMap1(key, hash, value, merger(this.kv, kv))
      } else {
        var thatindex = (hash >>> level) & 0x1f
        var thisindex = (this.hash >>> level) & 0x1f
        if (hash != this.hash) {
          // they have different hashes, but may collide at this level - find a level at which they don't
          var lvl = level
          var top: HashTrieMap[A, B1] = null
          var prev: HashTrieMap[A, B1] = null
          while (thisindex == thatindex) {
            val newlevel = new HashTrieMap[A, B1](1 << thisindex, new Array[HashMap[A, B1]](1), 2)
            if (prev ne null) prev.elems(0) = newlevel else top = newlevel
            prev = newlevel
            lvl += 5
            thatindex = (hash >>> lvl) & 0x1f
            thisindex = (this.hash >>> lvl) & 0x1f
          }
          val bottelems = new Array[HashMap[A,B1]](2)
          val ind = if (thisindex < thatindex) 1 else 0
          bottelems(1 - ind) = this
          bottelems(ind) = new HashMap1[A, B1](key, hash, value, kv)
          val bottom = new HashTrieMap[A,B1]((1 << thisindex) | (1 << thatindex), bottelems, 2)
          if (prev ne null) {
            prev.elems(0) = bottom
            top
          } else bottom
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
    protected override def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[B1]): HashMap[A, B1] = {
      that.updated0(key, hash, level, value, kv, merger)
    }
  }

  private class HashMapCollision1[A,+B](private[HashMap] var hash: Int, var kvs: ListMap[A,B @uncheckedVariance]) extends HashMap[A,B] {
    override def size = kvs.size

    override def get0(key: A, hash: Int, level: Int): Option[B] =
      if (hash == this.hash) kvs.get(key) else None

    override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[B1]): HashMap[A, B1] =
      if (hash == this.hash) {
        if ((merger eq null) || !kvs.contains(key)) new HashMapCollision1(hash, kvs.updated(key, value))
        else new HashMapCollision1(hash, kvs + merger((key, kvs(key)), kv))
      } else {
        var m: HashMap[A,B1] = new HashTrieMap[A,B1](0,new Array[HashMap[A,B1]](0),0)
        // might be able to save some ops here, but it doesn't seem to be worth it
        for ((k,v) <- kvs)
          m = m.updated0(k, this.hash, level, v, null, merger)
        m.updated0(key, hash, level, value, kv, merger)
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
    protected override def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[B1]): HashMap[A, B1] = {
      // this can be made more efficient by passing the entire ListMap at once
      var m = that
      for (p <- kvs) m = m.updated0(p._1, this.hash, level, p._2, p, merger)
      m
    }
  }

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

    override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[B1]): HashMap[A, B1] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask-1))
      if ((bitmap & mask) != 0) {
        val elemsNew = new Array[HashMap[A,B1]](elems.length)
        Array.copy(elems, 0, elemsNew, 0, elems.length)
        val sub = elems(offset)
        // TODO: might be worth checking if sub is HashTrieMap (-> monomorphic call site)
        val subNew = sub.updated0(key, hash, level + 5, value, kv, merger)
        elemsNew(offset) = subNew
        new HashTrieMap(bitmap, elemsNew, size + (subNew.size - sub.size))
      } else {
        val elemsNew = new Array[HashMap[A,B1]](elems.length + 1)
        Array.copy(elems, 0, elemsNew, 0, offset)
        elemsNew(offset) = new HashMap1(key, hash, value, kv)
        Array.copy(elems, offset, elemsNew, offset + 1, elems.length - offset)
        new HashTrieMap(bitmap | mask, elemsNew, size + 1)
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

    override def iterator = new TrieIterator[A, B](elems)

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
      var i = 32
      var b = bm
      while (i != 0) {
	print((b & 1) + " ")
	b = b >>> 1
	i -= 1
      }
      println
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

    override def split: Seq[HashMap[A, B]] = if (size == 1) Seq(this) else {
      val nodesize = Integer.bitCount(bitmap)
      if (nodesize > 1) {
        // printBitmap(bitmap)
        // println(elems.toList)

        // println("subtrees: " + nodesize)
        // println("will split at: " + (nodesize / 2))
        val splitpoint = nodesize / 2
        val bitsplitpoint = posOf(nodesize / 2, bitmap)
        val bm1 = bitmap & (-1 << bitsplitpoint)
        val bm2 = bitmap & (-1 >>> (32 - bitsplitpoint))
        // printBitmap(bm1)
        // printBitmap(bm2)
        val (e1, e2) = elems.splitAt(splitpoint)
        // println(e1.toList)
        // println(e2.toList)
        val hm1 = new HashTrieMap(bm1, e1, e1.foldLeft(0)(_ + _.size))
        val hm2 = new HashTrieMap(bm2, e2, e2.foldLeft(0)(_ + _.size))

        List(hm1, hm2)
      } else elems(0).split
    }

    protected override def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[B1]): HashMap[A, B1] = that match {
      case hm: HashMap1[_, _] =>
        // onetrie += 1
        this.updated0(hm.key, hm.hash, level, hm.value.asInstanceOf[B1], hm.kv, merger)
      case hm: HashTrieMap[_, _] =>
        // bothtries += 1
        val that = hm.asInstanceOf[HashTrieMap[A, B1]]
        val thiselems = this.elems
        val thatelems = that.elems
        var thisbm = this.bitmap
        var thatbm = that.bitmap

        // determine the necessary size for the array
	val subcount = Integer.bitCount(thisbm | thatbm)

        // construct a new array of appropriate size
        val merged = new Array[HashMap[A, B1]](subcount)

	// run through both bitmaps and add elements to it
        var i = 0
        var thisi = 0
        var thati = 0
        var totalelems = 0
        while (i < subcount) {
          val thislsb = thisbm ^ (thisbm & (thisbm - 1))
          val thatlsb = thatbm ^ (thatbm & (thatbm - 1))
          // if (this.bitmap == -1660585213) { TODO remove
          //   printBitmap(thislsb)
          //   printBitmap(thatlsb)
          //   println("------------------")
          // }
          if (thislsb == thatlsb) {
            // println("a collision")
            val m = thiselems(thisi).merge0(thatelems(thati), level + 5, merger)
            totalelems += m.size
            merged(i) = m
            thisbm = thisbm & ~thislsb
            thatbm = thatbm & ~thatlsb
            thati += 1
            thisi += 1
          } else {
            // condition below is due to 2 things:
            // 1) no unsigned int compare on JVM
            // 2) 0 (no lsb) should always be greater in comparison
            // also, search for unsigned compare Scala to find Dave's solution
            // and compare a and b defined as below:
            val a = thislsb - 1
            val b = thatlsb - 1
            // ! our case indeed is more specific, but this didn't help:
            // if ((thislsb > 0 && thislsb < thatlsb) || thatlsb == 0 || (thatlsb < 0 && thislsb != 0)) {
            if ((a < b) ^ (a < 0) ^ (b < 0)) {
              // println("an element from this trie")
              val m = thiselems(thisi)
              totalelems += m.size
              merged(i) = m
              thisbm = thisbm & ~thislsb
              thisi += 1
            } else {
              // println("an element from that trie")
              val m = thatelems(thati)
              totalelems += m.size
              merged(i) = m
              thatbm = thatbm & ~thatlsb
              thati += 1
            }
          }
          i += 1
        }

        new HashTrieMap[A, B1](this.bitmap | that.bitmap, merged, totalelems)
      case hm: HashMapCollision1[_, _] => that.merge0(this, level, merger)
      case hm: HashMap[_, _] => this
      case _ => error("section supposed to be unreachable.")
    }

  }

  class TrieIterator[A, +B](elems: Array[HashMap[A, B]]) extends Iterator[(A, B)] {
    private[this] var depth = 0
    private[this] var arrayStack = new Array[Array[HashMap[A,B]]](6)
    private[this] var posStack = new Array[Int](6)

    private[this] var arrayD = elems
    private[this] var posD = 0

    private[this] var subIter: Iterator[(A, B)] = null // to traverse collision nodes

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

    // assumption: contains 2 or more elements
    // splits this iterator into 2 iterators
    // returns the 1st iterator, its number of elements, and the second iterator
    def split: ((Iterator[(A, B)], Int), Iterator[(A, B)]) = {
      def collisionToArray(c: HashMapCollision1[_, _]) =
        c.asInstanceOf[HashMapCollision1[A, B]].kvs.toArray map { HashMap() + _ }
      def arrayToIterators(arr: Array[HashMap[A, B]]) = {
        val (fst, snd) = arr.splitAt(arr.length / 2)
        val szsnd = snd.foldLeft(0)(_ + _.size)
        ((new TrieIterator(snd), szsnd), new TrieIterator(fst))
      }
      def splitArray(ad: Array[HashMap[A, B]]): ((Iterator[(A, B)], Int), Iterator[(A, B)]) = if (ad.length > 1) {
        arrayToIterators(ad)
      } else ad(0) match {
        case c: HashMapCollision1[a, b] => arrayToIterators(collisionToArray(c.asInstanceOf[HashMapCollision1[A, B]]))
        case hm: HashTrieMap[a, b] => splitArray(hm.elems.asInstanceOf[Array[HashMap[A, B]]])
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
            arrayStack = arrayStack.tail ++ Array[Array[HashMap[A, B]]](null)
            posStack = posStack.tail ++ Array[Int](0)
            // we know that `this` is not empty, since it had something on the arrayStack and arrayStack elements are always non-empty
            ((new TrieIterator[A, B](snd), szsnd), this)
          } else {
            // 2b) more than a single entry left on top
            val (fst, snd) = arrayStack(0).splitAt(arrayStack(0).length - (arrayStack(0).length - posStack(0) + 1) / 2)
            arrayStack(0) = fst
            val szsnd = snd.foldLeft(0)(_ + _.size)
            ((new TrieIterator[A, B](snd), szsnd), this)
          }
        } else {
          // 3) no topmost element (arrayD is at the top)
          //    steal a portion of it and update this iterator
          if (posD == arrayD.length - 1) {
            // 3a) positioned at the last element of arrayD
            val arr: Array[HashMap[A, B]] = arrayD(posD) match {
              case c: HashMapCollision1[a, b] => collisionToArray(c).asInstanceOf[Array[HashMap[A, B]]]
              case ht: HashTrieMap[_, _] => ht.asInstanceOf[HashTrieMap[A, B]].elems
              case _ => error("cannot divide single element")
            }
            arrayToIterators(arr)
          } else {
            // 3b) arrayD has more free elements
            val (fst, snd) = arrayD.splitAt(arrayD.length - (arrayD.length - posD + 1) / 2)
            arrayD = fst
            val szsnd = snd.foldLeft(0)(_ + _.size)
            ((new TrieIterator[A, B](snd), szsnd), this)
          }
        }
      }
    }
  }

  private def check[K](x: HashMap[K, _], y: HashMap[K, _], xy: HashMap[K, _]) = { // TODO remove this debugging helper
    var xs = Set[K]()
    for (elem <- x) xs += elem._1
    var ys = Set[K]()
    for (elem <- y) ys += elem._1
    var union = Set[K]()
    for (elem <- xy) union += elem._1
    if ((xs ++ ys) != union) {
      println("Error.")
      println(x.getClass)
      println(y.getClass)
      println(xs)
      println(ys)
      println(xs ++ ys)
      println(union)
      false
    } else true
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

