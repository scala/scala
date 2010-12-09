package scala.collection.parallel.immutable







import scala.collection.parallel.ParMap
import scala.collection.parallel.ParMapLike
import scala.collection.parallel.Combiner
import scala.collection.parallel.ParIterableIterator
import scala.collection.parallel.EnvironmentPassingCombiner
import scala.collection.parallel.UnrolledBuffer.Unrolled
import scala.collection.parallel.UnrolledBuffer
import scala.collection.generic.ParMapFactory
import scala.collection.generic.CanCombineFrom
import scala.collection.generic.GenericParMapTemplate
import scala.collection.generic.GenericParMapCompanion
import scala.collection.immutable.HashMap


import annotation.unchecked.uncheckedVariance



/** Parallel hash trie map.
 *
 *  @author prokopec
 */
class ParHashMap[K, +V] private[immutable] (private[this] val trie: HashMap[K, V])
extends ParMap[K, V]
   with GenericParMapTemplate[K, V, ParHashMap]
   with ParMapLike[K, V, ParHashMap[K, V], HashMap[K, V]]
{
self =>

  def this() = this(HashMap.empty[K, V])

  override def mapCompanion: GenericParMapCompanion[ParHashMap] = ParHashMap

  override def empty: ParHashMap[K, V] = new ParHashMap[K, V]

  protected[this] override def newCombiner = HashMapCombiner[K, V]

  def parallelIterator: ParIterableIterator[(K, V)] = new ParHashMapIterator(trie.iterator, trie.size) with SCPI

  def seq = trie

  def -(k: K) = new ParHashMap(trie - k)

  def +[U >: V](kv: (K, U)) = new ParHashMap(trie + kv)

  def get(k: K) = trie.get(k)

  override def size = trie.size

  protected override def reuse[S, That](oldc: Option[Combiner[S, That]], newc: Combiner[S, That]) = oldc match {
    case Some(old) => old
    case None => newc
  }

  type SCPI = SignalContextPassingIterator[ParHashMapIterator]

  class ParHashMapIterator(var triter: Iterator[(K, V @uncheckedVariance)], val sz: Int)
  extends super.ParIterator {
  self: SignalContextPassingIterator[ParHashMapIterator] =>
    var i = 0
    def dup = triter match {
      case t: HashMap.TrieIterator[_, _] =>
        val dupt = t.dupIterator.asInstanceOf[Iterator[(K, V)]]
        dupFromIterator(dupt)
      case _ =>
        val buff = triter.toBuffer
        triter = buff.iterator
        dupFromIterator(buff.iterator)
    }
    private def dupFromIterator(it: Iterator[(K, V @uncheckedVariance)]) = {
      val phit = new ParHashMapIterator(it, sz) with SCPI
      phit.i = i
      phit
    }
    def split: Seq[ParIterator] = if (remaining < 2) Seq(this) else triter match {
      case t: HashMap.TrieIterator[_, _] =>
        val previousRemaining = remaining
        val ((fst, fstlength), snd) = t.asInstanceOf[HashMap.TrieIterator[K, V]].split
        val sndlength = previousRemaining - fstlength
        Seq(
          new ParHashMapIterator(fst, fstlength) with SCPI,
          new ParHashMapIterator(snd, sndlength) with SCPI
        )
      case _ =>
        // iterator of the collision map case
        val buff = triter.toBuffer
        val (fp, sp) = buff.splitAt(buff.length / 2)
        Seq(fp, sp) map { b => new ParHashMapIterator(b.iterator, b.length) with SCPI }
    }
    def next: (K, V) = {
      i += 1
      val r = triter.next
      r
    }
    def hasNext: Boolean = {
      i < sz
    }
    def remaining = sz - i
    override def toString = "HashTrieIterator(" + sz + ")"
  }

  private[parallel] def printDebugInfo {
    println("Parallel hash trie")
    println("Top level inner trie type: " + trie.getClass)
    trie match {
      case hm: HashMap.HashMap1[k, v] =>
        println("single node type")
        println("key stored: " + hm.getKey)
        println("hash of key: " + hm.getHash)
        println("computed hash of " + hm.getKey + ": " + hm.computeHashFor(hm.getKey))
        println("trie.get(key): " + hm.get(hm.getKey))
      case _ =>
        println("other kind of node")
    }
  }

}


object ParHashMap extends ParMapFactory[ParHashMap] {
  def empty[K, V]: ParHashMap[K, V] = new ParHashMap[K, V]

  def newCombiner[K, V]: Combiner[(K, V), ParHashMap[K, V]] = HashMapCombiner[K, V]

  implicit def canBuildFrom[K, V]: CanCombineFrom[Coll, (K, V), ParHashMap[K, V]] = {
    new CanCombineFromMap[K, V]
  }

  def fromTrie[K, V](t: HashMap[K, V]) = new ParHashMap(t)

  var totalcombines = new java.util.concurrent.atomic.AtomicInteger(0)
}


private[immutable] abstract class HashMapCombiner[K, V]
extends collection.parallel.BucketCombiner[(K, V), ParHashMap[K, V], (K, V), HashMapCombiner[K, V]](HashMapCombiner.rootsize) {
self: EnvironmentPassingCombiner[(K, V), ParHashMap[K, V]] =>
  import HashMapCombiner._
  import tasksupport._
  val emptyTrie = HashMap.empty[K, V]

  def +=(elem: (K, V)) = {
    sz += 1
    val hc = emptyTrie.computeHash(elem._1)
    val pos = hc & 0x1f
    if (buckets(pos) eq null) {
      // initialize bucket
      buckets(pos) = new UnrolledBuffer[(K, V)]
    }
    // add to bucket
    buckets(pos) += elem
    this
  }

  def result = {
    val bucks = buckets.filter(_ != null).map(_.headPtr)
    val root = new Array[HashMap[K, V]](bucks.length)

    executeAndWaitResult(new CreateTrie(bucks, root, 0, bucks.length))

    var bitmap = 0
    var i = 0
    while (i < rootsize) {
      if (buckets(i) ne null) bitmap |= 1 << i
      i += 1
    }
    val sz = root.foldLeft(0)(_ + _.size)

    if (sz == 0) new ParHashMap[K, V]
    else if (sz == 1) new ParHashMap[K, V](root(0))
    else {
      val trie = new HashMap.HashTrieMap(bitmap, root, sz)
      new ParHashMap[K, V](trie)
    }
  }

  override def toString = {
    "HashTrieCombiner(sz: " + size + ")"
    //"HashTrieCombiner(buckets:\n\t" + buckets.filter(_ != null).mkString("\n\t") + ")\n"
  }

  /* tasks */

  class CreateTrie(bucks: Array[Unrolled[(K, V)]], root: Array[HashMap[K, V]], offset: Int, howmany: Int)
  extends Task[Unit, CreateTrie] {
    var result = ()
    def leaf(prev: Option[Unit]) = {
      var i = offset
      val until = offset + howmany
      while (i < until) {
        root(i) = createTrie(bucks(i))
        i += 1
      }
    }
    private def createTrie(elems: Unrolled[(K, V)]): HashMap[K, V] = {
      var trie = new HashMap[K, V]

      var unrolled = elems
      var i = 0
      while (unrolled ne null) {
        val chunkarr = unrolled.array
        val chunksz = unrolled.size
        while (i < chunksz) {
          val kv = chunkarr(i)
          val hc = trie.computeHash(kv._1)
          trie = trie.updated0(kv._1, hc, rootbits, kv._2, kv, null)
          i += 1
        }
        i = 0
        unrolled = unrolled.next
      }

      trie
    }
    def split = {
      val fp = howmany / 2
      List(new CreateTrie(bucks, root, offset, fp), new CreateTrie(bucks, root, offset + fp, howmany - fp))
    }
    def shouldSplitFurther = howmany > collection.parallel.thresholdFromSize(root.length, parallelismLevel)
  }

}


private[parallel] object HashMapCombiner {
  def apply[K, V] = new HashMapCombiner[K, V] with EnvironmentPassingCombiner[(K, V), ParHashMap[K, V]]

  private[immutable] val rootbits = 5
  private[immutable] val rootsize = 1 << 5
}

















