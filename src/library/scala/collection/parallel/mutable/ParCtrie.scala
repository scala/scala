/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2012, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection.parallel.mutable



import scala.collection.generic._
import scala.collection.parallel.Combiner
import scala.collection.parallel.IterableSplitter
import scala.collection.mutable.BasicNode
import scala.collection.mutable.TNode
import scala.collection.mutable.LNode
import scala.collection.mutable.CNode
import scala.collection.mutable.SNode
import scala.collection.mutable.INode
import scala.collection.mutable.Ctrie
import scala.collection.mutable.CtrieIterator



/** Parallel Ctrie collection.
 *  
 *  It has its bulk operations parallelized, but uses the snapshot operation
 *  to create the splitter. This means that parallel bulk operations can be
 *  called concurrently with the modifications.
 *  
 *  @author Aleksandar Prokopec
 *  @since 2.10
 */
final class ParCtrie[K, V] private[collection] (private val ctrie: Ctrie[K, V])
extends ParMap[K, V]
   with GenericParMapTemplate[K, V, ParCtrie]
   with ParMapLike[K, V, ParCtrie[K, V], Ctrie[K, V]]
   with ParCtrieCombiner[K, V]
   with Serializable
{
  import collection.parallel.tasksupport._
  
  def this() = this(new Ctrie)
  
  override def mapCompanion: GenericParMapCompanion[ParCtrie] = ParCtrie
  
  override def empty: ParCtrie[K, V] = ParCtrie.empty
  
  protected[this] override def newCombiner = ParCtrie.newCombiner
  
  override def seq = ctrie
  
  def splitter = new ParCtrieSplitter(0, ctrie.readOnlySnapshot().asInstanceOf[Ctrie[K, V]], true)
  
  override def clear() = ctrie.clear()
  
  def result = this
  
  def get(key: K): Option[V] = ctrie.get(key)
  
  def put(key: K, value: V): Option[V] = ctrie.put(key, value)
  
  def update(key: K, value: V): Unit = ctrie.update(key, value)
  
  def remove(key: K): Option[V] = ctrie.remove(key)
  
  def +=(kv: (K, V)): this.type = {
    ctrie.+=(kv)
    this
  }
  
  def -=(key: K): this.type = {
    ctrie.-=(key)
    this
  }
  
  override def size = {
    val in = ctrie.readRoot()
    val r = in.gcasRead(ctrie)
    r match {
      case tn: TNode[_, _] => tn.cachedSize(ctrie)
      case ln: LNode[_, _] => ln.cachedSize(ctrie)
      case cn: CNode[_, _] =>
        executeAndWaitResult(new Size(0, cn.array.length, cn.array))
        cn.cachedSize(ctrie)
    }
  }
  
  override def stringPrefix = "ParCtrie"
  
  /* tasks */
  
  /** Computes Ctrie size in parallel. */
  class Size(offset: Int, howmany: Int, array: Array[BasicNode]) extends Task[Int, Size] {
    var result = -1
    def leaf(prev: Option[Int]) = {
      var sz = 0
      var i = offset
      val until = offset + howmany
      while (i < until) {
        array(i) match {
          case sn: SNode[_, _] => sz += 1
          case in: INode[K, V] => sz += in.cachedSize(ctrie)
        }
        i += 1
      }
      result = sz
    }
    def split = {
      val fp = howmany / 2
      Seq(new Size(offset, fp, array), new Size(offset + fp, howmany - fp, array))
    }
    def shouldSplitFurther = howmany > 1
    override def merge(that: Size) = result = result + that.result
  }
  
}


private[collection] class ParCtrieSplitter[K, V](lev: Int, ct: Ctrie[K, V], mustInit: Boolean)
extends CtrieIterator[K, V](lev, ct, mustInit)
   with IterableSplitter[(K, V)]
{
  // only evaluated if `remaining` is invoked (which is not used by most tasks)
  lazy val totalsize = ct.par.size
  var iterated = 0
  
  protected override def newIterator(_lev: Int, _ct: Ctrie[K, V], _mustInit: Boolean) = new ParCtrieSplitter[K, V](_lev, _ct, _mustInit)
  
  override def shouldSplitFurther[S](coll: collection.parallel.ParIterable[S], parallelismLevel: Int) = {
    val maxsplits = 3 + Integer.highestOneBit(parallelismLevel)
    level < maxsplits
  }
  
  def dup = {
    val it = newIterator(0, ct, false)
    dupTo(it)
    it.iterated = this.iterated
    it
  }
  
  override def next() = {
    iterated += 1
    super.next()
  }
  
  def split: Seq[IterableSplitter[(K, V)]] = subdivide().asInstanceOf[Seq[IterableSplitter[(K, V)]]]
  
  override def isRemainingCheap = false
  
  def remaining: Int = totalsize - iterated
}


/** Only used within the `ParCtrie`. */
private[mutable] trait ParCtrieCombiner[K, V] extends Combiner[(K, V), ParCtrie[K, V]] {
  
  def combine[N <: (K, V), NewTo >: ParCtrie[K, V]](other: Combiner[N, NewTo]): Combiner[N, NewTo] = if (this eq other) this else {
    throw new UnsupportedOperationException("This shouldn't have been called in the first place.")
    
    val thiz = this.asInstanceOf[ParCtrie[K, V]]
    val that = other.asInstanceOf[ParCtrie[K, V]]
    val result = new ParCtrie[K, V]
    
    result ++= thiz.iterator
    result ++= that.iterator
    
    result
  }
  
  override def canBeShared = true
  
}

  
object ParCtrie extends ParMapFactory[ParCtrie] {
  
  def empty[K, V]: ParCtrie[K, V] = new ParCtrie[K, V]
  
  def newCombiner[K, V]: Combiner[(K, V), ParCtrie[K, V]] = new ParCtrie[K, V]
  
  implicit def canBuildFrom[K, V]: CanCombineFrom[Coll, (K, V), ParCtrie[K, V]] = new CanCombineFromMap[K, V]
  
}








