/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2012, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package mutable



import java.util.concurrent.atomic._
import collection.immutable.{ ListMap => ImmutableListMap }
import collection.parallel.mutable.ParCtrie
import generic._
import annotation.tailrec
import annotation.switch



private[collection] final class INode[K, V](bn: MainNode[K, V], g: Gen) extends INodeBase[K, V](g) {
  import INodeBase._
  
  WRITE(bn)
  
  def this(g: Gen) = this(null, g)
  
  @inline final def WRITE(nval: MainNode[K, V]) = INodeBase.updater.set(this, nval)
  
  @inline final def CAS(old: MainNode[K, V], n: MainNode[K, V]) = INodeBase.updater.compareAndSet(this, old, n)
  
  final def gcasRead(ct: Ctrie[K, V]): MainNode[K, V] = GCAS_READ(ct)
  
  @inline final def GCAS_READ(ct: Ctrie[K, V]): MainNode[K, V] = {
    val m = /*READ*/mainnode
    val prevval = /*READ*/m.prev
    if (prevval eq null) m
    else GCAS_Complete(m, ct)
  }
    
  @tailrec private def GCAS_Complete(m: MainNode[K, V], ct: Ctrie[K, V]): MainNode[K, V] = if (m eq null) null else {
    // complete the GCAS
    val prev = /*READ*/m.prev
    val ctr = ct.readRoot(true)
    
    prev match {
      case null =>
        m
      case fn: FailedNode[_, _] => // try to commit to previous value
        if (CAS(m, fn.prev)) fn.prev
        else GCAS_Complete(/*READ*/mainnode, ct)
      case vn: MainNode[_, _] =>
        // Assume that you've read the root from the generation G.
        // Assume that the snapshot algorithm is correct.
        // ==> you can only reach nodes in generations <= G.
        // ==> `gen` is <= G.
        // We know that `ctr.gen` is >= G.
        // ==> if `ctr.gen` = `gen` then they are both equal to G.
        // ==> otherwise, we know that either `ctr.gen` > G, `gen` < G,
        //     or both
        if ((ctr.gen eq gen) && ct.nonReadOnly) {
          // try to commit
          if (m.CAS_PREV(prev, null)) m
          else GCAS_Complete(m, ct)
        } else {
          // try to abort
          m.CAS_PREV(prev, new FailedNode(prev))
          GCAS_Complete(/*READ*/mainnode, ct)
        }
    }
  }
  
  @inline final def GCAS(old: MainNode[K, V], n: MainNode[K, V], ct: Ctrie[K, V]): Boolean = {
    n.WRITE_PREV(old)
    if (CAS(old, n)) {
      GCAS_Complete(n, ct)
      /*READ*/n.prev eq null
    } else false
  }
  
  @inline private def inode(cn: MainNode[K, V]) = {
    val nin = new INode[K, V](gen)
    nin.WRITE(cn)
    nin
  }
  
  final def copyToGen(ngen: Gen, ct: Ctrie[K, V]) = {
    val nin = new INode[K, V](ngen)
    val main = GCAS_READ(ct)
    nin.WRITE(main)
    nin
  }
  
  /** Inserts a key value pair, overwriting the old pair if the keys match.
   *  
   *  @return        true if successful, false otherwise
   */
  @tailrec final def rec_insert(k: K, v: V, hc: Int, lev: Int, parent: INode[K, V], startgen: Gen, ct: Ctrie[K, V]): Boolean = {
    val m = GCAS_READ(ct) // use -Yinline!
    
    m match {
      case cn: CNode[K, V] => // 1) a multiway node
        val idx = (hc >>> lev) & 0x1f
        val flag = 1 << idx
        val bmp = cn.bitmap
        val mask = flag - 1
        val pos = Integer.bitCount(bmp & mask)
        if ((bmp & flag) != 0) {
          // 1a) insert below
          cn.array(pos) match {
            case in: INode[K, V] =>
              if (startgen eq in.gen) in.rec_insert(k, v, hc, lev + 5, this, startgen, ct)
              else {
                if (GCAS(cn, cn.renewed(startgen, ct), ct)) rec_insert(k, v, hc, lev, parent, startgen, ct)
                else false
              }
            case sn: SNode[K, V] =>
              if (sn.hc == hc && sn.k == k) GCAS(cn, cn.updatedAt(pos, new SNode(k, v, hc), gen), ct)
              else {
                val rn = if (cn.gen eq gen) cn else cn.renewed(gen, ct)
                val nn = rn.updatedAt(pos, inode(CNode.dual(sn, sn.hc, new SNode(k, v, hc), hc, lev + 5, gen)), gen)
                GCAS(cn, nn, ct)
              }
          }
        } else {
          val rn = if (cn.gen eq gen) cn else cn.renewed(gen, ct)
          val ncnode = rn.insertedAt(pos, flag, new SNode(k, v, hc), gen)
          GCAS(cn, ncnode, ct)
        }
      case tn: TNode[K, V] =>
        clean(parent, ct, lev - 5)
        false
      case ln: LNode[K, V] => // 3) an l-node
        val nn = ln.inserted(k, v)
        GCAS(ln, nn, ct)
    }
  }
  
  /** Inserts a new key value pair, given that a specific condition is met.
   *
   *  @param cond        null - don't care if the key was there; KEY_ABSENT - key wasn't there; KEY_PRESENT - key was there; other value `v` - key must be bound to `v`
   *  @return            null if unsuccessful, Option[V] otherwise (indicating previous value bound to the key)
   */
  @tailrec final def rec_insertif(k: K, v: V, hc: Int, cond: AnyRef, lev: Int, parent: INode[K, V], startgen: Gen, ct: Ctrie[K, V]): Option[V] = {
    val m = GCAS_READ(ct)  // use -Yinline!
    
    m match {
      case cn: CNode[K, V] => // 1) a multiway node
        val idx = (hc >>> lev) & 0x1f
        val flag = 1 << idx
        val bmp = cn.bitmap
        val mask = flag - 1
        val pos = Integer.bitCount(bmp & mask)
        if ((bmp & flag) != 0) {
          // 1a) insert below
          cn.array(pos) match {
            case in: INode[K, V] =>
              if (startgen eq in.gen) in.rec_insertif(k, v, hc, cond, lev + 5, this, startgen, ct)
              else {
                if (GCAS(cn, cn.renewed(startgen, ct), ct)) rec_insertif(k, v, hc, cond, lev, parent, startgen, ct)
                else null
              }
            case sn: SNode[K, V] => cond match {
              case null =>
                if (sn.hc == hc && sn.k == k) {
                  if (GCAS(cn, cn.updatedAt(pos, new SNode(k, v, hc), gen), ct)) Some(sn.v) else null
                } else {
                  val rn = if (cn.gen eq gen) cn else cn.renewed(gen, ct)
                  val nn = rn.updatedAt(pos, inode(CNode.dual(sn, sn.hc, new SNode(k, v, hc), hc, lev + 5, gen)), gen)
                  if (GCAS(cn, nn, ct)) None
                  else null
                }
              case INode.KEY_ABSENT =>
                if (sn.hc == hc && sn.k == k) Some(sn.v)
                else {
                  val rn = if (cn.gen eq gen) cn else cn.renewed(gen, ct)
                  val nn = rn.updatedAt(pos, inode(CNode.dual(sn, sn.hc, new SNode(k, v, hc), hc, lev + 5, gen)), gen)
                  if (GCAS(cn, nn, ct)) None
                  else null
                }
              case INode.KEY_PRESENT =>
                if (sn.hc == hc && sn.k == k) {
                  if (GCAS(cn, cn.updatedAt(pos, new SNode(k, v, hc), gen), ct)) Some(sn.v) else null
                } else None
              case otherv: V =>
                if (sn.hc == hc && sn.k == k && sn.v == otherv) {
                  if (GCAS(cn, cn.updatedAt(pos, new SNode(k, v, hc), gen), ct)) Some(sn.v) else null
                } else None
            }
          }
        } else cond match {
          case null | INode.KEY_ABSENT =>
            val rn = if (cn.gen eq gen) cn else cn.renewed(gen, ct)
            val ncnode = rn.insertedAt(pos, flag, new SNode(k, v, hc), gen)
            if (GCAS(cn, ncnode, ct)) None else null
          case INode.KEY_PRESENT => None
          case otherv: V => None
        }
      case sn: TNode[K, V] =>
        clean(parent, ct, lev - 5)
        null
      case ln: LNode[K, V] => // 3) an l-node
        @inline def insertln() = {
          val nn = ln.inserted(k, v)
          GCAS(ln, nn, ct)
        }
        cond match {
          case null =>
            val optv = ln.get(k)
            if (insertln()) optv else null
          case INode.KEY_ABSENT =>
            ln.get(k) match {
              case None => if (insertln()) None else null
              case optv => optv
            }
          case INode.KEY_PRESENT =>
            ln.get(k) match {
              case Some(v0) => if (insertln()) Some(v0) else null
              case None => None
            }
          case otherv: V =>
            ln.get(k) match {
              case Some(v0) if v0 == otherv => if (insertln()) Some(otherv) else null
              case _ => None
            }
        }
    }
  }
  
  /** Looks up the value associated with the key.
   *  
   *  @return          null if no value has been found, RESTART if the operation wasn't successful, or any other value otherwise
   */
  @tailrec final def rec_lookup(k: K, hc: Int, lev: Int, parent: INode[K, V], startgen: Gen, ct: Ctrie[K, V]): AnyRef = {
    val m = GCAS_READ(ct) // use -Yinline!
    
    m match {
      case cn: CNode[K, V] => // 1) a multinode
        val idx = (hc >>> lev) & 0x1f
        val flag = 1 << idx
        val bmp = cn.bitmap
        if ((bmp & flag) == 0) null // 1a) bitmap shows no binding
        else { // 1b) bitmap contains a value - descend
          val pos = if (bmp == 0xffffffff) idx else Integer.bitCount(bmp & (flag - 1))
          val sub = cn.array(pos)
          sub match {
            case in: INode[K, V] =>
              if (ct.isReadOnly || (startgen eq in.gen)) in.rec_lookup(k, hc, lev + 5, this, startgen, ct)
              else {
                if (GCAS(cn, cn.renewed(startgen, ct), ct)) rec_lookup(k, hc, lev, parent, startgen, ct)
                else return RESTART // used to be throw RestartException
              }
            case sn: SNode[K, V] => // 2) singleton node
              if (sn.hc == hc && sn.k == k) sn.v.asInstanceOf[AnyRef]
              else null
          }
        }
      case tn: TNode[K, V] => // 3) non-live node
        def cleanReadOnly(tn: TNode[K, V]) = if (ct.nonReadOnly) {
          clean(parent, ct, lev - 5)
          RESTART // used to be throw RestartException
        } else {
          if (tn.hc == hc && tn.k == k) tn.v.asInstanceOf[AnyRef]
          else null
        }
        cleanReadOnly(tn)
      case ln: LNode[K, V] => // 5) an l-node
        ln.get(k).asInstanceOf[Option[AnyRef]].orNull
    }
  }
  
  /** Removes the key associated with the given value.
   *  
   *  @param v         if null, will remove the key irregardless of the value; otherwise removes only if binding contains that exact key and value
   *  @return          null if not successful, an Option[V] indicating the previous value otherwise
   */
  final def rec_remove(k: K, v: V, hc: Int, lev: Int, parent: INode[K, V], startgen: Gen, ct: Ctrie[K, V]): Option[V] = {
    val m = GCAS_READ(ct) // use -Yinline!
    
    m match {
      case cn: CNode[K, V] =>
        val idx = (hc >>> lev) & 0x1f
        val bmp = cn.bitmap
        val flag = 1 << idx
        if ((bmp & flag) == 0) None
        else {
          val pos = Integer.bitCount(bmp & (flag - 1))
          val sub = cn.array(pos)
          val res = sub match {
            case in: INode[K, V] => 
              if (startgen eq in.gen) in.rec_remove(k, v, hc, lev + 5, this, startgen, ct)
              else {
                if (GCAS(cn, cn.renewed(startgen, ct), ct)) rec_remove(k, v, hc, lev, parent, startgen, ct)
                else null
              }
            case sn: SNode[K, V] =>
              if (sn.hc == hc && sn.k == k && (v == null || sn.v == v)) {
                val ncn = cn.removedAt(pos, flag, gen).toContracted(lev)
                if (GCAS(cn, ncn, ct)) Some(sn.v) else null
              } else None
          }
          
          if (res == None || (res eq null)) res
          else {
            @tailrec def cleanParent(nonlive: AnyRef) {
              val pm = parent.GCAS_READ(ct)
              pm match {
                case cn: CNode[K, V] =>
                  val idx = (hc >>> (lev - 5)) & 0x1f
                  val bmp = cn.bitmap
                  val flag = 1 << idx
                  if ((bmp & flag) == 0) {} // somebody already removed this i-node, we're done
                  else {
                    val pos = Integer.bitCount(bmp & (flag - 1))
                    val sub = cn.array(pos)
                    if (sub eq this) nonlive match {
                      case tn: TNode[K, V] =>
                        val ncn = cn.updatedAt(pos, tn.copyUntombed, gen).toContracted(lev - 5)
                        if (!parent.GCAS(cn, ncn, ct))
                          if (ct.readRoot().gen == startgen) cleanParent(nonlive)
                    }
                  }
                case _ => // parent is no longer a cnode, we're done
              }
            }
            
            if (parent ne null) { // never tomb at root
              val n = GCAS_READ(ct)
              if (n.isInstanceOf[TNode[_, _]])
                cleanParent(n)
            }
            
            res
          }
        }
      case tn: TNode[K, V] =>
        clean(parent, ct, lev - 5)
        null
      case ln: LNode[K, V] =>
        if (v == null) {
          val optv = ln.get(k)
          val nn = ln.removed(k)
          if (GCAS(ln, nn, ct)) optv else null
        } else ln.get(k) match {
          case optv @ Some(v0) if v0 == v =>
            val nn = ln.removed(k)
            if (GCAS(ln, nn, ct)) optv else null
          case _ => None
        }
    }
  }
  
  private def clean(nd: INode[K, V], ct: Ctrie[K, V], lev: Int) {
    val m = nd.GCAS_READ(ct)
    m match {
      case cn: CNode[K, V] => nd.GCAS(cn, cn.toCompressed(ct, lev, gen), ct)
      case _ =>
    }
  }
  
  final def isNullInode(ct: Ctrie[K, V]) = GCAS_READ(ct) eq null
  
  final def cachedSize(ct: Ctrie[K, V]): Int = {
    val m = GCAS_READ(ct)
    m.cachedSize(ct)
  }
  
  /* this is a quiescent method! */
  def string(lev: Int) = "%sINode -> %s".format("  " * lev, mainnode match {
    case null => "<null>"
    case tn: TNode[_, _] => "TNode(%s, %s, %d, !)".format(tn.k, tn.v, tn.hc)
    case cn: CNode[_, _] => cn.string(lev)
    case ln: LNode[_, _] => ln.string(lev)
    case x => "<elem: %s>".format(x)
  })
  
}


private[mutable] object INode {
  val KEY_PRESENT = new AnyRef
  val KEY_ABSENT = new AnyRef
  
  def newRootNode[K, V] = {
    val gen = new Gen
    val cn = new CNode[K, V](0, new Array(0), gen)
    new INode[K, V](cn, gen)
  }
}


private[mutable] final class FailedNode[K, V](p: MainNode[K, V]) extends MainNode[K, V] {
  WRITE_PREV(p)
  
  def string(lev: Int) = throw new UnsupportedOperationException
  
  def cachedSize(ct: AnyRef): Int = throw new UnsupportedOperationException
  
  override def toString = "FailedNode(%s)".format(p)
}


private[mutable] trait KVNode[K, V] {
  def kvPair: (K, V)
}


private[collection] final class SNode[K, V](final val k: K, final val v: V, final val hc: Int)
extends BasicNode with KVNode[K, V] {
  final def copy = new SNode(k, v, hc)
  final def copyTombed = new TNode(k, v, hc)
  final def copyUntombed = new SNode(k, v, hc)
  final def kvPair = (k, v)
  final def string(lev: Int) = ("  " * lev) + "SNode(%s, %s, %x)".format(k, v, hc)
}


private[collection] final class TNode[K, V](final val k: K, final val v: V, final val hc: Int)
extends MainNode[K, V] with KVNode[K, V] {
  final def copy = new TNode(k, v, hc)
  final def copyTombed = new TNode(k, v, hc)
  final def copyUntombed = new SNode(k, v, hc)
  final def kvPair = (k, v)
  final def cachedSize(ct: AnyRef): Int = 1
  final def string(lev: Int) = ("  " * lev) + "TNode(%s, %s, %x, !)".format(k, v, hc)
}


private[collection] final class LNode[K, V](final val listmap: ImmutableListMap[K, V])
extends MainNode[K, V] {
  def this(k: K, v: V) = this(ImmutableListMap(k -> v))
  def this(k1: K, v1: V, k2: K, v2: V) = this(ImmutableListMap(k1 -> v1, k2 -> v2))
  def inserted(k: K, v: V) = new LNode(listmap + ((k, v)))
  def removed(k: K): MainNode[K, V] = {
    val updmap = listmap - k
    if (updmap.size > 1) new LNode(updmap)
    else {
      val (k, v) = updmap.iterator.next
      new TNode(k, v, Ctrie.computeHash(k)) // create it tombed so that it gets compressed on subsequent accesses
    }
  }
  def get(k: K) = listmap.get(k)
  def cachedSize(ct: AnyRef): Int = listmap.size
  def string(lev: Int) = (" " * lev) + "LNode(%s)".format(listmap.mkString(", "))
}


private[collection] final class CNode[K, V](final val bitmap: Int, final val array: Array[BasicNode], final val gen: Gen)
extends CNodeBase[K, V] {
  
  // this should only be called from within read-only snapshots
  final def cachedSize(ct: AnyRef) = {
    val currsz = READ_SIZE()
    if (currsz != -1) currsz
    else {
      val sz = computeSize(ct.asInstanceOf[Ctrie[K, V]])
      while (READ_SIZE() == -1) CAS_SIZE(-1, sz)
      READ_SIZE()
    }
  }
  
  // lends itself towards being parallelizable by choosing
  // a random starting offset in the array
  // => if there are concurrent size computations, they start
  //    at different positions, so they are more likely to
  //    to be independent
  private def computeSize(ct: Ctrie[K, V]): Int = {
    var i = 0
    var sz = 0
    val offset = math.abs(util.Random.nextInt()) % array.length
    while (i < array.length) {
      val pos = (i + offset) % array.length
      array(pos) match {
        case sn: SNode[_, _] => sz += 1
        case in: INode[K, V] => sz += in.cachedSize(ct)
      }
      i += 1
    }
    sz
  }
  
  final def updatedAt(pos: Int, nn: BasicNode, gen: Gen) = {
    val len = array.length
    val narr = new Array[BasicNode](len)
    Array.copy(array, 0, narr, 0, len)
    narr(pos) = nn
    new CNode[K, V](bitmap, narr, gen)
  }
  
  final def removedAt(pos: Int, flag: Int, gen: Gen) = {
    val arr = array
    val len = arr.length
    val narr = new Array[BasicNode](len - 1)
    Array.copy(arr, 0, narr, 0, pos)
    Array.copy(arr, pos + 1, narr, pos, len - pos - 1)
    new CNode[K, V](bitmap ^ flag, narr, gen)
  }
  
  final def insertedAt(pos: Int, flag: Int, nn: BasicNode, gen: Gen) = {
    val len = array.length
    val bmp = bitmap
    val narr = new Array[BasicNode](len + 1)
    Array.copy(array, 0, narr, 0, pos)
    narr(pos) = nn
    Array.copy(array, pos, narr, pos + 1, len - pos)
    new CNode[K, V](bmp | flag, narr, gen)
  }
  
  /** Returns a copy of this cnode such that all the i-nodes below it are copied
   *  to the specified generation `ngen`.
   */
  final def renewed(ngen: Gen, ct: Ctrie[K, V]) = {
    var i = 0
    val arr = array
    val len = arr.length
    val narr = new Array[BasicNode](len)
    while (i < len) {
      arr(i) match {
        case in: INode[K, V] => narr(i) = in.copyToGen(ngen, ct)
        case bn: BasicNode => narr(i) = bn
      }
      i += 1
    }
    new CNode[K, V](bitmap, narr, ngen)
  }
  
  private def resurrect(inode: INode[K, V], inodemain: AnyRef): BasicNode = inodemain match {
    case tn: TNode[_, _] => tn.copyUntombed
    case _ => inode
  }
  
  final def toContracted(lev: Int): MainNode[K, V] = if (array.length == 1 && lev > 0) array(0) match {
    case sn: SNode[K, V] => sn.copyTombed
    case _ => this
  } else this
  
  // - if the branching factor is 1 for this CNode, and the child
  //   is a tombed SNode, returns its tombed version
  // - otherwise, if there is at least one non-null node below,
  //   returns the version of this node with at least some null-inodes
  //   removed (those existing when the op began)
  // - if there are only null-i-nodes below, returns null
  final def toCompressed(ct: Ctrie[K, V], lev: Int, gen: Gen) = {
    var bmp = bitmap
    var i = 0
    val arr = array
    val tmparray = new Array[BasicNode](arr.length)
    while (i < arr.length) { // construct new bitmap
      val sub = arr(i)
      sub match {
        case in: INode[K, V] =>
          val inodemain = in.gcasRead(ct)
          assert(inodemain ne null)
          tmparray(i) = resurrect(in, inodemain)
        case sn: SNode[K, V] =>
          tmparray(i) = sn
      }
      i += 1
    }
    
    new CNode[K, V](bmp, tmparray, gen).toContracted(lev)
  }
  
  private[mutable] def string(lev: Int): String = "CNode %x\n%s".format(bitmap, array.map(_.string(lev + 1)).mkString("\n"))
  
  /* quiescently consistent - don't call concurrently to anything involving a GCAS!! */
  protected def collectElems: Seq[(K, V)] = array flatMap {
    case sn: SNode[K, V] => Some(sn.kvPair)
    case in: INode[K, V] => in.mainnode match {
      case tn: TNode[K, V] => Some(tn.kvPair)
      case ln: LNode[K, V] => ln.listmap.toList
      case cn: CNode[K, V] => cn.collectElems
    }
  }
  
  protected def collectLocalElems: Seq[String] = array flatMap {
    case sn: SNode[K, V] => Some(sn.kvPair._2.toString)
    case in: INode[K, V] => Some(in.toString.drop(14) + "(" + in.gen + ")")
  }
  
  override def toString = {
    val elems = collectLocalElems
    "CNode(sz: %d; %s)".format(elems.size, elems.sorted.mkString(", "))
  }
}


private[mutable] object CNode {
  
  def dual[K, V](x: SNode[K, V], xhc: Int, y: SNode[K, V], yhc: Int, lev: Int, gen: Gen): MainNode[K, V] = if (lev < 35) {
    val xidx = (xhc >>> lev) & 0x1f
    val yidx = (yhc >>> lev) & 0x1f
    val bmp = (1 << xidx) | (1 << yidx)
    if (xidx == yidx) {
      val subinode = new INode[K, V](gen)//(Ctrie.inodeupdater)
      subinode.mainnode = dual(x, xhc, y, yhc, lev + 5, gen)
      new CNode(bmp, Array(subinode), gen)
    } else {
      if (xidx < yidx) new CNode(bmp, Array(x, y), gen)
      else new CNode(bmp, Array(y, x), gen)
    }
  } else {
    new LNode(x.k, x.v, y.k, y.v)
  }
  
}


private[mutable] case class RDCSS_Descriptor[K, V](old: INode[K, V], expectedmain: MainNode[K, V], nv: INode[K, V]) {
  @volatile var committed = false
}


/** A concurrent hash-trie or Ctrie is a concurrent thread-safe lock-free
 *  implementation of a hash array mapped trie. It is used to implement the
 *  concurrent map abstraction. It has particularly scalable concurrent insert
 *  and remove operations and is memory-efficient. It supports O(1), atomic,
 *  lock-free snapshots which are used to implement linearizable lock-free size,
 *  iterator and clear operations. The cost of evaluating the (lazy) snapshot is
 *  distributed across subsequent updates, thus making snapshot evaluation horizontally scalable.
 *  
 *  For details, see: http://lampwww.epfl.ch/~prokopec/ctries-snapshot.pdf
 *  
 *  @author Aleksandar Prokopec
 *  @since 2.10
 */
@SerialVersionUID(0L - 6402774413839597105L)
final class Ctrie[K, V] private (r: AnyRef, rtupd: AtomicReferenceFieldUpdater[Ctrie[K, V], AnyRef])
extends ConcurrentMap[K, V]
   with MapLike[K, V, Ctrie[K, V]]
   with CustomParallelizable[(K, V), ParCtrie[K, V]]
   with Serializable
{
  import Ctrie.computeHash
  
  private var rootupdater = rtupd
  @volatile var root = r
  
  def this() = this(
    INode.newRootNode,
    AtomicReferenceFieldUpdater.newUpdater(classOf[Ctrie[K, V]], classOf[AnyRef], "root")
  )
  
  /* internal methods */
  
  private def writeObject(out: java.io.ObjectOutputStream) {
    val it = iterator
    while (it.hasNext) {
      val (k, v) = it.next()
      out.writeObject(k)
      out.writeObject(v)
    }
    out.writeObject(CtrieSerializationEnd)
  }
  
  private def readObject(in: java.io.ObjectInputStream) {
    root = INode.newRootNode
    rootupdater = AtomicReferenceFieldUpdater.newUpdater(classOf[Ctrie[K, V]], classOf[AnyRef], "root")
    
    var obj: AnyRef = null
    do {
      obj = in.readObject()
      if (obj != CtrieSerializationEnd) {
        val k = obj.asInstanceOf[K]
        val v = in.readObject().asInstanceOf[V]
        update(k, v)
      }
    } while (obj != CtrieSerializationEnd)
  }
  
  @inline final def CAS_ROOT(ov: AnyRef, nv: AnyRef) = rootupdater.compareAndSet(this, ov, nv)
  
  final def readRoot(abort: Boolean = false): INode[K, V] = RDCSS_READ_ROOT(abort)
  
  @inline final def RDCSS_READ_ROOT(abort: Boolean = false): INode[K, V] = {
    val r = /*READ*/root
    r match {
      case in: INode[K, V] => in
      case desc: RDCSS_Descriptor[K, V] => RDCSS_Complete(abort)
    }
  }
  
  @tailrec private def RDCSS_Complete(abort: Boolean): INode[K, V] = {
    val v = /*READ*/root
    v match {
      case in: INode[K, V] => in
      case desc: RDCSS_Descriptor[K, V] =>
        val RDCSS_Descriptor(ov, exp, nv) = desc
        if (abort) {
          if (CAS_ROOT(desc, ov)) ov
          else RDCSS_Complete(abort)
        } else {
          val oldmain = ov.gcasRead(this)
          if (oldmain eq exp) {
            if (CAS_ROOT(desc, nv)) {
              desc.committed = true
              nv
            } else RDCSS_Complete(abort)
          } else {
            if (CAS_ROOT(desc, ov)) ov
            else RDCSS_Complete(abort)
          }
        }
    }
  }
  
  private def RDCSS_ROOT(ov: INode[K, V], expectedmain: MainNode[K, V], nv: INode[K, V]): Boolean = {
    val desc = RDCSS_Descriptor(ov, expectedmain, nv)
    if (CAS_ROOT(ov, desc)) {
      RDCSS_Complete(false)
      /*READ*/desc.committed
    } else false
  }
  
  @tailrec private def inserthc(k: K, hc: Int, v: V) {
    val r = RDCSS_READ_ROOT()
    if (!r.rec_insert(k, v, hc, 0, null, r.gen, this)) inserthc(k, hc, v)
  }
  
  @tailrec private def insertifhc(k: K, hc: Int, v: V, cond: AnyRef): Option[V] = {
    val r = RDCSS_READ_ROOT()
    
    val ret = r.rec_insertif(k, v, hc, cond, 0, null, r.gen, this)
    if (ret eq null) insertifhc(k, hc, v, cond)
    else ret
  }
  
  @tailrec private def lookuphc(k: K, hc: Int): AnyRef = {
    val r = RDCSS_READ_ROOT()
    val res = r.rec_lookup(k, hc, 0, null, r.gen, this)
    if (res eq INodeBase.RESTART) lookuphc(k, hc)
    else res
  }
  
  /* slower:
  //@tailrec
  private def lookuphc(k: K, hc: Int): AnyRef = {
    val r = RDCSS_READ_ROOT()
    try {
      r.rec_lookup(k, hc, 0, null, r.gen, this)
    } catch {
      case RestartException =>
        lookuphc(k, hc)
    }
  }
  */
  
  @tailrec private def removehc(k: K, v: V, hc: Int): Option[V] = {
    val r = RDCSS_READ_ROOT()
    val res = r.rec_remove(k, v, hc, 0, null, r.gen, this)
    if (res ne null) res
    else removehc(k, v, hc)
  }
  
  def string = RDCSS_READ_ROOT().string(0)
  
  /* public methods */
  
  override def seq = this
  
  override def par = new ParCtrie(this)
  
  override def empty: Ctrie[K, V] = new Ctrie[K, V]
  
  final def isReadOnly = rootupdater eq null
  
  final def nonReadOnly = rootupdater ne null
  
  /** Returns a snapshot of this Ctrie.
   *  This operation is lock-free and linearizable.
   *  
   *  The snapshot is lazily updated - the first time some branch
   *  in the snapshot or this Ctrie are accessed, they are rewritten.
   *  This means that the work of rebuilding both the snapshot and this
   *  Ctrie is distributed across all the threads doing updates or accesses
   *  subsequent to the snapshot creation.
   */
  @tailrec final def snapshot(): Ctrie[K, V] = {
    val r = RDCSS_READ_ROOT()
    val expmain = r.gcasRead(this)
    if (RDCSS_ROOT(r, expmain, r.copyToGen(new Gen, this))) new Ctrie(r.copyToGen(new Gen, this), rootupdater)
    else snapshot()
  }
  
  /** Returns a read-only snapshot of this Ctrie.
   *  This operation is lock-free and linearizable.
   *  
   *  The snapshot is lazily updated - the first time some branch
   *  of this Ctrie are accessed, it is rewritten. The work of creating
   *  the snapshot is thus distributed across subsequent updates
   *  and accesses on this Ctrie by all threads.
   *  Note that the snapshot itself is never rewritten unlike when calling
   *  the `snapshot` method, but the obtained snapshot cannot be modified.
   *  
   *  This method is used by other methods such as `size` and `iterator`.
   */
  @tailrec final def readOnlySnapshot(): collection.Map[K, V] = {
    val r = RDCSS_READ_ROOT()
    val expmain = r.gcasRead(this)
    if (RDCSS_ROOT(r, expmain, r.copyToGen(new Gen, this))) new Ctrie(r, null)
    else readOnlySnapshot()
  }
  
  @tailrec final override def clear() {
    val r = RDCSS_READ_ROOT()
    if (!RDCSS_ROOT(r, r.gcasRead(this), INode.newRootNode[K, V])) clear()
  }
  
  final def lookup(k: K): V = {
    val hc = computeHash(k)
    lookuphc(k, hc).asInstanceOf[V]
  }
  
  final override def apply(k: K): V = {
    val hc = computeHash(k)
    val res = lookuphc(k, hc)
    if (res eq null) throw new NoSuchElementException
    else res.asInstanceOf[V]
  }
  
  final def get(k: K): Option[V] = {
    val hc = computeHash(k)
    Option(lookuphc(k, hc)).asInstanceOf[Option[V]]
  }
  
  override def put(key: K, value: V): Option[V] = {
    val hc = computeHash(key)
    insertifhc(key, hc, value, null)
  }
  
  final override def update(k: K, v: V) {
    val hc = computeHash(k)
    inserthc(k, hc, v)
  }
  
  final def +=(kv: (K, V)) = {
    update(kv._1, kv._2)
    this
  }
  
  final override def remove(k: K): Option[V] = {
    val hc = computeHash(k)
    removehc(k, null.asInstanceOf[V], hc)
  }
  
  final def -=(k: K) = {
    remove(k)
    this
  }
  
  def putIfAbsent(k: K, v: V): Option[V] = {
    val hc = computeHash(k)
    insertifhc(k, hc, v, INode.KEY_ABSENT)
  }
  
  def remove(k: K, v: V): Boolean = {
    val hc = computeHash(k)
    removehc(k, v, hc).nonEmpty
  }
  
  def replace(k: K, oldvalue: V, newvalue: V): Boolean = {
    val hc = computeHash(k)
    insertifhc(k, hc, newvalue, oldvalue.asInstanceOf[AnyRef]).nonEmpty
  }
  
  def replace(k: K, v: V): Option[V] = {
    val hc = computeHash(k)
    insertifhc(k, hc, v, INode.KEY_PRESENT)
  }
  
  def iterator: Iterator[(K, V)] =
    if (nonReadOnly) readOnlySnapshot().iterator
    else new CtrieIterator(0, this)
  
  private def cachedSize() = {
    val r = RDCSS_READ_ROOT()
    r.cachedSize(this)
  }
  
  override def size: Int =
    if (nonReadOnly) readOnlySnapshot().size
    else cachedSize()
  
  override def stringPrefix = "Ctrie"
  
}


object Ctrie extends MutableMapFactory[Ctrie] {
  val inodeupdater = AtomicReferenceFieldUpdater.newUpdater(classOf[INodeBase[_, _]], classOf[MainNode[_, _]], "mainnode")
  
  implicit def canBuildFrom[K, V]: CanBuildFrom[Coll, (K, V), Ctrie[K, V]] = new MapCanBuildFrom[K, V]
  
  def empty[K, V]: Ctrie[K, V] = new Ctrie[K, V]
  
  @inline final def computeHash[K](k: K): Int = {
    var hcode = k.hashCode
    hcode = hcode * 0x9e3775cd
    hcode = java.lang.Integer.reverseBytes(hcode)
    hcode * 0x9e3775cd
  }
  
}


private[collection] class CtrieIterator[K, V](var level: Int, private var ct: Ctrie[K, V], mustInit: Boolean = true) extends Iterator[(K, V)] {
  var stack = new Array[Array[BasicNode]](7)
  var stackpos = new Array[Int](7)
  var depth = -1
  var subiter: Iterator[(K, V)] = null
  var current: KVNode[K, V] = null
  
  if (mustInit) initialize()
  
  def hasNext = (current ne null) || (subiter ne null)
  
  def next() = if (hasNext) {
    var r: (K, V) = null
    if (subiter ne null) {
      r = subiter.next()
      checkSubiter()
    } else {
      r = current.kvPair
      advance()
    }
    r
  } else Iterator.empty.next()
  
  private def readin(in: INode[K, V]) = in.gcasRead(ct) match {
    case cn: CNode[K, V] =>
      depth += 1
      stack(depth) = cn.array
      stackpos(depth) = -1
      advance()
    case tn: TNode[K, V] =>
      current = tn
    case ln: LNode[K, V] =>
      subiter = ln.listmap.iterator
      checkSubiter()
    case null =>
      current = null
  }
  
  @inline private def checkSubiter() = if (!subiter.hasNext) {
    subiter = null
    advance()
  }
  
  @inline private def initialize() {
    assert(ct.isReadOnly)
    
    val r = ct.RDCSS_READ_ROOT()
    readin(r)
  }
  
  def advance(): Unit = if (depth >= 0) {
    val npos = stackpos(depth) + 1
    if (npos < stack(depth).length) {
      stackpos(depth) = npos
      stack(depth)(npos) match {
        case sn: SNode[K, V] =>
          current = sn
        case in: INode[K, V] =>
          readin(in)
      }
    } else {
      depth -= 1
      advance()
    }
  } else current = null
  
  protected def newIterator(_lev: Int, _ct: Ctrie[K, V], _mustInit: Boolean) = new CtrieIterator[K, V](_lev, _ct, _mustInit)
  
  protected def dupTo(it: CtrieIterator[K, V]) = {
    it.level = this.level
    it.ct = this.ct
    it.depth = this.depth
    it.current = this.current
    
    // these need a deep copy
    Array.copy(this.stack, 0, it.stack, 0, 7)
    Array.copy(this.stackpos, 0, it.stackpos, 0, 7)
    
    // this one needs to be evaluated
    if (this.subiter == null) it.subiter = null
    else {
      val lst = this.subiter.toList
      this.subiter = lst.iterator
      it.subiter = lst.iterator
    }
  }
  
  /** Returns a sequence of iterators over subsets of this iterator.
   *  It's used to ease the implementation of splitters for a parallel version of the Ctrie.
   */
  protected def subdivide(): Seq[Iterator[(K, V)]] = if (subiter ne null) {
    // the case where an LNode is being iterated
    val it = subiter
    subiter = null
    advance()
    this.level += 1
    Seq(it, this)
  } else if (depth == -1) {
    this.level += 1
    Seq(this)
  } else {
    var d = 0
    while (d <= depth) {
      val rem = stack(d).length - 1 - stackpos(d)
      if (rem > 0) {
        val (arr1, arr2) = stack(d).drop(stackpos(d) + 1).splitAt(rem / 2)
        stack(d) = arr1
        stackpos(d) = -1
        val it = newIterator(level + 1, ct, false)
        it.stack(0) = arr2
        it.stackpos(0) = -1
        it.depth = 0
        it.advance() // <-- fix it
        this.level += 1
        return Seq(this, it)
      }
      d += 1
    }
    this.level += 1
    Seq(this)
  }
  
  def printDebug {
    println("ctrie iterator")
    println(stackpos.mkString(","))
    println("depth: " + depth)
    println("curr.: " + current)
    println(stack.mkString("\n"))
  }
  
}


private[mutable] object RestartException extends util.control.ControlThrowable


/** Only used for ctrie serialization. */
@SerialVersionUID(0L - 7237891413820527142L)
private[mutable] case object CtrieSerializationEnd


private[mutable] object Debug {
  import collection._
  
  lazy val logbuffer = new java.util.concurrent.ConcurrentLinkedQueue[AnyRef]
  
  def log(s: AnyRef) = logbuffer.add(s)
  
  def flush() {
    for (s <- JavaConversions.asScalaIterator(logbuffer.iterator())) Console.out.println(s.toString)
    logbuffer.clear()
  }
  
  def clear() {
    logbuffer.clear()
  }
  
}










