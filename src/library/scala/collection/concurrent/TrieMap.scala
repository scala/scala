/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package concurrent

import java.util.concurrent.atomic._
import scala.collection.parallel.mutable.ParTrieMap
import scala.util.hashing.Hashing
import scala.util.control.ControlThrowable
import generic._
import scala.annotation.tailrec

private[collection] final class INode[K, V](bn: MainNode[K, V], g: Gen) extends INodeBase[K, V](g) {
  import INodeBase._

  WRITE(bn)

  def this(g: Gen) = this(null, g)

  def WRITE(nval: MainNode[K, V]) = INodeBase.updater.set(this, nval)

  def CAS(old: MainNode[K, V], n: MainNode[K, V]) = INodeBase.updater.compareAndSet(this, old, n)

  def gcasRead(ct: TrieMap[K, V]): MainNode[K, V] = GCAS_READ(ct)

  def GCAS_READ(ct: TrieMap[K, V]): MainNode[K, V] = {
    val m = /*READ*/mainnode
    val prevval = /*READ*/m.prev
    if (prevval eq null) m
    else GCAS_Complete(m, ct)
  }

  @tailrec private def GCAS_Complete(m: MainNode[K, V], ct: TrieMap[K, V]): MainNode[K, V] = if (m eq null) null else {
    // complete the GCAS
    val prev = /*READ*/m.prev
    val ctr = ct.readRoot(abort = true)

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

  def GCAS(old: MainNode[K, V], n: MainNode[K, V], ct: TrieMap[K, V]): Boolean = {
    n.WRITE_PREV(old)
    if (CAS(old, n)) {
      GCAS_Complete(n, ct)
      /*READ*/n.prev eq null
    } else false
  }

  private def equal(k1: K, k2: K, ct: TrieMap[K, V]) = ct.equality.equiv(k1, k2)

  private def inode(cn: MainNode[K, V]) = {
    val nin = new INode[K, V](gen)
    nin.WRITE(cn)
    nin
  }

  def copyToGen(ngen: Gen, ct: TrieMap[K, V]) = {
    val nin = new INode[K, V](ngen)
    val main = GCAS_READ(ct)
    nin.WRITE(main)
    nin
  }

  /** Inserts a key value pair, overwriting the old pair if the keys match.
   *
   *  @return        true if successful, false otherwise
   */
  @tailrec def rec_insert(k: K, v: V, hc: Int, lev: Int, parent: INode[K, V], startgen: Gen, ct: TrieMap[K, V]): Boolean = {
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
              if (sn.hc == hc && equal(sn.k, k, ct)) GCAS(cn, cn.updatedAt(pos, new SNode(k, v, hc), gen), ct)
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
  @tailrec def rec_insertif(k: K, v: V, hc: Int, cond: AnyRef, lev: Int, parent: INode[K, V], startgen: Gen, ct: TrieMap[K, V]): Option[V] = {
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
                if (sn.hc == hc && equal(sn.k, k, ct)) {
                  if (GCAS(cn, cn.updatedAt(pos, new SNode(k, v, hc), gen), ct)) Some(sn.v) else null
                } else {
                  val rn = if (cn.gen eq gen) cn else cn.renewed(gen, ct)
                  val nn = rn.updatedAt(pos, inode(CNode.dual(sn, sn.hc, new SNode(k, v, hc), hc, lev + 5, gen)), gen)
                  if (GCAS(cn, nn, ct)) None
                  else null
                }
              case INode.KEY_ABSENT =>
                if (sn.hc == hc && equal(sn.k, k, ct)) Some(sn.v)
                else {
                  val rn = if (cn.gen eq gen) cn else cn.renewed(gen, ct)
                  val nn = rn.updatedAt(pos, inode(CNode.dual(sn, sn.hc, new SNode(k, v, hc), hc, lev + 5, gen)), gen)
                  if (GCAS(cn, nn, ct)) None
                  else null
                }
              case INode.KEY_PRESENT =>
                if (sn.hc == hc && equal(sn.k, k, ct)) {
                  if (GCAS(cn, cn.updatedAt(pos, new SNode(k, v, hc), gen), ct)) Some(sn.v) else null
                } else None
              case otherv =>
                if (sn.hc == hc && equal(sn.k, k, ct) && sn.v == otherv) {
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
          case otherv => None
        }
      case sn: TNode[K, V] =>
        clean(parent, ct, lev - 5)
        null
      case ln: LNode[K, V] => // 3) an l-node
        def insertln() = {
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
          case otherv =>
            ln.get(k) match {
              case Some(v0) if v0 == otherv => if (insertln()) Some(otherv.asInstanceOf[V]) else null
              case _ => None
            }
        }
    }
  }

  /** Looks up the value associated with the key.
   *
   *  @return          null if no value has been found, RESTART if the operation wasn't successful, or any other value otherwise
   */
  @tailrec def rec_lookup(k: K, hc: Int, lev: Int, parent: INode[K, V], startgen: Gen, ct: TrieMap[K, V]): AnyRef = {
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
                else RESTART // used to be throw RestartException
              }
            case sn: SNode[K, V] => // 2) singleton node
              if (sn.hc == hc && equal(sn.k, k, ct)) sn.v.asInstanceOf[AnyRef]
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
  def rec_remove(k: K, v: V, hc: Int, lev: Int, parent: INode[K, V], startgen: Gen, ct: TrieMap[K, V]): Option[V] = {
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
              if (sn.hc == hc && equal(sn.k, k, ct) && (v == null || sn.v == v)) {
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
          val nn = ln.removed(k, ct)
          if (GCAS(ln, nn, ct)) optv else null
        } else ln.get(k) match {
          case optv @ Some(v0) if v0 == v =>
            val nn = ln.removed(k, ct)
            if (GCAS(ln, nn, ct)) optv else null
          case _ => None
        }
    }
  }

  private def clean(nd: INode[K, V], ct: TrieMap[K, V], lev: Int) {
    val m = nd.GCAS_READ(ct)
    m match {
      case cn: CNode[K, V] => nd.GCAS(cn, cn.toCompressed(ct, lev, gen), ct)
      case _ =>
    }
  }

  def isNullInode(ct: TrieMap[K, V]) = GCAS_READ(ct) eq null

  def cachedSize(ct: TrieMap[K, V]): Int = {
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


private[concurrent] object INode {
  val KEY_PRESENT = new AnyRef
  val KEY_ABSENT = new AnyRef

  def newRootNode[K, V] = {
    val gen = new Gen
    val cn = new CNode[K, V](0, new Array(0), gen)
    new INode[K, V](cn, gen)
  }
}


private[concurrent] final class FailedNode[K, V](p: MainNode[K, V]) extends MainNode[K, V] {
  WRITE_PREV(p)

  def string(lev: Int) = throw new UnsupportedOperationException

  def cachedSize(ct: AnyRef): Int = throw new UnsupportedOperationException

  override def toString = "FailedNode(%s)".format(p)
}


private[concurrent] trait KVNode[K, V] {
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


private[collection] final class LNode[K, V](final val listmap: immutable.ListMap[K, V])
extends MainNode[K, V] {
  def this(k: K, v: V) = this(immutable.ListMap(k -> v))
  def this(k1: K, v1: V, k2: K, v2: V) = this(immutable.ListMap(k1 -> v1, k2 -> v2))
  def inserted(k: K, v: V) = new LNode(listmap + ((k, v)))
  def removed(k: K, ct: TrieMap[K, V]): MainNode[K, V] = {
    val updmap = listmap - k
    if (updmap.size > 1) new LNode(updmap)
    else {
      val (k, v) = updmap.iterator.next()
      new TNode(k, v, ct.computeHash(k)) // create it tombed so that it gets compressed on subsequent accesses
    }
  }
  def get(k: K) = listmap.get(k)
  def cachedSize(ct: AnyRef): Int = listmap.size
  def string(lev: Int) = (" " * lev) + "LNode(%s)".format(listmap.mkString(", "))
}


private[collection] final class CNode[K, V](val bitmap: Int, val array: Array[BasicNode], val gen: Gen) extends CNodeBase[K, V] {
  // this should only be called from within read-only snapshots
  def cachedSize(ct: AnyRef) = {
    val currsz = READ_SIZE()
    if (currsz != -1) currsz
    else {
      val sz = computeSize(ct.asInstanceOf[TrieMap[K, V]])
      while (READ_SIZE() == -1) CAS_SIZE(-1, sz)
      READ_SIZE()
    }
  }

  // lends itself towards being parallelizable by choosing
  // a random starting offset in the array
  // => if there are concurrent size computations, they start
  //    at different positions, so they are more likely to
  //    to be independent
  private def computeSize(ct: TrieMap[K, V]): Int = {
    var i = 0
    var sz = 0
    val offset =
      if (array.length > 0)
        //util.Random.nextInt(array.length) /* <-- benchmarks show that this causes observable contention */
        java.util.concurrent.ThreadLocalRandom.current.nextInt(0, array.length)
      else 0
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

  def updatedAt(pos: Int, nn: BasicNode, gen: Gen) = {
    val len = array.length
    val narr = new Array[BasicNode](len)
    Array.copy(array, 0, narr, 0, len)
    narr(pos) = nn
    new CNode[K, V](bitmap, narr, gen)
  }

  def removedAt(pos: Int, flag: Int, gen: Gen) = {
    val arr = array
    val len = arr.length
    val narr = new Array[BasicNode](len - 1)
    Array.copy(arr, 0, narr, 0, pos)
    Array.copy(arr, pos + 1, narr, pos, len - pos - 1)
    new CNode[K, V](bitmap ^ flag, narr, gen)
  }

  def insertedAt(pos: Int, flag: Int, nn: BasicNode, gen: Gen) = {
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
  def renewed(ngen: Gen, ct: TrieMap[K, V]) = {
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

  def toContracted(lev: Int): MainNode[K, V] = if (array.length == 1 && lev > 0) array(0) match {
    case sn: SNode[K, V] => sn.copyTombed
    case _ => this
  } else this

  // - if the branching factor is 1 for this CNode, and the child
  //   is a tombed SNode, returns its tombed version
  // - otherwise, if there is at least one non-null node below,
  //   returns the version of this node with at least some null-inodes
  //   removed (those existing when the op began)
  // - if there are only null-i-nodes below, returns null
  def toCompressed(ct: TrieMap[K, V], lev: Int, gen: Gen) = {
    val bmp = bitmap
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

  private[concurrent] def string(lev: Int): String = "CNode %x\n%s".format(bitmap, array.map(_.string(lev + 1)).mkString("\n"))

  /* quiescently consistent - don't call concurrently to anything involving a GCAS!! */
  private def collectElems: Seq[(K, V)] = array flatMap {
    case sn: SNode[K, V] => Some(sn.kvPair)
    case in: INode[K, V] => in.mainnode match {
      case tn: TNode[K, V] => Some(tn.kvPair)
      case ln: LNode[K, V] => ln.listmap.toList
      case cn: CNode[K, V] => cn.collectElems
    }
  }

  private def collectLocalElems: Seq[String] = array flatMap {
    case sn: SNode[K, V] => Some(sn.kvPair._2.toString)
    case in: INode[K, V] => Some(in.toString.drop(14) + "(" + in.gen + ")")
  }

  override def toString = {
    val elems = collectLocalElems
    "CNode(sz: %d; %s)".format(elems.size, elems.sorted.mkString(", "))
  }
}


private[concurrent] object CNode {

  def dual[K, V](x: SNode[K, V], xhc: Int, y: SNode[K, V], yhc: Int, lev: Int, gen: Gen): MainNode[K, V] = if (lev < 35) {
    val xidx = (xhc >>> lev) & 0x1f
    val yidx = (yhc >>> lev) & 0x1f
    val bmp = (1 << xidx) | (1 << yidx)
    if (xidx == yidx) {
      val subinode = new INode[K, V](gen)//(TrieMap.inodeupdater)
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


private[concurrent] case class RDCSS_Descriptor[K, V](old: INode[K, V], expectedmain: MainNode[K, V], nv: INode[K, V]) {
  @volatile var committed = false
}


/** A concurrent hash-trie or TrieMap is a concurrent thread-safe lock-free
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
final class TrieMap[K, V] private (r: AnyRef, rtupd: AtomicReferenceFieldUpdater[TrieMap[K, V], AnyRef], hashf: Hashing[K], ef: Equiv[K])
extends scala.collection.concurrent.Map[K, V]
   with scala.collection.mutable.MapLike[K, V, TrieMap[K, V]]
   with CustomParallelizable[(K, V), ParTrieMap[K, V]]
   with Serializable
{
  private var hashingobj = if (hashf.isInstanceOf[Hashing.Default[_]]) new TrieMap.MangledHashing[K] else hashf
  private var equalityobj = ef
  private var rootupdater = rtupd
  def hashing = hashingobj
  def equality = equalityobj
  @deprecated("this field will be made private", "2.12.0")
  @volatile /*private*/ var root = r

  def this(hashf: Hashing[K], ef: Equiv[K]) = this(
    INode.newRootNode,
    AtomicReferenceFieldUpdater.newUpdater(classOf[TrieMap[K, V]], classOf[AnyRef], "root"),
    hashf,
    ef
  )

  def this() = this(Hashing.default, Equiv.universal)

  /* internal methods */

  private def writeObject(out: java.io.ObjectOutputStream) {
    out.writeObject(hashingobj)
    out.writeObject(equalityobj)

    val it = iterator
    while (it.hasNext) {
      val (k, v) = it.next()
      out.writeObject(k)
      out.writeObject(v)
    }
    out.writeObject(TrieMapSerializationEnd)
  }

  private def readObject(in: java.io.ObjectInputStream) {
    root = INode.newRootNode
    rootupdater = AtomicReferenceFieldUpdater.newUpdater(classOf[TrieMap[K, V]], classOf[AnyRef], "root")

    hashingobj = in.readObject().asInstanceOf[Hashing[K]]
    equalityobj = in.readObject().asInstanceOf[Equiv[K]]

    var obj: AnyRef = null
    do {
      obj = in.readObject()
      if (obj != TrieMapSerializationEnd) {
        val k = obj.asInstanceOf[K]
        val v = in.readObject().asInstanceOf[V]
        update(k, v)
      }
    } while (obj != TrieMapSerializationEnd)
  }

  @deprecated("this method will be made private", "2.12.0")
  /*private*/ def CAS_ROOT(ov: AnyRef, nv: AnyRef) = rootupdater.compareAndSet(this, ov, nv)

  @deprecated("this method will be made private", "2.12.0")
  /*private[collection]*/ def readRoot(abort: Boolean = false): INode[K, V] = RDCSS_READ_ROOT(abort)

  @deprecated("this method will be made private", "2.12.0")
  /*private[concurrent]*/ def RDCSS_READ_ROOT(abort: Boolean = false): INode[K, V] = {
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
      RDCSS_Complete(abort = false)
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

  override def par = new ParTrieMap(this)

  override def empty: TrieMap[K, V] = new TrieMap[K, V]

  def isReadOnly = rootupdater eq null

  def nonReadOnly = rootupdater ne null

  /** Returns a snapshot of this TrieMap.
   *  This operation is lock-free and linearizable.
   *
   *  The snapshot is lazily updated - the first time some branch
   *  in the snapshot or this TrieMap are accessed, they are rewritten.
   *  This means that the work of rebuilding both the snapshot and this
   *  TrieMap is distributed across all the threads doing updates or accesses
   *  subsequent to the snapshot creation.
   */
  @tailrec def snapshot(): TrieMap[K, V] = {
    val r = RDCSS_READ_ROOT()
    val expmain = r.gcasRead(this)
    if (RDCSS_ROOT(r, expmain, r.copyToGen(new Gen, this))) new TrieMap(r.copyToGen(new Gen, this), rootupdater, hashing, equality)
    else snapshot()
  }

  /** Returns a read-only snapshot of this TrieMap.
   *  This operation is lock-free and linearizable.
   *
   *  The snapshot is lazily updated - the first time some branch
   *  of this TrieMap are accessed, it is rewritten. The work of creating
   *  the snapshot is thus distributed across subsequent updates
   *  and accesses on this TrieMap by all threads.
   *  Note that the snapshot itself is never rewritten unlike when calling
   *  the `snapshot` method, but the obtained snapshot cannot be modified.
   *
   *  This method is used by other methods such as `size` and `iterator`.
   */
  @tailrec def readOnlySnapshot(): scala.collection.Map[K, V] = {
    val r = RDCSS_READ_ROOT()
    val expmain = r.gcasRead(this)
    if (RDCSS_ROOT(r, expmain, r.copyToGen(new Gen, this))) new TrieMap(r, null, hashing, equality)
    else readOnlySnapshot()
  }

  @tailrec override def clear() {
    val r = RDCSS_READ_ROOT()
    if (!RDCSS_ROOT(r, r.gcasRead(this), INode.newRootNode[K, V])) clear()
  }


  def computeHash(k: K) = hashingobj.hash(k)

  def lookup(k: K): V = {
    val hc = computeHash(k)
    lookuphc(k, hc).asInstanceOf[V]
  }

  override def apply(k: K): V = {
    val hc = computeHash(k)
    val res = lookuphc(k, hc)
    if (res eq null) throw new NoSuchElementException
    else res.asInstanceOf[V]
  }

  def get(k: K): Option[V] = {
    val hc = computeHash(k)
    Option(lookuphc(k, hc)).asInstanceOf[Option[V]]
  }

  override def put(key: K, value: V): Option[V] = {
    val hc = computeHash(key)
    insertifhc(key, hc, value, null)
  }

  override def update(k: K, v: V) {
    val hc = computeHash(k)
    inserthc(k, hc, v)
  }

  def +=(kv: (K, V)) = {
    update(kv._1, kv._2)
    this
  }

  override def remove(k: K): Option[V] = {
    val hc = computeHash(k)
    removehc(k, null.asInstanceOf[V], hc)
  }

  def -=(k: K) = {
    remove(k)
    this
  }

  def putIfAbsent(k: K, v: V): Option[V] = {
    val hc = computeHash(k)
    insertifhc(k, hc, v, INode.KEY_ABSENT)
  }

  // TODO once computeIfAbsent is added to concurrent.Map,
  // move the comment there and tweak the 'at most once' part
  /** If the specified key is not already in the map, computes its value using
   *  the given thunk `op` and enters it into the map.
   *
   *  Since concurrent maps cannot contain `null` for keys or values,
   *  a `NullPointerException` is thrown if the thunk `op`
   *  returns `null`.
   *
   *  If the specified mapping function throws an exception,
   *  that exception is rethrown.
   *
   *  Note: This method will invoke op at most once.
   *  However, `op` may be invoked without the result being added to the map if
   *  a concurrent process is also trying to add a value corresponding to the
   *  same key `k`.
   *
   *  @param k      the key to modify
   *  @param op     the expression that computes the value
   *  @return       the newly added value
   */
  override def getOrElseUpdate(k: K, op: =>V): V = {
    val oldv = lookup(k)
    if (oldv != null) oldv.asInstanceOf[V]
    else {
      val v = op
      if (v == null) {
        throw new NullPointerException("Concurrent TrieMap values cannot be null.")
      } else {
        val hc = computeHash(k)
        insertifhc(k, hc, v, INode.KEY_ABSENT) match {
          case Some(oldv) => oldv
          case None => v
        }
      }
    }
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
    else new TrieMapIterator(0, this)

  ////////////////////////////////////////////////////////////////////////////
  //
  // scala/bug#10177 These methods need overrides as the inherited implementations
  // call `.iterator` more than once, which doesn't guarantee a coherent
  // view of the data if there is a concurrent writer
  // Note that the we don't need overrides for keysIterator or valuesIterator
  // TrieMapTest validates the behaviour.
  override def values: Iterable[V] = {
    if (nonReadOnly) readOnlySnapshot().values
    else super.values
  }
  override def keySet: Set[K] = {
    if (nonReadOnly) readOnlySnapshot().keySet
    else super.keySet
  }
  override def filterKeys(p: K => Boolean): collection.Map[K, V] = {
    if (nonReadOnly) readOnlySnapshot().filterKeys(p)
    else super.filterKeys(p)
  }
  override def mapValues[W](f: V => W): collection.Map[K, W] = {
    if (nonReadOnly) readOnlySnapshot().mapValues(f)
    else super.mapValues(f)
  }
  // END extra overrides
  ///////////////////////////////////////////////////////////////////


  private def cachedSize() = {
    val r = RDCSS_READ_ROOT()
    r.cachedSize(this)
  }

  override def size: Int =
    if (nonReadOnly) readOnlySnapshot().size
    else cachedSize()

  override def stringPrefix = "TrieMap"

}


object TrieMap extends MutableMapFactory[TrieMap] {
  val inodeupdater = AtomicReferenceFieldUpdater.newUpdater(classOf[INodeBase[_, _]], classOf[MainNode[_, _]], "mainnode")

  implicit def canBuildFrom[K, V]: CanBuildFrom[Coll, (K, V), TrieMap[K, V]] = new MapCanBuildFrom[K, V]

  def empty[K, V]: TrieMap[K, V] = new TrieMap[K, V]

  class MangledHashing[K] extends Hashing[K] {
    def hash(k: K)= scala.util.hashing.byteswap32(k.##)
  }

}


private[collection] class TrieMapIterator[K, V](var level: Int, private var ct: TrieMap[K, V], mustInit: Boolean = true) extends Iterator[(K, V)] {
  private val stack = new Array[Array[BasicNode]](7)
  private val stackpos = new Array[Int](7)
  private var depth = -1
  private var subiter: Iterator[(K, V)] = null
  private var current: KVNode[K, V] = null

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

  private def checkSubiter() = if (!subiter.hasNext) {
    subiter = null
    advance()
  }

  private def initialize() {
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

  protected def newIterator(_lev: Int, _ct: TrieMap[K, V], _mustInit: Boolean) = new TrieMapIterator[K, V](_lev, _ct, _mustInit)

  protected def dupTo(it: TrieMapIterator[K, V]) = {
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
   *  It's used to ease the implementation of splitters for a parallel version of the TrieMap.
   */
  protected def subdivide(): Seq[Iterator[(K, V)]] = if (subiter ne null) {
    // the case where an LNode is being iterated
    val it = newIterator(level + 1, ct, _mustInit = false)
    it.depth = -1
    it.subiter = this.subiter
    it.current = null
    this.subiter = null
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
        val it = newIterator(level + 1, ct, _mustInit = false)
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

  @deprecated("this method will be removed", "2.12.0")
  def printDebug() {
    println("ctrie iterator")
    println(stackpos.mkString(","))
    println("depth: " + depth)
    println("curr.: " + current)
    println(stack.mkString("\n"))
  }

}


private[concurrent] object RestartException extends ControlThrowable


/** Only used for ctrie serialization. */
@SerialVersionUID(0L - 7237891413820527142L)
private[concurrent] case object TrieMapSerializationEnd


private[concurrent] object Debug {
  import JavaConverters._

  lazy val logbuffer = new java.util.concurrent.ConcurrentLinkedQueue[AnyRef]

  def log(s: AnyRef) = logbuffer.add(s)

  def flush() {
    for (s <- logbuffer.iterator().asScala) Console.out.println(s.toString)
    logbuffer.clear()
  }

  def clear() {
    logbuffer.clear()
  }

}
