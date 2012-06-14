/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.reflect
package internal

// todo implement in terms of BitSet
import scala.collection.{ mutable, immutable }
import math.max
import util.Statistics

/** A base type sequence (BaseTypeSeq) is an ordered sequence spanning all the base types
 *  of a type. It characterized by the following two laws:
 *
 *  (1) Each element of `tp.baseTypeSeq`  is a basetype of `tp`
 *  (2) For each basetype `bt1` of `tp` there is an element `bt` in `tp.baseTypeSeq` such that
 *
 *      bt.typeSymbol = bt1.typeSymbol
 *      bt <: bt1
 *
 *  (3) The type symbols of different elements are different.
 *
 *  Elements in the sequence are ordered by Symbol.isLess.
 *  @note base type sequences were called closures up to 2.7.1. The name has been changed
 *  to avoid confusion with function closures.
 */
trait BaseTypeSeqs {
  this: SymbolTable =>
  import definitions._
  import BaseTypeSeqsStats._

  protected def newBaseTypeSeq(parents: List[Type], elems: Array[Type]) =
    new BaseTypeSeq(parents, elems)

  /** Note: constructor is protected to force everyone to use the factory method newBaseTypeSeq instead.
   *  This is necessary because when run from reflection every base type sequence needs to have a
   *  SynchronizedBaseTypeSeq as mixin.
   */
  class BaseTypeSeq protected[BaseTypeSeqs] (private[BaseTypeSeqs] val parents: List[Type], private[BaseTypeSeqs] val elems: Array[Type]) {
  self =>
    Statistics.incCounter(baseTypeSeqCount)
    Statistics.incCounter(baseTypeSeqLenTotal, elems.length)

    /** The number of types in the sequence */
    def length: Int = elems.length

    // #3676 shows why we can't store NoType in elems to mark cycles
    // (while NoType is in there to indicate a cycle in this BTS, during the execution of
    //  the mergePrefixAndArgs below, the elems get copied without the pending map,
    //  so that NoType's are seen instead of the original type --> spurious compile error)
    private val pending = new mutable.BitSet(length)

    /** The type at i'th position in this sequence; lazy types are returned evaluated. */
    def apply(i: Int): Type =
      if(pending contains i) {
        pending.clear()
        throw CyclicInheritance
      } else
        elems(i) match {
          case rtp @ RefinedType(variants, decls) =>
            // can't assert decls.isEmpty; see t0764
            //if (!decls.isEmpty) assert(false, "computing closure of "+this+":"+this.isInstanceOf[RefinedType]+"/"+closureCache(j))
            //Console.println("compute closure of "+this+" => glb("+variants+")")
            pending += i
            try {
              mergePrefixAndArgs(variants, -1, lubDepth(variants)) match {
                case Some(tp0) =>
                  pending(i) = false
                  elems(i) = tp0
                  tp0
                case None =>
                  typeError(
                    "no common type instance of base types "+(variants mkString ", and ")+" exists.")
              }
            } catch {
              case CyclicInheritance =>
                typeError(
                  "computing the common type instance of base types "+(variants mkString ", and ")+" leads to a cycle.")
            }
          case tp =>
            tp
        }

    def rawElem(i: Int) = elems(i)

    /** The type symbol of the type at i'th position in this sequence;
     *  no evaluation needed.
     */
    def typeSymbol(i: Int): Symbol = {
      elems(i) match {
        case RefinedType(v :: vs, _) => v.typeSymbol
        case tp => tp.typeSymbol
      }
    }

    /** Return all evaluated types in this sequence as a list */
    def toList: List[Type] = elems.toList

    def copy(head: Type, offset: Int): BaseTypeSeq = {
      val arr = new Array[Type](elems.length + offset)
      compat.Platform.arraycopy(elems, 0, arr, offset, elems.length)
      arr(0) = head
      newBaseTypeSeq(parents, arr)
    }

    /** Compute new base type sequence with `tp` prepended to this sequence */
    def prepend(tp: Type): BaseTypeSeq = copy(tp, 1)

    /** Compute new base type sequence with `tp` replacing the head of this sequence */
    def updateHead(tp: Type): BaseTypeSeq = copy(tp, 0)

    /** Compute new base type sequence where every element is mapped
     *  with function `f`. Lazy types are mapped but not evaluated */
    def map(f: Type => Type): BaseTypeSeq = {
	  // inlined `elems map f` for performance
      val len = length
      var arr = new Array[Type](len)
      var i = 0
      while (i < len) {
        arr(i) = f(elems(i))
        i += 1
      }
      newBaseTypeSeq(parents, arr)
    }

    def lateMap(f: Type => Type): BaseTypeSeq = new MappedBaseTypeSeq(this, f)

    def exists(p: Type => Boolean): Boolean = elems exists p

    lazy val maxDepth = maxDepthOfElems

    protected def maxDepthOfElems: Int = {
      var d = 0
      for (i <- 1 until length) d = max(d, typeDepth(elems(i)))
      d
    }

    override def toString = elems.mkString("BTS(", ",", ")")

    private def typeError(msg: String): Nothing =
      throw new TypeError(
        "the type intersection "+(parents mkString " with ")+" is malformed"+
        "\n --- because ---\n"+msg)
  }

  /** A merker object for a base type sequence that's no yet computed.
   *  used to catch inheritance cycles
   */
  val undetBaseTypeSeq: BaseTypeSeq = newBaseTypeSeq(List(), Array())

  /** Create a base type sequence consisting of a single type */
  def baseTypeSingletonSeq(tp: Type): BaseTypeSeq = newBaseTypeSeq(List(), Array(tp))

  /** Create the base type sequence of a compound type wuth given tp.parents */
  def compoundBaseTypeSeq(tp: Type): BaseTypeSeq = {
    val tsym = tp.typeSymbol
    val parents = tp.parents
//    Console.println("computing baseTypeSeq of " + tsym.tpe + " " + parents)//DEBUG
    val buf = new mutable.ListBuffer[Type]
    buf += tsym.tpe
    var btsSize = 1
    if (parents.nonEmpty) {
      val nparents = parents.length
      val pbtss = new Array[BaseTypeSeq](nparents)
      val index = new Array[Int](nparents)
      var i = 0
      for (p <- parents) {
        pbtss(i) =
          if (p.baseTypeSeq eq undetBaseTypeSeq) AnyClass.info.baseTypeSeq
          else p.baseTypeSeq
        index(i) = 0
        i += 1
      }
      def nextTypeSymbol(i: Int): Symbol = {
        val j = index(i)
        val pbts = pbtss(i)
        if (j < pbts.length) pbts.typeSymbol(j) else AnyClass
      }
      def nextRawElem(i: Int): Type = {
        val j = index(i)
        val pbts = pbtss(i)
        if (j < pbts.length) pbts.rawElem(j) else AnyClass.tpe
      }
      var minSym: Symbol = NoSymbol
      while (minSym != AnyClass) {
        minSym = nextTypeSymbol(0)
        i = 1
        while (i < nparents) {
          val nextSym = nextTypeSymbol(i)
          if (nextSym isLess minSym)
            minSym = nextSym
          i += 1
        }
        var minTypes: List[Type] = List()
        i = 0
        while (i < nparents) {
          if (nextTypeSymbol(i) == minSym) {
            nextRawElem(i) match {
              case RefinedType(variants, decls) =>
                for (tp <- variants)
                  if (!(minTypes exists (tp =:= _))) minTypes = tp :: minTypes
              case tp =>
                if (!(minTypes exists (tp =:= _))) minTypes = tp :: minTypes
            }
            index(i) = index(i) + 1
          }
          i += 1
        }
        buf += intersectionType(minTypes)
        btsSize += 1
      }
    }
    val elems = new Array[Type](btsSize)
    buf.copyToArray(elems, 0)
//    Console.println("computed baseTypeSeq of " + tsym.tpe + " " + parents + ": "+elems.toString)//DEBUG
    newBaseTypeSeq(parents, elems)
  }

  class MappedBaseTypeSeq(orig: BaseTypeSeq, f: Type => Type) extends BaseTypeSeq(orig.parents map f, orig.elems) {
    override def apply(i: Int) = f(orig.apply(i))
    override def rawElem(i: Int) = f(orig.rawElem(i))
    override def typeSymbol(i: Int) = orig.typeSymbol(i)
    override def toList = orig.toList map f
    override def copy(head: Type, offset: Int) = (orig map f).copy(head, offset)
    override def map(g: Type => Type) = lateMap(g)
    override def lateMap(g: Type => Type) = orig.lateMap(x => g(f(x)))
    override def exists(p: Type => Boolean) = elems exists (x => p(f(x)))
    override protected def maxDepthOfElems: Int = elems map (x => typeDepth(f(x))) max
    override def toString = elems.mkString("MBTS(", ",", ")")
  }

  val CyclicInheritance = new Throwable
}

object BaseTypeSeqsStats {
  val baseTypeSeqCount = Statistics.newCounter("#base type seqs")
  val baseTypeSeqLenTotal = Statistics.newRelCounter("avg base type seq length", baseTypeSeqCount)
}
