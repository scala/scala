/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.tools.nsc.symtab

// todo implement in terms of BitSet
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Map
import Math.max

/** A base type sequence (BaseTypeSeq) is an ordered sequence spanning all the base types
 *  of a type. It characterized by the following two laws:
 *
 *  (1) Each element of `tp.baseTypeSeq'  is a basetype of `tp'
 *  (2) For each basetype `bt1' of `tp' there is an element `bt' in `tp.baseTypeSeq' such that
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

  class BaseTypeSeq(parents: List[Type], elems: Array[Type]) {

    /** The number of types in the sequence */
    def length: Int = elems.length

    var pending: Map[Int, Type] = Map()

    /** The type at i'th position in this sequence; lazy types are returned evaluated. */
    def apply(i: Int): Type = elems(i) match {
      case NoType =>
        pending = Map()
        elems(i) = AnyClass.tpe
        throw CyclicInheritance
      case rtp @ RefinedType(variants, decls) =>
        // can't assert decls.isEmpty; see t0764
        //if (!decls.isEmpty) assert(false, "computing closure of "+this+":"+this.isInstanceOf[RefinedType]+"/"+closureCache(j))
        //Console.println("compute closure of "+this+" => glb("+variants+")")
        pending += (i -> rtp)
        elems(i) = NoType
        try {
          mergePrefixAndArgs(variants, -1, lubDepth(variants)) match {
            case Some(tp0) =>
              pending -= i
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
      def tsym(tp: Type) = tp match {
        case RefinedType(v :: vs, _) => v.typeSymbol
        case _ => tp.typeSymbol
      }
      elems(i) match {
        case NoType =>
          pending get i match {
            case Some(tp) => tsym(tp)
            case _ => NoType.typeSymbol
          }
        case tp => tsym(tp)
      }
    }

    /** Return all evaluated types in this sequence as a list */
    def toList: List[Type] = elems.toList

    private def copy(head: Type, offset: Int): BaseTypeSeq = {
      val arr = new Array[Type](elems.length + offset)
      compat.Platform.arraycopy(elems, 0, arr, offset, elems.length)
      arr(0) = head
      new BaseTypeSeq(parents, arr)
    }

    /** Compute new base type sequence with `tp' prepended to this sequence */
    def prepend(tp: Type): BaseTypeSeq = copy(tp, 1)

    /** Compute new base type sequence with `tp' replacing the head of this sequence */
    def updateHead(tp: Type): BaseTypeSeq = copy(tp, 0)

    /** Compute new base type sequence where every element is mapped
     *  with function `f'. Lazy types are mapped but not evaluated */
    def map(f: Type => Type): BaseTypeSeq = {
	  // inlined `elems map f' for performance
      val len = length
      var arr = new Array[Type](len)
      var i = 0
      while (i < len) {
        arr(i) = f(elems(i))
        i += 1
      }
      new BaseTypeSeq(parents, arr)
    }

    def exists(p: Type => Boolean): Boolean = elems exists p
//      (0 until length) exists (i => p(this(i)))

    def normalize(parents: List[Type]) {}
/*
      var j = 0
      while (j < elems.length) {
        elems(j) match {
          case RefinedType(variants, decls) =>
            // can't assert decls.isEmpty; see t0764
            //if (!decls.isEmpty) assert(false, "computing closure of "+this+":"+this.isInstanceOf[RefinedType]+"/"+closureCache(j))
            //Console.println("compute closure of "+this+" => glb("+variants+")")
            elems(j) = mergePrefixAndArgs(variants, -1, maxBaseTypeSeqDepth(variants) + LubGlbMargin) match {
              case Some(tp0) => tp0
              case None => throw new TypeError(
                "the type intersection "+(parents mkString " with ")+" is malformed"+
                "\n --- because ---"+
                "\n no common type instance of base types "+(variants mkString ", and ")+" exists.")
            }
          case _ =>
        }
        j += 1
      }
    }
*/
    lazy val maxDepth: Int = {
      var d = 0
      for (i <- 0 until length) d = Math.max(d, maxDpth(elems(i)))
      d
    }

    /** The maximum depth of type `tp' */
    private def maxDpth(tp: Type): Int = tp match {
      case TypeRef(pre, sym, args) =>
        max(maxDpth(pre), maxDpth(args) + 1)
      case RefinedType(parents, decls) =>
        max(maxDpth(parents), maxDpth(decls.toList.map(_.info)) + 1)
      case TypeBounds(lo, hi) =>
        max(maxDpth(lo), maxDpth(hi))
      case MethodType(paramtypes, result) =>
        maxDpth(result)
      case PolyType(tparams, result) =>
        max(maxDpth(result), maxDpth(tparams map (_.info)) + 1)
      case ExistentialType(tparams, result) =>
        max(maxDpth(result), maxDpth(tparams map (_.info)) + 1)
      case _ =>
        1
    }

    /** The maximum depth of all types `tps' */
    private def maxDpth(tps: Seq[Type]): Int = {
      var d = 0
      for (tp <- tps) d = max(d, maxDpth(tp))
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
  val undetBaseTypeSeq: BaseTypeSeq = new BaseTypeSeq(List(), Array())

  /** Create a base type sequence consisting of a single type */
  def baseTypeSingletonSeq(tp: Type): BaseTypeSeq = new BaseTypeSeq(List(), Array(tp))

  /** Create the base type sequence of a compound type wuth given tp.parents */
  def compoundBaseTypeSeq(tp: Type/*tsym: Symbol, parents: List[Type]*/): BaseTypeSeq = {
    val tsym = tp.typeSymbol
    val parents = tp.parents
//    Console.println("computing baseTypeSeq of " + tsym.tpe + " " + parents)//DEBUG
    val buf = new ListBuffer[Type]
    buf += tsym.tpe
    var btsSize = 1
    val nparents = parents.length
    if (nparents != 0) {
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
                  if (!(minTypes exists (tp =:=))) minTypes = tp :: minTypes
              case tp =>
                if (!(minTypes exists (tp =:=))) minTypes = tp :: minTypes
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
    new BaseTypeSeq(parents, elems)
  }

  val CyclicInheritance = new Throwable
}
