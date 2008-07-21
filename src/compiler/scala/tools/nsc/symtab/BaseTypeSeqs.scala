/* NSC -- new Scala compiler
 * Copyright 2005-2008 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.tools.nsc.symtab

import scala.collection.mutable.ListBuffer
import util.BitSet

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
  self: SymbolTable =>
  import definitions._

  class BaseTypeSeq(elems: Array[Type]) {

    /** The number of types in the sequence */
    def length: Int = elems.length

    /** The type at i'th position in this sequence; lazy types are returned evaluated. */
    def apply(i: Int): Type = elems(i)

    /** The type symbol of the type at i'th position in this sequence;
     *  no evaluation needed.
     */
    def typeSymbol(i: Int): Symbol = elems(i).typeSymbol

    /** Return all evaluated types in this sequence as a list */
    def toList: List[Type] = elems.toList

    private def copy(head: Type, offset: Int): BaseTypeSeq = {
      val arr = new Array[Type](elems.length + offset)
      Array.copy(elems, 0, arr, offset, elems.length)
      arr(0) = head
      new BaseTypeSeq(arr)
    }

    /** Compute new base type sequence with `tp' prepended to this sequence */
    def prepend(tp: Type): BaseTypeSeq = copy(tp, 1)

    /** Compute new base type sequence with `tp' replacing the head of this sequence */
    def updateHead(tp: Type): BaseTypeSeq = copy(tp, 0)

    /** Compute new base type sequence where every element is mapped
     *  with function `f'. Lazy types are mapped but not evaluated */
    def map(f: Type => Type): BaseTypeSeq = new BaseTypeSeq(elems map f)

    def exists(p: Type => Boolean): Boolean = elems exists p
//      (0 until length) exists (i => p(this(i)))

    lazy val maxDepth: Int = {
      var d = 0
      for (i <- 0 until length) d = Math.max(d, self.maxDepth(this(i)))
      d
    }

    override def toString = elems.mkString("BTS(", ",", ")")
  }

  /** A merker object for a base type sequence that's no yet computed.
   *  used to catch inheritance cycles
   */
  val undetBaseTypeSeq: BaseTypeSeq = new BaseTypeSeq(Array())

  /** Create a base type sequence consisting of a single type */
  def baseTypeSingletonSeq(tp: Type): BaseTypeSeq = new BaseTypeSeq(Array(tp))

  /** Create the base type sequence of a compound type wuth given tp.parents */
  def compoundBaseTypeSeq(tp: CompoundType): BaseTypeSeq = {
    //Console.println("computing baseTypeSeq of " + tsym.tpe + " " + tp.parents)//DEBUG
    val buf = new ListBuffer[Type]
    buf += tp.typeSymbol.tpe
    var btsSize = 1
    val nparents = tp.parents.length
    if (nparents != 0) {
      val pbtss = new Array[BaseTypeSeq](nparents)
      val index = new Array[Int](nparents)
      var i = 0
      for (p <- tp.parents) {
        pbtss(i) =
          if (p.baseTypeSeq eq undetBaseTypeSeq) AnyClass.info.baseTypeSeq
          else p.baseTypeSeq
        index(i) = 0
        i += 1
      }
      def nextBaseType(i: Int): Type = {
        val j = index(i)
        val pbts = pbtss(i)
        if (j < pbts.length) pbts(j) else AnyClass.tpe
      }
      var minSym: Symbol = NoSymbol
      while (minSym != AnyClass) {
        minSym = nextBaseType(0).typeSymbol
        i = 1
        while (i < nparents) {
          if (nextBaseType(i).typeSymbol isLess minSym)
            minSym = nextBaseType(i).typeSymbol
          i += 1
        }
        var minTypes: List[Type] = List()
        i = 0
        while (i < nparents) {
          val tp = nextBaseType(i)
          if (tp.typeSymbol == minSym) {
            if (!(minTypes exists (tp =:=))) minTypes = tp :: minTypes;
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
    //Console.println("baseTypeSeqCache of " + tsym.tpe + " = " + arr.toString)//DEBUG
    tp.baseTypeSeqCache = new BaseTypeSeq(elems)
    var j = 0
    while (j < btsSize) {
      elems(j) match {
        case RefinedType(variants, decls) =>
          // can't assert decls.isEmpty; see t0764
          //if (!decls.isEmpty) assert(false, "computing closure of "+this+":"+this.isInstanceOf[RefinedType]+"/"+closureCache(j))
          //Console.println("compute closure of "+this+" => glb("+variants+")")
          elems(j) = mergePrefixAndArgs(variants, -1, maxBaseTypeSeqDepth(variants) + LubGlbMargin) match {
            case Some(tp0) => tp0
            case None => throw new TypeError(
              "the type intersection "+(tp.parents mkString " with ")+" is malformed"+
              "\n --- because ---"+
              "\n no common type instance of base types "+(variants mkString ", and ")+" exists.")
          }
        case _ =>
      }
      j += 1
    }
    tp.baseTypeSeqCache // todo: needed, or can be unit?
  }
}
