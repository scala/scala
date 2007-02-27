/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Burak Emir
 */
// $Id$

package scala.tools.nsc.matching

import compat.StringBuilder
import scala.tools.nsc.util.Position

trait PatternNodes requires transform.ExplicitOuter {

  import global._

  type SymSet = collection.immutable.Set[Symbol]

  /** Intermediate data structure for algebraic + pattern matcher
   */
  sealed class PatternNode {
    var pos = FirstPos
    var tpe: Type  = _
    var or: PatternNode = _
    var and: PatternNode = _

    def casted: Symbol = NoSymbol

    def symbol2bind: Symbol = casted

    def forEachAlternative(f: PatternNode => Unit) { // only for header?
      var z = this;
      while(z ne null) {
        f(z)
        z = z.or
      }
    }

    def isUnguardedBody = this match {
      case b:Body => b.hasUnguarded
      case _      => false
    }

    def isSingleUnguardedBody = this match {
      case b:Body => b.isSingleUnguarded
      case _      => false
    }

    def bodyToTree(): Tree = this match {
      case _b:Body =>
        _b.body(0)
      case _ =>
        error("bodyToTree called for pattern node "+this)
        null
    }

    def getTpe(): Type = tpe

    def setType(tpe: Type): Unit = { this.tpe = tpe }

    def dup(): PatternNode = {
      var res: PatternNode = this match {
        case h:Header =>
          new Header(h.selector, h.next)
        case b:Body=>
          new Body(b.bound, b.guard, b.body)
        case DefaultPat() =>
          DefaultPat()
        case ConstrPat(casted) =>
          ConstrPat(casted)
        case SequencePat(casted, len) =>
          SequencePat(casted, len)
        case ConstantPat(value) =>
          ConstantPat(value)
        case VariablePat(tree) =>
          VariablePat(tree)
        case AltPat(subheader) =>
          AltPat(subheader)
        case _ =>
          error(""); null
      }
      res.pos = pos
      res.tpe = tpe
      res.or = or
      res.and = and
      res
    }

    def _symbol: Symbol = this match { // @todo
      case UnapplyPat(casted, fn) =>
	casted
      case ConstrPat(casted) =>
        casted
      case SequencePat(casted, _) =>
        casted
      case RightIgnoringSequencePat(casted, _, _) =>
        casted
      case _ =>
        NoSymbol //.NONE
    }

    def nextH(): PatternNode = this match {
      case _h:Header => _h.next
      case _ => null
    }

    def isDefaultPat(): boolean = this match {
      case DefaultPat() => true
      case _ => false
    }

    /** returns true if
     *  p and q are equal (constructor | sequence) type tests, or
     *  "q matches" => "p matches"
     */
    def isSameAs(q: PatternNode): boolean = this match {
      case ConstrPat(_) =>
        q match {
          case ConstrPat(_) =>
            isSameType(q.getTpe(), this.getTpe())
          case _ =>
            false
        }
      case SequencePat(_, plen) =>
        q match {
          case SequencePat(_, qlen) =>
            (plen == qlen) && isSameType(q.getTpe(), this.getTpe())
          case _ =>
            false
        }
      case _ =>
        subsumes(q)
    }

    /** returns true if "q matches" => "p matches"
     */
    def subsumes(q:PatternNode): Boolean = this match {
      case DefaultPat() =>
        q match {
          case DefaultPat() =>
            true
          case _ =>
            false
        }
      case ConstrPat(_) =>
        q match {
          case ConstrPat(_) =>
            isSubType(q.getTpe(), this.getTpe())
          case _ =>
            false
        }
      case SequencePat(_, plen) =>
        q match {
          case SequencePat(_, qlen) =>
            (plen == qlen) && isSubType(q.getTpe(), this.getTpe())
          case _ =>
            false
        }
      case ConstantPat(pval) =>
        q match {
          case ConstantPat(qval) =>
             pval == qval
          case _ =>
            false
        }
      case VariablePat(tree) =>
        q match {
          case VariablePat(other) =>
            (tree.symbol ne null) &&
            (tree.symbol != NoSymbol) &&
            (!tree.symbol.isError) &&
            (tree.symbol == other.symbol)
          case _ =>
            false
        }
      case _ =>
        false
    }

    override def toString(): String = this match {
      case _h:Header =>
        "Header(" + _h.selector + ")";
      case _b:Body =>
        "Body"
      case DefaultPat() =>
        "DefaultPat"
      case ConstrPat(casted) =>
        "ConstrPat(" + casted + ")"
      case SequencePat(casted, len) =>
        "SequencePat(" + casted + ", " + len + "...)"
      case RightIgnoringSequencePat(casted, castedRest, minlen) =>
        "RightIgnoringSequencePat(" + casted + ", " + castedRest + ", "+ minlen + "...)"
      case ConstantPat(value) =>
        "ConstantPat(" + value + ")"
      case VariablePat(tree) =>
        "VariablePat"
      case UnapplyPat(casted, fn) =>
	"UnapplyPat(" + casted + ")"
      case AltPat(alts) =>
        "Alternative("+alts+")"
    }

    def print(indent: String, sb: StringBuilder): StringBuilder = {
      val patNode = this

      def cont = if (patNode.or ne null) patNode.or.print(indent, sb) else sb

      def newIndent(s: String) = {
        val removeBar: Boolean = (null == patNode.or)
        val sb = new StringBuilder()
        sb.append(indent)
        if (removeBar)
          sb.setCharAt(indent.length() - 1, ' ')
        var i = 0; while (i < s.length()) {
          sb.append(' ')
          i = i + 1
        }
        sb.toString()
      }

      if (patNode eq null)
        sb.append(indent).append("NULL")
      else
        patNode match {
        case UnapplyPat(_,fn) =>
          sb.append(indent + "UNAPPLY(" + fn + ")").append('\n')
        case _h: Header =>
          val selector = _h.selector
          val next = _h.next
          sb.append(indent + "HEADER(" + patNode.getTpe() +
                          ", " + selector + ")").append('\n')
          if(patNode.or ne null) patNode.or.print(indent + "|", sb)
          if (next ne null)
            next.print(indent, sb)
          else
            sb
        case ConstrPat(casted) =>
          val s = ("-- " + patNode.getTpe().symbol.name +
                   "(" + patNode.getTpe() + ", " + casted + ") -> ")
          val nindent = newIndent(s)
          sb.append(nindent + s).append('\n')
          patNode.and.print(nindent, sb)
          cont

        case SequencePat( casted, plen ) =>
          val s = ("-- " + patNode.getTpe().symbol.name + "(" +
                   patNode.getTpe() +
                   ", " + casted + ", " + plen + ") -> ")
          val nindent = newIndent(s)
          sb.append(indent + s).append('\n')
          patNode.and.print(nindent, sb)
          cont

        case RightIgnoringSequencePat( casted, castedRest, plen ) =>
          val s = ("-- ri " + patNode.getTpe().symbol.name + "(" +
                   patNode.getTpe() +
                   ", " + casted + ", " + plen + ") -> ")
          val nindent = newIndent(s)
          sb.append(indent + s).append('\n')
          patNode.and.print(nindent, sb)
          cont


        case DefaultPat() =>
          sb.append(indent + "-- _ -> ").append('\n')
          patNode.and.print(indent.substring(0, indent.length() - 1) +
                      "         ", sb)
          cont

        case ConstantPat(value) =>
          val s = "-- CONST(" + value + ") -> "
          val nindent = newIndent(s)
          sb.append(indent + s).append('\n')
          patNode.and.print( nindent, sb)
          cont

        case VariablePat(tree) =>
          val s = "-- STABLEID(" + tree + ": " + patNode.getTpe() + ") -> "
          val nindent = newIndent(s)
          sb.append(indent + s).append('\n')
          patNode.and.print(nindent, sb)
          cont

        case AltPat(header) =>
          sb.append(indent + "-- ALTERNATIVES:").append('\n')
          header.print(indent + "   * ", sb)
          patNode.and.print(indent + "   * -> ", sb)
          cont

        case _b:Body =>
          if ((_b.guard.length == 0) && (_b.body.length == 0))
            sb.append(indent + "true").append('\n')
          else
            sb.append(indent + "BODY(" + _b.body.length + ")").append('\n')

      }
    } // def print

  } // class PatternNode

  class Header(sel1: Tree, next1: Header) extends PatternNode {
    var selector: Tree = sel1
    var next: Header = next1

    def findLast: PatternNode = {
      var g: PatternNode = findLastSection
      while(g.or != null)   { g = g.or }
      g
    }

    def findLastSection: Header = {
      var h: Header = this;
      while(h.next != null) { h = h.next }
      h
    }
    var isSubHeader = false;

    // returns true if this header node has a catch all case

    def catchesAll: Boolean = {
      //Console.println(this.print("  catchesAll %%%%", new StringBuilder()).toString)
      val p = findLast
      (p.isDefaultPat && p.and.isUnguardedBody)
    }

    // executes an action for every or branch
    def forEachBranch(f: PatternNode => Unit) { if(or ne null) or.forEachAlternative(f) }

    // executes an action for every header section
    def forEachSection(f: Header => Unit) { var h = this; while (h ne null) {f(h); h = h.next}}

    /** returns true if this tree is optimizable
     *  throws a warning if is not exhaustive
     */
    def optimize1(): (Boolean, SymSet, SymSet) = {
      import symtab.Flags

      val selType = this.getTpe

      if (!isSubType(selType, definitions.ScalaObjectClass.tpe))
        return (false, null, emptySymbolSet)

      if(this.or eq null)
        return (false, null, emptySymbolSet)  // only case _

      def checkExCoverage(tpesym:Symbol): SymSet =
        if(!tpesym.hasFlag(Flags.SEALED)) emptySymbolSet else
          tpesym.children.flatMap { x =>
            val z = checkExCoverage(x)
            if(x.hasFlag(Flags.ABSTRACT)) z else z + x
          }

      def andIsUnguardedBody(p1:PatternNode) = p1.and match {
        case p: Body => p.hasUnguarded
        case _       => false
      }

      //Console.println("optimize1("+selType+","+alternatives1+")")
      var res = true
      var coveredCases: SymSet  = emptySymbolSet
      var remainingCases        = checkExCoverage(selType.symbol)
      var cases = 0;

      def traverse(alts:PatternNode) {
        //Console.println("traverse, alts="+alts)
        alts match {
          case ConstrPat(_) =>
            //Console.print("ConstPat! of"+alts.getTpe.symbol)
            if (alts.getTpe.symbol.hasFlag(Flags.CASE)) {
              coveredCases   = coveredCases + alts.getTpe.symbol
              remainingCases = remainingCases - alts.getTpe.symbol
              cases = cases + 1
            } else {
              val covered = remainingCases.filter { x =>
                //Console.println("x.tpe is "+x.tpe)
                val y = alts.getTpe.prefix.memberType(x)
                //Console.println(y + " is sub of "+alts.getTpe+" ? "+isSubType(y, alts.getTpe));
                isSubType(y, alts.getTpe)
              }
              //Console.println(" covered : "+covered)

              coveredCases   = coveredCases ++ covered
              remainingCases = remainingCases -- covered
              res = false
            }

          // Nil is also a "constructor pattern" somehow
          case VariablePat(tree) if (tree.tpe.symbol.hasFlag(Flags.MODULE)) => // Nil
            coveredCases   = coveredCases + tree.tpe.symbol
            remainingCases = remainingCases - tree.tpe.symbol
            cases = cases + 1
            res = res && tree.tpe.symbol.hasFlag(Flags.CASE)
          case DefaultPat() =>
            if(andIsUnguardedBody(alts) || alts.and.isInstanceOf[Header]) {
              coveredCases   = emptySymbolSet
              remainingCases = emptySymbolSet
            }
          case UnapplyPat(_,_) | SequencePat(_, _) | RightIgnoringSequencePat(_, _, _) =>
            res = false
            remainingCases = emptySymbolSet

          case ConstantPat(_) =>
            res = false

          case AltPat(branchesHeader) =>
            res = false
          //Console.println("----------bfore: "+coveredCases)
            branchesHeader.forEachBranch(traverse) // branchesHeader is header
          //Console.println("----------after: "+coveredCases)

          case VariablePat(_) =>
            res = false

          case _:Header | _:Body =>
            Predef.error("cannot happen")
        }
      }

      this.forEachBranch(traverse)
      return (res && (cases > 2), coveredCases, remainingCases)
    } // def optimize

  }

  /** contains at least one body, so arrays are always nonempty
   */
  class Body(bound1: Array[Array[ValDef]], guard1:Array[Tree], body1:Array[Tree]) extends PatternNode {
    var bound = bound1
    var guard = guard1
    var body = body1

    def hasUnguarded = guard.exists { x => x == EmptyTree }

    def isSingleUnguarded = (guard.length == 1) && (guard(0) == EmptyTree) && (bound(0).length == 0)
  }

  case class DefaultPat()extends PatternNode
  case class ConstrPat(override val casted:Symbol) extends PatternNode
  case class UnapplyPat(override val casted:Symbol, fn:Tree) extends PatternNode {

  override def symbol2bind = NoSymbol

    def returnsOne =  {
      /*val res =*/ definitions.getProductArgs(casted.tpe) match {
        case Some(Nil) => true     // n = 0
        case Some(x::Nil) => true  // n = 1
        case Some(_) => false
        case _ => true
      }
      //Console.println("returns one? "+casted.tpe)
      //Console.println(" I say: "+res)
      //res
    }
  }
  case class ConstantPat(value: Any /*AConstant*/) extends PatternNode
  case class VariablePat(tree: Tree) extends PatternNode
  case class AltPat(subheader: Header) extends PatternNode
  case class SequencePat(override val casted: Symbol, len: int) extends PatternNode // only used in PatternMatcher

  case class RightIgnoringSequencePat(override val casted: Symbol, castedRest: Symbol, minlen: int) extends PatternNode //PM

  /** the environment for a body of a case
   * @param owner the owner of the variables created here
   */
  class CaseEnv {
//    (val owner:Symbol, unit:CompilationUnit)
    private var boundVars: Array[ValDef] = new Array[ValDef](4)
    private var numVars = 0

    /** substitutes a symbol on the right hand side of a ValDef
     *
     *  @param oldSymm ...
     *  @param newInit ...
     */
    def substitute(oldSym: Symbol, newInit: Tree): Unit = {
      var i = 0; while (i < numVars) {
        if (boundVars(i).rhs.symbol == oldSym) {
          boundVars(i) = ValDef(boundVars(i).symbol, newInit)
          return
        }
        i = i + 1
      }
    }

    /**
     *  @param sym  ...
     *  @param tpe  ...
     *  @param init ...
     */
    def newBoundVar(sym: Symbol, tpe: Type, init: Tree): Unit = {
      //if(sym == Symbol.NoSymbol ) {
      //  scala.Predef.Error("can't add variable with NoSymbol");
      //}
      //sym.setOwner( owner ); // FIXME should be corrected earlier
      //@maybe is corrected now? bq
      if (numVars == boundVars.length) {
        val newVars = new Array[ValDef](numVars * 2)
        Array.copy(boundVars, 0, newVars, 0, numVars)
        this.boundVars = newVars
      }
      sym.setInfo(tpe)
      this.boundVars(numVars) = ValDef(sym, init.duplicate)
      numVars = numVars + 1
    }

    def getBoundVars(): Array[ValDef] = {
      val newVars = new Array[ValDef](numVars)
      Array.copy(boundVars, 0, newVars, 0, numVars)
      newVars
    }

    override def equals(obj: Any): Boolean = {
      if (!(obj.isInstanceOf[CaseEnv]))
        return false
      val env = obj.asInstanceOf[CaseEnv]
      if (env.numVars != numVars)
        return false
      var i = 0; while (i < numVars) {
        if ((boundVars(i).name != env.boundVars(i).name) ||
	    !isSameType(boundVars(i).tpe, env.boundVars(i).tpe) ||
	    (boundVars(i).rhs != env.boundVars(i).rhs))
	  return false
        i = i + 1
      }
      true
    }

  } // class CaseEnv

}
