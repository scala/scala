/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scala.tools.nsc.matching ;

import scala.tools.util.Position;

abstract class PatternNodes {

  val global: Global;

  import global._;

  /** Intermediate data structure for algebraic + pattern matcher
   */
  class PatternNode {
  var pos = Position.FIRSTPOS;
  var tpe: Type  = _;
  var or: PatternNode = _;
  var and: PatternNode = _;

  def getTpe(): Type = {
    tpe;
  }
  def setType(tpe: Type): Unit = {
    this.tpe = tpe;
  }

  def dup(): PatternNode = {
    var res: PatternNode = _;
    this match {
      case h:Header =>
        res = new Header(h.selector, h.next);

      case b:Body=>
      res = new Body(b.bound, b.guard, b.body);

      case DefaultPat() =>
        res = DefaultPat();

      case ConstrPat(casted) =>
        res = ConstrPat(casted);

      case SequencePat(casted, len) =>
        res = SequencePat(casted, len);

      case SeqContainerPat(casted, seqpat) =>
        res = SeqContainerPat(casted, seqpat);

      case ConstantPat(value) =>
        res = ConstantPat(value);

      case VariablePat(tree) =>
        res = VariablePat(tree);

      case AltPat(subheader) =>
        res = AltPat(subheader);

      case _ =>
        error("")
    }
    res.pos = pos;
    res.tpe = tpe;
    res.or = or;
    res.and = and;
    res;
  }

  def symbol: Symbol = {
    this match {
      case ConstrPat(casted) =>
        return casted;
      case SequencePat(casted, _) =>
        return casted;
      case SeqContainerPat(casted, _) =>
        return casted;
      case _ =>
        return NoSymbol; //.NONE;
    }
  }

  def nextH(): PatternNode = {
    this match {
      case _h:Header =>
        return _h.next;
      case _ =>
        return null;
    }
  }

  def isDefaultPat(): boolean = {
    this match {
      case DefaultPat() =>
	return true;
      case _ =>
        return false;
    }
  }

   /** returns true if
    *  p and q are equal (constructor | sequence) type tests, or
    *  "q matches" => "p matches"
    */
   def isSameAs(q: PatternNode): boolean = {
     this match {
       case ConstrPat(_) =>
         q match {
           case ConstrPat(_) =>
             isSameType(q.getTpe(), this.getTpe());
           case _ =>
             false
         }
       case SequencePat(_, plen) =>
         q match {
           case SequencePat(_, qlen) =>
             return (plen == qlen) && isSameType(q.getTpe(), this.getTpe());
           case _ =>
             false
         }
       case _ =>
         subsumes(q);
     }
   }

   /** returns true if "q matches" => "p matches"
    */
   def subsumes(q:PatternNode): Boolean = {
     this match {
       case DefaultPat() =>
         q match {
           case DefaultPat() =>
             true;
           case _ =>
             false;
         }
       case ConstrPat(_) =>
         q match {
           case ConstrPat(_) =>
              isSubType(q.getTpe(), this.getTpe());
           case _ =>
             false;
         }
       case SequencePat(_, plen) =>
         q match {
           case SequencePat(_, qlen) =>
              (plen == qlen) && isSubType(q.getTpe(), this.getTpe());
           case _ =>
             false;
         }
       case ConstantPat(pval) =>
         q match {
           case ConstantPat(qval) =>
              pval.equals(qval);
           case _ =>
             false;
         }
       case VariablePat(tree) =>
         q match {
           case VariablePat(other) =>
              (tree.symbol != null) &&
           (tree.symbol != NoSymbol) &&
           (!tree.symbol.isError) &&
           (tree.symbol == other.symbol);
           case _ =>
             false;
         }
       case _ =>
         false;
     }
   }

     override def toString(): String  = {
       this match {
         case _h:Header =>
           return "Header(" + _h.selector + ")";
         case _b:Body =>
           return "Body";
         case DefaultPat() =>
           return "DefaultPat";
         case ConstrPat(casted) =>
           return "ConstrPat(" + casted + ")";
         case SequencePat(casted,  len) =>
           return "SequencePat(" + casted + ", " + len + "...)";
         case SeqContainerPat(casted, seqpat) =>
           return "SeqContainerPat(" + casted + ", " + seqpat + ")";
         case ConstantPat(value) =>
           return "ConstantPat(" + value + ")";
         case VariablePat(tree) =>
           return "VariablePat";
         case _ =>
           return "<unknown pat>";
       }
     }
  }

  class Header(sel1: Tree, next1: Header ) extends PatternNode {
    var selector: Tree = sel1;
    var next: Header = next1;
  }

  class Body(bound1: Array[Array[ValDef]] , guard1:Array[Tree] , body1:Array[Tree] ) extends PatternNode {
    var bound = bound1;
    var guard = guard1;
    var body = body1;
  }

  case class DefaultPat()extends PatternNode;
  case class ConstrPat(casted:Symbol ) extends PatternNode;
  case class ConstantPat(value: Any /*AConstant*/ ) extends PatternNode;
  case class VariablePat(tree: Tree ) extends PatternNode;
  case class AltPat(subheader: Header ) extends PatternNode;
  case class SequencePat( casted: Symbol,  len:int) extends PatternNode; // only used in PatternMatcher
  case class SeqContainerPat(casted: Symbol ,  seqpat: Tree ) extends PatternNode; //   in AlgebraicMatcher

  /** the environment for a body of a case
   * @param owner the owner of the variables created here
   */
  class CaseEnv {
//    (val owner:Symbol, unit:CompilationUnit)
  private var boundVars:scala.Array[ValDef] = new Array[ValDef](4);
  private var numVars = 0;

  /** substitutes a symbol on the right hand side of a ValDef
   */
   def substitute(oldSym: Symbol, newInit: Tree): Unit = {
     var i = 0; while( i < numVars) {
       if( boundVars(i).rhs.symbol == oldSym ) {
         boundVars(i) = ValDef(boundVars(i).symbol,  newInit);
         return;
       }
       i = i + 1;
     }
   }

  def newBoundVar(sym:Symbol, tpe: Type, init:Tree ): Unit = {
    //    sym.setOwner( owner ); // FIXME should be corrected earlier
    // @maybe is corrected now? bq
    if (numVars == boundVars.length) {
      val newVars = new Array[ValDef](numVars * 2);
      System.arraycopy(boundVars, 0, newVars, 0, numVars);
      this.boundVars = newVars;
    }
    sym.setInfo(tpe);
    this.boundVars(numVars) = ValDef(sym, init.duplicate);
    numVars = numVars + 1;
  }

    def getBoundVars(): Array[ValDef] = {
      val newVars = new Array[ValDef](numVars);
      System.arraycopy(boundVars, 0, newVars, 0, numVars);
      return newVars;
    }

    override def equals(obj: Any): Boolean = {
      if (!(obj.isInstanceOf[CaseEnv]))
        return false;
      val env = obj.asInstanceOf[CaseEnv];
      if (env.numVars != numVars)
        return false;
      var i = 0; while(i < numVars) {
        if ((boundVars(i).name != env.boundVars(i).name) ||
	    !isSameType(boundVars(i).tpe, env.boundVars(i).tpe) ||
	    (boundVars(i).rhs != env.boundVars(i).rhs))
	  return false;
        i = i + 1;
      }
      return true;
    }

  } // class CaseEnv
}
