/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scala.tools.nsc.matching ;

import scala.tools.nsc.util.Position;

trait PatternNodes requires TransMatcher {

  import global._;

  /** Intermediate data structure for algebraic + pattern matcher
   */
  class PatternNode {
  var pos = FirstPos;
  var tpe: Type  = _;
  var or: PatternNode = _;
  var and: PatternNode = _;

   def bodyToTree(): Tree = this match {
     case _b:Body =>
       return _b.body(0);
   }

  def getTpe(): Type = {
    tpe;
  }
  def setType(tpe: Type): Unit = {
    this.tpe = tpe;
  }

  def dup(): PatternNode = {
    var res: PatternNode = null;
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
              pval == qval;
           case _ =>
             false;
         }
       case VariablePat(tree) =>
         q match {
           case VariablePat(other) =>
             ((tree.symbol != null) &&
              (tree.symbol != NoSymbol) &&
              (!tree.symbol.isError) &&
              (tree.symbol == other.symbol))
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
         case RightIgnoringSequencePat(casted, castedRest,  minlen) =>
           return "RightIgnoringSequencePat(" + casted + ", " + castedRest + ", "+ minlen + "...)";
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

  def print(indent: String, sb: StringBuffer): StringBuffer = {

    val patNode = this;

    def cont = if (patNode.or != null) patNode.or.print(indent, sb); else sb;

    def newIndent(s: String) = {
      val removeBar: Boolean = (null == patNode.or);
      val sb = new StringBuffer();
      sb.append(indent);
      if (removeBar)
        sb.setCharAt(indent.length() - 1, ' ');
      var i = 0; while (i < s.length()) {
        sb.append(' ');
        i = i + 1
      }
      sb.toString()
    }

    if (patNode == null)
      sb.append(indent).append("NULL");
    else
      patNode match {

        case _h: Header =>
          val selector = _h.selector;
          val next = _h.next;
          sb.append(indent + "HEADER(" + patNode.getTpe() +
                          ", " + selector + ")").append('\n');
          patNode.or.print(indent + "|", sb);
          if (next != null)
            next.print(indent, sb);
          else
            sb
        case ConstrPat(casted) =>
          val s = ("-- " + patNode.getTpe().symbol.name +
                   "(" + patNode.getTpe() + ", " + casted + ") -> ");
          val nindent = newIndent(s);
          sb.append(nindent + s).append('\n');
          patNode.and.print(nindent, sb);
          cont;

        case SequencePat( casted, plen ) =>
          val s = ("-- " + patNode.getTpe().symbol.name + "(" +
                   patNode.getTpe() +
                   ", " + casted + ", " + plen + ") -> ");
          val nindent = newIndent(s);
          sb.append(indent + s).append('\n');
          patNode.and.print(nindent, sb);
          cont;

        case DefaultPat() =>
          sb.append(indent + "-- _ -> ").append('\n');
          patNode.and.print(indent.substring(0, indent.length() - 1) +
                      "         ", sb);
          cont;

        case ConstantPat(value) =>
          val s = "-- CONST(" + value + ") -> ";
          val nindent = newIndent(s);
          sb.append(indent + s).append('\n');
          patNode.and.print( nindent, sb);
          cont;

        case VariablePat(tree) =>
          val s = "-- STABLEID(" + tree + ": " + patNode.getTpe() + ") -> ";
          val nindent = newIndent(s);
          sb.append(indent + s).append('\n');
          patNode.and.print(nindent, sb);
          cont;

        case AltPat(header) =>
          sb.append(indent + "-- ALTERNATIVES:").append('\n');
          header.print(indent + "   * ", sb);
          patNode.and.print(indent + "   * -> ", sb);
          cont;

        case _b:Body =>
          if ((_b.guard.length == 0) && (_b.body.length == 0))
            sb.append(indent + "true").append('\n') ;
          else
            sb.append(indent + "BODY(" + _b.body.length + ")").append('\n');

      }
  } // def print

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
  case class SequencePat(casted: Symbol,  len:int) extends PatternNode; // only used in PatternMatcher

  case class RightIgnoringSequencePat(casted: Symbol, castedRest: Symbol, minlen: int) extends PatternNode; //PM
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
    //if(sym == Symbol.NoSymbol ) {
//		scala.Predef.Error("can't add variable with NoSymbol");
//	}
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
