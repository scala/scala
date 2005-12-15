/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */


import scala.tools.util.Position;
import scalac._;
import scalac.ast._;
import scalac.atree.AConstant;
import scalac.symtab._;
import scalac.typechecker._;

package scala.tools.scalac.transformer.matching {

/** Intermediate data structure for algebraic + pattern matcher
 */
 class PatternNode {
   var pos = Position.FIRSTPOS;
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

       //case AltPat(subheader) =>
       //  res = AltPat(subheader);

       case _ =>
         throw new ApplicationError();
     }
     res.pos = pos;
     res.tpe = tpe;
     res.or = or;
     res.and = and;
     return res;
   }

    def symbol(): Symbol = {
        this match {
          case ConstrPat(casted) =>
            return casted;
          case SequencePat(casted, _) =>
            return casted;
          case SeqContainerPat(casted, _) =>
            return casted;
          case _ =>
            return Symbol.NONE;
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
     //Console.println("isSameAs("+this+","+q+")");
     this match {
       case ConstrPat(_) =>
         q match {
           case ConstrPat(_) =>
             q.getTpe().isSameAs(this.getTpe());
           case _ =>
             false
         }
       case SequencePat(_, plen) =>
         q match {
           case SequencePat(_, qlen) =>
             return (plen == qlen) && q.getTpe().isSameAs(this.getTpe());
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
     //Console.print("subsumes("+this+","+q+")");
     /* val res = */
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
              q.getTpe().isSubType(this.getTpe());
           case _ =>
             false;
         }
       case SequencePat(_, plen) =>
         q match {
           case SequencePat(_, qlen) =>
              (plen == qlen) && q.getTpe().isSubType(this.getTpe());
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
             val treesym = tree match {
               case _:Tree.Ident => tree.symbol();
               case _:Tree.Apply => TreeInfo.methSymbol( tree );
             }
             val othersym =  tree match {
               case _:Tree.Ident => tree.symbol();
               case _:Tree.Apply => TreeInfo.methSymbol( tree );
             }
/*
             Console.println("VPat, getc = "+tree.getClass());
             Console.println("[tree.symbol() = "+tree.symbol());
           if (tree.symbol() != null) {
             Console.println("[other.symbol() = "+other.symbol());
             Console.println("[ ==? "+(tree.symbol() == other.symbol()));

             Console.println("\n\n[treesym != null? "+ tree.symbol() != null);
             Console.println("\n\n[!treesym.isNone? "+ !tree.symbol().isNone());
             Console.println("\n\n[!treesym.isError? "+ !tree.symbol().isError());
           }
*/
              (treesym != null) &&
           (!treesym.isNone()) &&
           (!treesym.isError()) &&
           (treesym == othersym);
           case _ =>
             false;
         }
       case _ =>
         false;
     }
     /*Console.println("="+res);
     return res;*/
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

  class Body(bound1: Array[Array[Tree.ValDef]] , guard1:Array[Tree] , body1:Array[Tree] ) extends PatternNode {
    var bound = bound1;
    var guard = guard1;
    var body = body1;
  }

  case class DefaultPat()extends PatternNode;
  case class ConstrPat(casted:Symbol )extends PatternNode;
  case class ConstantPat(value:AConstant )extends PatternNode;
  case class VariablePat(tree:Tree )extends PatternNode;
  //case class AltPat(subheader:Header )extends PatternNode;
  case class SequencePat( casted:Symbol,  len:int)extends PatternNode; // only used in PatternMatcher
  case class SeqContainerPat(casted:Symbol ,  seqpat:Tree)extends PatternNode; //   in AlgebraicMatcher
  }
