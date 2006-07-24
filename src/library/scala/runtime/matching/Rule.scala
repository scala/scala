/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime.matching ;


/* hedge grammar rules */
abstract class Rule extends Ordered[Rule] {

  def compare (that: Rule): int = {
    if( rule_smaller( this, that ) )
      -1
    else if( rule_eq( this, that ) )
      0
    else
      1
  }

  final def rule_smaller( r1:Rule, r2:Rule ):boolean = r1 match {
    case HedgeRule( h1, _, hh1 ) => r2 match {
      case HedgeRule( h2, _, hh2 ) =>
        ((h1 == h2)&&( hh1.i < hh2.i )) || h1.i < h2.i;
      case HedgeChainRule( h2, hh2 ) =>
        ((h1 == h2)&&( hh1.i < hh2.i )) || h1.i < h2.i;
      case _ => false;
    }
    case HedgeChainRule( h1, hh1 ) => r2 match {
      case HedgeRule( h2, _, hh2 ) =>
        ((h1 == h2)&&( hh1.i < hh2.i )) || h1.i < h2.i;
      case HedgeChainRule( h2, hh2 ) =>
        ((h1 == h2)&&( hh1.i < hh2.i )) || h1.i < h2.i;
      case _ => false;
    }
    case TreeRule( t1, _, hh1 ) => r2 match {
      case TreeRule( t2, _, hh2 ) =>
        ((t1 == t2 )&&(hh1.i < hh2.i )) || t1.i < t2.i;
      case AnyTreeRule( t2 ) => false;
      case AnyNodeRule( t2, hh2 ) =>
        ((t1 == t2 )&&(hh1.i < hh2.i )) || t1.i < t2.i;
      case _ => true;
    }
    case AnyTreeRule( t1 ) => r2 match {
      case TreeRule( _, _ , _ ) => true;
      case AnyTreeRule( t2 ) => t1.i < t2.i;
      case AnyNodeRule( t2, _ ) => true
      case _ => true;
    }
    case AnyNodeRule( t1, hh1 ) => r2 match {
      case TreeRule( t2, _, hh2 ) =>
        ((t1 == t2 )&&(hh1.i < hh2.i )) || t1.i < t2.i;
      case AnyTreeRule( t2 ) => false;
      case AnyNodeRule( t2, hh2 ) =>
        ((t1 == t2 )&&(hh1.i < hh2.i )) || t1.i < t2.i;
      case _ => true;
    }
  };
  final def rule_eq( r1:Rule, r2:Rule ):boolean = r1 == r2;


  override def toString() = this match {
    case HedgeChainRule( n, m ) =>
      n.toString()+" ::= "+m.toString();

    case TreeRule( n, label, n2 )  =>
      (n.toString()+{ if( !n.vset.isEmpty ) n.vset.toString() else "" }+
       " ::= "+label+"( "+n2.toString()+{if( n2.nullable ) "~" else ""}+" )")

    case AnyTreeRule( n ) =>
      n.toString()+{ if( !n.vset.isEmpty ) n.vset.toString() else "" }+" ::= _ ";

    case AnyNodeRule( n, h ) =>
      n.toString()+{ if( !n.vset.isEmpty ) n.vset.toString() else "" }+" ::= _ ( "+h.toString()+" )";

    case HedgeRule( n, t, h ) =>
      n.toString()+(
        if( n.nullable ) "~" else " "
      )+" ::= "+(
        if( t == ANYTREE ) "_" else t.toString()
      )+" "+h.toString();

  }
}

abstract class TRule extends Rule;
abstract class HRule extends Rule;
/*
a tree rule is of the from  A -> s(B)
where A,B are TreeNTs and s is an identifier (string).

If s is the empty string, then the node label is arbitrary
If HedgeNT is AnyHedgeNT, then the tree is arbitrary
*/
case class HedgeChainRule( n: HedgeNT, rhs: HedgeNT ) extends HRule;
case class TreeRule( n:TreeNT, test:Int, h:HedgeNT ) extends TRule {
  def this(i:Int, s:Int, n:Int ) = {
    this( new TreeNT(i), s, new HedgeNT(n));
  }
};
case class AnyTreeRule( n:TreeNT ) extends TRule {
}
case class AnyNodeRule( n:TreeNT, h:HedgeNT ) extends TRule {
}
case class HedgeRule( n:HedgeNT, t:TreeNT, h:HedgeNT ) extends HRule;

