package scala.runtime.matching ;

import scala.collection.Set;
import scala.collection.Map ;
import scala.collection.immutable;
import scala.collection.mutable;

/** runtime representation of patterns. This class treats all variable
 *  indices as sequence variables
 *  @caseVars an array, field i holding the number of variables in case i
 */
class Grammar {

  var treeTransitions:  Array[immutable.Set[TRule]] = _; /*i:(i,test,hnt) */
  var hedgeTransitions: Array[immutable.Set[HRule]] = _; /*i:(i,tnt,hnt)*/
  /** for cases 1..n, holds max. variable index */
  var vars:Array[Int] = _;
  //val vsetTree:Array[Set[Int]] ;
  //val vsetHedge:Array[Set[Int]] ;

  var treeInitials:Set[TreeNT]  = _;
  var hedgeInitials:Set[HedgeNT] = _;

  def test(test:int, inp:Any):boolean = false;

  final def isSequenceType = treeInitials.isEmpty;

  /*
  protected def allTreeNT: Array[TreeNT] = {
    val ar = new Array[TreeNT]( treeTransitions.length );
    for( val tr <- treeTransitions ) {
      tr.elements.next match {
        case TreeRule(n,_,_)  =>
        case AnyTreeRule(n)   =>
        case AnyNodeRule(n,_) =>
      }
    }
  }
  */
  //val hedgeNT:          Array[HedgeNT];

  final def encode:Array[Byte] = {
    val bos = new java.io.ByteArrayOutputStream();
    val obos = new java.io.ObjectOutputStream(bos);
    obos.writeObject(this);
    bos.toByteArray();
  }
  /*
  var i = 0;
    val sb = new StringBuffer();
    sb.append( treeTransitions.length.toString() )
    sb.append('#')
    while( i < treeTransitions.length ) {
      sb.append( Grammar.encodeTRuleSet( treeTransitions( i )));
      sb.append('#')
      i = i + 1;
    }
    sb.append( hedgeTransitions.length.toString() )
    sb.append('#')
    i = 0;
    while( i < hedgeTransitions.length ) {
      sb.append( Grammar.encodeHRuleSet( hedgeTransitions( i )));
      i = i + 1;
    }
    sb.append('#')
  }
  */

}


/*
object Grammar {

    new Grammar  {
      override val treeTransitions  = theTreeTransitions;
      override val hedgeTransitions = theHedgeTransitions;
      override val vars             = theVars;
      override val treeInitials     = null;//theTreeInitials;
      override val hedgeInitials    = null;//theHedgeInitials;
      // @todo
      //def test( test:int, inp:Any ):boolean = { false; };
    }
  }


  final def decodeTRuleSet(trules: String): immutable.Set[TRule]= {
    new immutable.ListSet[TRule]();
  }

  final def decodeHRuleSet(trules: String): immutable.Set[HRule]= {
    new immutable.ListSet[HRule]();
  }

  final def encodeTRuleSet(trules: immutable.Set[TRule]):String = {
    "";
  }

  final def encodeHRuleSet(trules: immutable.Set[TRule]):String = {
    "";
  }

}
*/


  /*
  object & {
    final def x (s:String): Int = Integer.parseInt(s, 16);
  }
  def decode(s: String): Grammar = {
    val z:Array[String] = s.split("#");
    var i = 0;
    val tlength = & x z(0) ;
    var j = 1;
    var theTreeTransitions = new Array[immutable.Set[TRule]]( tlength );
    while( i < tlength ) {
      theTreeTransitions.update( i, Grammar.decodeTRuleSet(z( j )));
      i = i + 1;
      j = j + 1;
    }
    val hlength = & x z(j);
    j = j + 1;
    var theHedgeTransitions = new Array[immutable.Set[HRule]]( hlength );
    i = 0;
    j = j + 1;
    while( i < hlength ) {
      theHedgeTransitions.update( i, Grammar.decodeHRuleSet(z( j )));
      i = i + 1;
      j = j + 1;
    }
    val vlength = & x z(j);
    j = j + 1;
    var theVars = new Array[Int]( vlength );
    i = 0;
    while( i < vlength ) {
      theVars.update( i, Integer.parseInt(z( j ),16));
      i = i + 1;
      j = j + 1;
    }

    val tilength = & x z(j) ;
    j = j + 1;
    var theTreeInitials = new Array[TreeNT]( tilength );
    i = 0;
    while( i < vlength ) {
      theTreeInitials.update( i, new TreeNT( & x z( j )));
      i = i + 1;
      j = j + 1;
    }


    val hilength = & x z(j) ;
    j = j + 1;
    var theHedgeInitials = new Array[HedgeNT]( hilength );
    i = 0;
    while( i < hilength ) {
      val hi = & x z( j );
      theHedgeInitials.update( i, {
        if( hi < 0 )
          new HedgeNT(-hi, true);
        else
          new HedgeNT(hi, false);
      });
      i = i + 1;
      j = j + 1;
    }
  */
