
import scala.collection.immutable;
import scala.collection.mutable;
import scala.runtime.matching._;

import scalac.symtab.Symbol;

package scala.tools.scalac.transformer.matching {

object InitialGrammar {
  /** returns an initial grammar, with any tree rule and any hedge rule */
  def make:MutableGrammar = {
    val initialTreeRules = new mutable.HashSet[TRule]();
    initialTreeRules += AnyTreeRule( ANYTREE );
    val initialHedgeRules = new mutable.HashSet[HRule]();
    val vars = new mutable.HashMap[Int,Int]();
    val tests = new mutable.HashMap[Int,PatternTest]() ;
    val z = new MutableGrammar(
      initialTreeRules,
      initialHedgeRules,
      vars,
      tests,
      new NonTermFactory
    );
    z.varMap = new mutable.HashMap[Symbol, Int]();
    z.invTest = new mutable.HashMap[PatternTest, Int]();
    z
  }
}


/** compile-time representation of patterns. This class treats all variable
 *  indices as sequence variables.
 */
case class MutableGrammar( treeRules:mutable.Set[TRule],
                           hedgeRules:mutable.Set[HRule],
                           caseVars:mutable.Map[Int,Int],
                           tests:mutable.Map[int,PatternTest],
                           make:NonTermFactory )
{

  var invTest:mutable.Map[PatternTest,int] = _;

  /** from variable names to indices. */
  var varMap:mutable.Map[Symbol, Int] = _;

  final def hedgeRulesToString:String = {
    hedgeRules.foldLeft ("") {
      (s:String,r:Rule) => s + (r match {
        case HedgeRule( h, _ ) if ( make.hedgeInitials contains h ) => "*"
        case _ => " "
      }) + r.toString() + "\n"
    } ;
  }

  final def treeRulesToString:String = {
    treeRules.foldLeft ("") {
      (s:String,r:Rule) => s + (r match {
        case TreeRule( t, _ ) if ( make.treeInitials contains t ) => "*"
        case _ => " "
      }) + r.toString() + "\n"
    } ;
  }

  def toString:String = {
    val sb = new StringBuffer();
    sb.append( { if( treeRules.isEmpty )
      "Set of rules is EMPTY.\n" else treeRulesToString });
    sb.append( { if( hedgeRules.isEmpty )
      "Set of rules is EMPTY.\n" else hedgeRulesToString });

    sb.append( "hedgeInitials:"+make.hedgeInitials );
    sb.append( "treeInitials:"+make.treeInitials );

    //sb.append( "\nvariables :"+( caseVars( 0 ).map { debugVarMapping }) );
    sb.toString()
  }

  protected def add( i:Int, r:Rule, m:immutable.TreeMap[Int,immutable.Set[Rule]] ) =
    m.update( i, m.get( i ) match {
      case Some( s ) => s + r;
      case None => immutable.ListSet.Empty[Rule] + r;
    });



  /** converts this grammar in a immutable grammar
  */
  def toGrammar:Grammar = {
    val treeTransitions:immutable.TreeMap[Int,immutable.Set[Rule]] = {
      var tmp =
        immutable.TreeMap.Empty[Int,immutable.Set[Rule]];
      treeRules.foreach { treeRule => treeRule match {
        case AnyTreeRule( t ) => tmp = add( t.i, treeRule, tmp );
        case AnyNodeRule( t, _ ) => tmp = add( t.i, treeRule, tmp )
        case TreeRule( t, _ ) => tmp = add( t.i, treeRule, tmp )
      }};
      tmp
    };
    val hedgeTransitions:immutable.TreeMap[Int,immutable.Set[Rule]] = {
      var tmp =
        immutable.TreeMap.Empty[Int,immutable.Set[Rule]];
      hedgeRules.foreach { hedgeRule => hedgeRule match {
        case HedgeRule( h, _ ) => tmp = add( h.i, hedgeRule, tmp );
        case HedgeChainRule( h, _ ) => tmp = add( h.i, hedgeRule, tmp );
      }};
      // to maintain assumption that every hedge nt is present
      tmp.update( 0, immutable.ListSet.Empty[Rule] );
    };
    val theCaseVars = new Array[Int]( caseVars.size );
    for( val k <- caseVars.keys ) {
      theCaseVars( k ) = caseVars( k );
    }
    new Grammar( treeTransitions, hedgeTransitions, theCaseVars ) {
      val treeInitials = make.treeInitials;
      val hedgeInitials = make.hedgeInitials;
      // @todo
      def test( test:int, inp:Any ):boolean = {
        false;
      };
    }
  }

}

}
