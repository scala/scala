
import scala.collection.immutable;
import scala.collection.mutable;
import scala.runtime.matching._;
import scalac.symtab._;

import scala.util.grammar.{ AnyTreeRHS, LabelledRHS, TreeRHS, ConsRHS, HedgeRHS };

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
        case HedgeRule( h, _, _ ) if ( make.hedgeInitials contains h ) => "*"
        case _ => " "
      }) + r.toString() + "\n"
    } ;
  }

  final def treeRulesToString:String = {
    treeRules.foldLeft ("") {
      (s:String,r:Rule) => s + (r match {
        case TreeRule( t, _, _ ) if ( make.treeInitials contains t ) => "*"
        case _ => " "
      }) + r.toString() + "\n"
    } ;
  }

  override def toString(): String = {
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

  protected def add[A]( i:Int, r:A, m:immutable.TreeMap[Int,immutable.Set[A]] ):immutable.TreeMap[Int,immutable.Set[A]] =
    m.update( i, m.get( i ) match {
      case Some( s ) => s + r;
      case None => immutable.ListSet.Empty[A] + r;
    });

  /** converts this grammar in a pattern grammar (an ImmutableTreeHedgeGrammar with variable info)
  */
  def toGrammar:PatternGrammar = {
    val rbsNullable = new mutable.ResizableBitSet();
    val _treeTransitions:Array[immutable.Set[TreeRHS]] = {
      val theTreeTransitionsMap: immutable.TreeMap[Int,immutable.Set[TreeRHS]] = {
        var tmp =
          immutable.TreeMap.Empty[Int,immutable.Set[TreeRHS]];
        treeRules.foreach { treeRule => treeRule match {
          case AnyTreeRule( t ) => tmp = add( t.i, AnyTreeRHS, tmp );
          //case AnyNodeRule( t, _ ) => tmp = add( t.i, treeRule, tmp ) // ??!!!!!!!!!!!!!!!!!!!!!!
          case TreeRule( t, i, h ) => tmp = add( t.i, LabelledRHS(TestLabel(i),h.i), tmp )
        }};
        tmp
      };
      val arr = new Array[immutable.Set[TreeRHS]]( theTreeTransitionsMap.size );
      val it = theTreeTransitionsMap.keys;
      while( it.hasNext ) {
        val k = it.next;
        arr.update( k, theTreeTransitionsMap( k ));
      }
      arr
    }
    val _nTreeNT        = _treeTransitions.length;

    val _hedgeTransitions: Array[immutable.Set[HedgeRHS]] = {
      val theHedgeTransitionsMap: immutable.TreeMap[Int,immutable.Set[HedgeRHS]] = {
        var tmp =
          immutable.TreeMap.Empty[Int,immutable.Set[HedgeRHS]];
        hedgeRules.foreach { hedgeRule => hedgeRule match {
          case HedgeRule( h, t, h2 ) =>
            rbsNullable.ensureSize( h2.i );
            rbsNullable.set( h2.i, h2.nullable );
            tmp = add( h.i, ConsRHS(t.i,h2.i), tmp );
          case HedgeChainRule( h, _ ) => throw new RuntimeException();
          //tmp = add( h.i, hedgeRule, tmp );
        }};
        // to maintain assumption that every hedge nt is present
        tmp.update( 0, immutable.ListSet.Empty[HedgeRHS] );
      };
      val arr = new Array[immutable.Set[HedgeRHS]]( theHedgeTransitionsMap.size );
      val it = theHedgeTransitionsMap.keys;
      while( it.hasNext ) {
        val k = it.next;
        arr.update( k, theHedgeTransitionsMap( k ));
      }
      arr
    }
    val _nHedgeNT       = _hedgeTransitions.length ;

    val _vars = new Array[Int]( caseVars.size );
    val it = caseVars.keys;
    while( it.hasNext ) {
      val k = it.next;
      _vars.update( k, caseVars( k ));
    }

    val _treeInitials = {
      val rbs = new mutable.ResizableBitSet( _nTreeNT );
      for( val k <- make.treeInitials ) {
        rbs.set( k.i )
      }
      new immutable.BitSet(rbs)
    }

    val _hedgeInitials = {
      val rbs = new mutable.ResizableBitSet( _nHedgeNT );
      for( val k <- make.hedgeInitials ) {
        rbsNullable.ensureSize( k.i );
        rbsNullable.set( k.i, k.nullable );
        rbs.set( k.i )
      }
      new immutable.BitSet(rbs)
    }
    val _isNullable = new immutable.BitSet( rbsNullable );
    new PatternGrammar {
      val nTreeNT          = _nTreeNT;
      val nHedgeNT         = _nHedgeNT;
      val treeTransitions  = _treeTransitions;
      val hedgeTransitions = _hedgeTransitions;
      val vars             = _vars;
      val treeInitials     = _treeInitials;
      val hedgeInitials    = _hedgeInitials;
      val isNullable       = _isNullable;
      // @todo
      def test( test:int, inp:Any ):boolean = { false; };
    }
  }

}

}
