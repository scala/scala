
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
      new NonTermFactory( ANYTREE.i+1, ANYHEDGE.i+1 )
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
  def toGrammar: PatternGrammar = {
    val rbsNullable = new mutable.BitSet();
    var tnt = 0;
    var renumber = new immutable.TreeMap[Int,Int];
    var invnum  = new immutable.TreeMap[Int,Int];
    val _treeTransitions:Array[immutable.Set[TreeRHS]] = {
      val theTreeTransitionsMap: immutable.TreeMap[Int,immutable.Set[TreeRHS]] = {
        var tmp = immutable.TreeMap.Empty[Int,immutable.Set[TreeRHS]];
        val it = treeRules.elements;
        while(it.hasNext) {
          val tr = it.next;
          //Console.print(tr.toString()+" ###");
          tr match {
            case AnyTreeRule( t ) =>
              tmp = add( tnt, AnyTreeRHS, tmp );
              //Console.println(tnt);
              //Console.println(tmp);
              renumber = renumber.update(t.i, tnt);
              invnum   = invnum.update(tnt, t.i);
              tnt = tnt+1;
            case AnyNodeRule( t, _ ) =>
              Console.println("?!?! "+t);
              // ?!
              throw new RuntimeException("cannot handle!");
            case TreeRule( t, i, h ) =>
              tmp = add( tnt, LabelledRHS(TestLabel(i),h.i), tmp );

              //Console.println(tnt);
              //Console.println(tmp);
              renumber = renumber.update(t.i, tnt);
              invnum   = invnum.update(tnt, t.i);
              tnt = tnt+1;
          }
        }
        tmp
      };
      //Console.println("map size"+theTreeTransitionsMap.size);
      //Console.println("mumap size"+treeRules.size);
      val arr = new Array[immutable.Set[TreeRHS]]( theTreeTransitionsMap.size );
      val it = theTreeTransitionsMap.keys;
      while( it.hasNext ) {
        val k = it.next;
        //Console.println(k+" which is t"+invnum(k));
        arr.update( k, theTreeTransitionsMap( k ));
      }
      arr
    }
    //Console.println("renumer T "+renumber);
    //Console.println("DONE with trees");
    val _nTreeNT        = _treeTransitions.length;
    var renumberH = new immutable.TreeMap[Int,Int].update(0,0);
    val it = hedgeRules.elements;
    var hnt = 1;
    while(it.hasNext)
      it.next match {
        case HedgeRule( h, t, h2 ) =>
          if(!renumberH.contains(h.i)) {
            renumberH = renumberH.update(h.i,hnt);
            hnt = hnt + 1;
          }
          if(!renumberH.contains(h2.i)) {
            renumberH = renumberH.update(h2.i,hnt);
            hnt = hnt + 1;
          }
      }
    //Console.println("hedge renum");
    //Console.println(renumberH);
    val _hedgeTransitions: Array[immutable.Set[HedgeRHS]] = {
      val theHedgeTransitionsMap: immutable.TreeMap[Int,immutable.Set[HedgeRHS]] = {
        var tmp =
          immutable.TreeMap.Empty[Int,immutable.Set[HedgeRHS]]
            // to maintain assumption that every hedge nt is present
          .update(0, immutable.ListSet.Empty[HedgeRHS]);

        val it2 = hedgeRules.elements;
        while(it2.hasNext) {
          val hedgeRule = it2.next;
          hedgeRule match {
          case HedgeRule( h, t, h2 ) =>
            //Console.println(hedgeRule);
            //Console.println("h"+h.i+" ===> "+renumberH(h.i));
            rbsNullable.ensureSize( renumberH(h2.i) );
            rbsNullable.set( renumberH(h2.i), h2.nullable );
            tmp = add( renumberH(h.i), ConsRHS(renumber(t.i),renumberH(h2.i)), tmp );
            Console.println("tmp = "+tmp);
        }};
        tmp
      };
      Console.println("hello, "+theHedgeTransitionsMap);
      val arr = new Array[immutable.Set[HedgeRHS]]( theHedgeTransitionsMap.size );
      val it = theHedgeTransitionsMap.keys;
      while( it.hasNext ) {
        val k = it.next;
        arr.update( k, theHedgeTransitionsMap( k ));
      }
      arr
    }
    //Console.println("DONE with hedges");
    val _nHedgeNT       = _hedgeTransitions.length ;

    Console.println("caseVars: "+caseVars);
    val _vars = new Array[Int]( caseVars.size );
    val it3 = caseVars.keys;
    while( it3.hasNext ) {
      val k = it3.next;
      Console.println("caseVar: "+k);
      _vars.update( k-1, caseVars( k ));
    }

    val _treeInitials = {
      val rbs = new mutable.BitSet( _nTreeNT );
      for( val k <- make.treeInitials ) {
        rbs.set( k.i )
      }
      new immutable.BitSet(rbs)
    }

    val _hedgeInitials = {
      val rbs = new mutable.BitSet( _nHedgeNT );
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
