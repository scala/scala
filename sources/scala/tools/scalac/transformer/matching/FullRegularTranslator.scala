
import scala.runtime.matching._ ;

import scala.collection.Set;
import scala.collection.Map;
import scala.collection.mutable;
import scala.collection.immutable;

import scalac.ast._ ;
import scalac.{Global => scalac_Global}
import scalac.{symtab => scalac_symtab}
import scalac.util.Names ;



package scala.tools.scalac.transformer.matching {

  import scalac_symtab.Symbol ;
  import PatternInfo.minimalWidth;
  import TreeInfo.isSequenceValued;

/** generates from a pattern (with variable names v_1,..,v_k)
 *  a set of regular hedge grammar rules,
 *  together with an variable index mapping 1,..,k
 *  @todo: handle x @ z @ p correctly
 */
class FullRegularTranslator(global: scalac_Global) {

  /* --- real variables --- */

  /** the grammar that we build up incrementally */
  var cur:MutableGrammar = _;

  var isSequenceType = false;

  /** counter for variables indices */
  var varCounter = 0;

  /** counter for test indices */
  var testCounter = 0;

  /* initialized when Subst is encountered */
  var iteration:HedgeNT = _;

  /* initialized when Iter is encountered */
  var iterationEnd:HedgeNT = _;

  /* --- methods --- */

  /** returns a variable index for this variable */
  def newVar( vble:Symbol ):Int =
    cur.varMap.get( vble ) match {
      case Some( x ) => x
      case None      =>
        val v = varCounter;
        varCounter = v + 1;
        cur.varMap.update( vble, v );
        v
    }

  /** returns a test index for this variable */
    def newTest( test:PatternTest ):Int =
      cur.invTest.get( test ) match {
        case Some( x ) => x;
        case None =>
          val x = testCounter;
          testCounter = testCounter + 1;
          cur.invTest.update( test, x );
          cur.tests.update( x, test );
          x
      }

  /** returns a test index for this variable
  */
    def newTest( test:Tree ):Int = {
      0 // bogus
    }



  /**
    * precondition: p.length > 0
    */
  def MakeGrammar( it:Iterator[Tree$CaseDef] ):Grammar = {
    var k = 0;
    cur = InitialGrammar.make;
    while( it.hasNext )
      translate( it.next.pat, { k = k + 1; k } );
    cur.toGrammar;
  }

  /** p must be a pattern
  */
  protected def translate( p:Tree, k:Int ):unit = {

    this.varCounter = 0;
    if( isSequenceValued( p ) ) {
      this.isSequenceType = true;
      MakeHedgeRule( cur.make.initialHedgeNT, EMPTYHEDGE, emptyVars, p );
    } else {
      this.isSequenceType = false;
      val in = cur.make.initialTreeNT;
      try {
        MakeTreeRule( in, emptyVars, p );
      } catch {
        case _:WildcardException =>
	  cur.treeRules += new AnyTreeRule( in );
      }
    }

    if( global.debug ) {
      for( val rule <- cur.treeRules ) {
        global.log( rule.toString() );
      }
      for( val rule <- cur.hedgeRules ) {
        global.log( rule.toString() );
      }
      global.log("=== Now Removing Chain Rules ===");
    } ;

    RemoveChainRules;

    // update variable array with nvariable + 1
    cur.caseVars.update( k, this.varCounter );

  }

    //val s: mutable.Set[Rule] = new mutable.HashSet[Rule](); /* the grammar */

    //s += AnyTreeRule( ANYTREE );

/* TO BE DONE

Afterwards: Chain rules need not be applied anymore. They only exist with
    the unique rhs "h0" which produces empty hedge.
    So, a Chainrule of NT only means that NT can derive the empty hedge
*/

    def RemoveChainRules: unit = {
      var replaced = true;
      while (replaced==true) {
	replaced = false;
	for( val rule  <- cur.hedgeRules ) {
	  rule match {
            case HedgeChainRule( a, b ) => {
	      replaced = true;
              DEBUGPRINT("chain rule"+rule);
	      cur.hedgeRules -= rule;
	      val B = b;
              DEBUGPRINT( "b.nullable " + b.nullable);
              DEBUGPRINT( "1a.nullable" + a.nullable );
              a.nullable = a.nullable || b.nullable;
              DEBUGPRINT( "2a.nullable" + a.nullable );
	      for( val brule <- cur.hedgeRules ) {
		brule match {
		  case HedgeRule( B, t, h ) =>
		    cur.hedgeRules += new HedgeRule( a, t, h );
		  case HedgeChainRule( B, h ) =>
		    cur.hedgeRules += new HedgeChainRule( a, h )
                  case _ =>
		}
	      };
	    }
	    case _  =>
	  }

	}
      };
      //Reachables;
    }

    /*
    def Reachables: unit = {
      var reached = new mutable.HashSet[HedgeNT]();
      reached += InitialHedgeNT;
      var changed = true;
      while( changed ) {
        changed = false;
        for( val rule <- rules ) {
          rule match {
            case HedgeRule( h, _,h2 ) =>
              if( !reached.contains(h2) ) {
                changed = true;
                reached += h2;
              }
            case _ =>
          }
        }
      };
      for( val rule <- rules ) {
        rule match {
          case HedgeRule( h, _,h2 ) =>
            if( !reached.contains(h) ) {
              rules = rules - rule;
            }
          case HedgeChainRule(h , _ ) =>
            if( !reached.contains(h) ) {
              rules = rules - rule;
              }
          case _ =>
        }
      }
    }
      */

    def sortByWidth( xs:Iterator[Tree] ):List[Tree] = {


      def view( x:Pair[Tree,int] ):Ordered[Pair[Tree,int]] =
        new Ordered[Pair[Tree,int]] with Proxy( x ) {
          def compareTo [b >: Pair[Tree,int] <% Ordered[b]](that: b): int =
            that match {
            case Pair(p:Tree,i:int) =>
              if( minimalWidth( x._1 ) == minimalWidth( p ) ) {
                if( x._2 < i ) -1 else
                  if( x._2 == i ) 0 else 1
              } else if ( minimalWidth( x._1 ) > minimalWidth( p ) ) {
                -1
              } else {
                1
              }
            case _ => error("unexpected");
          }
        };

      var mySet = new immutable.TreeSet[Pair[Tree,int]]();
      var j = 1;
      for( val p <- xs ) {
        mySet = mySet + Pair(p,j);
        j = j + 1;
      };
      {for( val Pair(p,i) <- mySet.elements ) yield p}.toList.reverse;

    }

    /* invariant: (in < out) || (out == 0)
    */
    def MakeHedgeRule( in:HedgeNT,
                       out:HedgeNT,
                       vset: immutable.Set[Int],
                       it:Iterator[Tree] ):unit = {
      DEBUGPRINT("MakeHedgeRule("+in+","+out+","+vset+","+it+")");
      var i = in;
      if( !it.hasNext ) {
	cur.hedgeRules += new HedgeChainRule(in, out);
      } else {
	while( it.hasNext ) {
          val pat = it.next;
	  val nt = if( it.hasNext ) { cur.make.HedgeNT } else { out };
          MakeHedgeRule( i, nt, vset, pat );
          i = nt;
        }
      }
                       }

  /*
  */
  def MakeHedgeRule( in:HedgeNT, nt:HedgeNT, vset:immutable.Set[Int], pat:Tree):unit =
    pat match {
      /*
      case Point() =>
	cur.hedgeRules += new HedgeChainRule(in, iteration);
      cur.hedgeRules += new HedgeChainRule(in, iterationEnd);
      */
      case Tree$Sequence( xs ) =>
	MakeHedgeRule( in, nt, vset, Iterator.fromArray( xs ) );

      case Tree$Alternative( xs  ) =>
	for( val z <- sortByWidth( Iterator.fromArray( xs ) ) ) {
	  MakeHedgeRule( in, nt, vset, z );
	}

      // Star( p )
      case Tree$Bind(n, p) if TreeInfo.isNameOfStarPattern( n ) =>
	MakeHedgeRule( in, in, vset, p );
      cur.hedgeRules += new HedgeChainRule( in, nt );

      // p^[ x ], nt
      /*
      case Iter( p, x ) =>

        // sort by width - case 1
        if( minimalWidth( p ) < minimalWidth( x ) ) {
          MakeHedgeRule( i, nt, vset, x ); // no iteration ==> x, nt
        }

      iteration = cur.make.HedgeNT;
      iterationEnd = cur.make.HedgeNT;

      MakeHedgeRule( i, nt, vset, p );  // unfold p [ # <- x ], nt

      MakeHedgeRule( iteration, EMPTYHEDGE, emptyVars, p );
      // unfold p [ # <- x + p] $END
      MakeHedgeRule( iterationEnd, EMPTYHEDGE, emptyVars, x ); // x $END
      // sort by width - case 2
      if( minimalWidth( p ) >= minimalWidth( x ) ) {
        MakeHedgeRule( i, nt, vset, x ); // no iteration ==> x, nt
      }

      // cleanup
      iteration = null;
      iterationEnd = null;
      */
      case Tree$Bind( vble, p:Tree ) =>
        if( isSequenceValued( p ) ) {
	  MakeHedgeRule( in, nt, vset + newVar( pat.symbol() ), p );
        } else {
          try {
 	    val trNT = cur.make.TreeNT( vset );
	    MakeTreeRule( trNT, vset, pat );
	    cur.hedgeRules += new HedgeRule( in, trNT, nt );
          } catch {
            case _:WildcardException =>
	      cur.hedgeRules += new HedgeRule( in, ANYTREE, nt );
          }
        }
      case _ =>
        try {
	  val trNT = cur.make.TreeNT( vset );
	  MakeTreeRule( trNT, vset, pat );
	  cur.hedgeRules += new HedgeRule( in, trNT, nt );
        } catch {
          case _:WildcardException =>
	    cur.hedgeRules += new HedgeRule( in, ANYTREE, nt );

        }
    }


  def MakeTreeRule( in:TreeNT, vset: immutable.Set[Int], pat:Tree):unit = {
    DEBUGPRINT("MakeTreeRule("+in+","+vset+","+pat+")");
    pat match {
      // WildcardTree()
      case Tree$Ident( Names.PATTERN_WILDCARD ) =>
        if( vset.isEmpty )
          throw new WildcardException(); // OPTIMIZATION to collapse _ rules
        else
          cur.treeRules += new AnyTreeRule( in );
      // WildcardNode( x @ _* )
      case Tree$Apply( Tree$Ident( Names.PATTERN_WILDCARD ), xs ) =>
	{
	  val Children = cur.make.HedgeNT;
          cur.treeRules += new AnyNodeRule( in, Children );
	  MakeHedgeRule( Children, EMPTYHEDGE, emptyVars, Iterator.fromArray( xs ));
	}
      // Node( label:String, x @ _* ) => //p.type
      case Tree$Apply( theTest, xs ) =>
        val test = newTest( theTest ); //p.type
        val childrenNT = cur.make.HedgeNT; // make a new NT
        cur.treeRules += new TreeRule(in, test, childrenNT );
        MakeHedgeRule( childrenNT, EMPTYHEDGE, emptyVars, Iterator.fromArray( xs ));

      case Tree$Alternative( xs ) => // is tree valued
        for(val pat<-Iterator.fromArray( xs ))
          MakeTreeRule( in, vset, pat );

      case Tree$Bind( _, p:Tree ) =>
        in.vset = in.vset + newVar( pat.symbol() );
        MakeTreeRule( in, in.vset, p );

      case _ => //DEBUGPRINT("error, we found "+p.getClass());
    }
  } // end MakeTreeRule


  /* misc stuff */

  def DEBUGPRINT( s:String ):unit =  // misc DEBUG
    if (global.debug) Console.println( s );

  class WildcardException extends Exception ;   // backtrack

  final val emptyVars = new immutable.TreeSet[Int](); // convenience

}
}
