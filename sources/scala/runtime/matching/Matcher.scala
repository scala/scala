package scala.runtime.matching ;

import scala.collection.Set;
import scala.collection.Map;
import scala.collection.mutable;
import scala.collection.immutable ;

import scala.util.grammar.{ LabelledRHS, AnyTreeRHS, TreeRHS, ConsRHS, HedgeRHS };
/** this class matches input against a grammar. A call to matchesT (matchesH)
**  returns the list of initial tree (hedge) nonterminals that generate the
**  input.
**  @author Burak Emir
**/
class Matcher( pgram:PatternGrammar )  {

  val treeTransitions = pgram.treeTransitions;
  val hedgeTransitions = pgram.hedgeTransitions;

  /** convenience method */
  def singT( T:Int ) = immutable.ListSet.Empty[Int] + T;

  /** convenience method */
  def singH( H:Int ) = immutable.ListSet.Empty[Int] + H;

  /** top-level call
  **  @todo remove sanity check
  **  @return index of the first applicable nonterminal in initials
  **/
  def matchesT( input:Any ):Iterator[Int] =
    if( !pgram.isSequenceType )
      isApplicableTree( pgram.treeInitials.toSet(true), input ).elements;
    else
      error("trying to match hedge against tree grammar");

  /** top-level call
  **  @todo remove sanity check
  **  @return index of the first applicable nonterminal in initials
  **/
  def matchesH( h:Seq[Any] ):Iterator[Int] =
    if( pgram.isSequenceType )
      isApplicableHedge( pgram.hedgeInitials.toSet(true), h.elements ).elements;
    else
      error("trying to match tree against hedge grammar");

  /** top-level call
  **/
  def isApplicableTree1( T:Int, t:Any ):boolean =
    !isApplicableTree( singT( T ), t ).isEmpty;

  /** top-level call
  **/
  def isApplicableHedge1( H:Int, h:Iterator[Any] ):boolean =
    !isApplicableHedge( singH( H ), h ).isEmpty;


  def getChildren( t:Any ):Iterator[Any] = t match {
    //case n:scala.xml.Node => n.child;
    case n:CaseClass => Iterator.fromCaseClass( n );
    case _ => Iterator.empty[Any];
  }
// ------------------------------------------------------------------------

  /** given set of tree nonterms initialNTs and input tree t, returns
  **  { T | T in initialNTs,
  **        T -> a&lt;H> in rules,
  **        isApplicable( H, t.children ) }
  */
  protected def isApplicableTree( initialNTs:Set[Int/*TreeNT*/],
                                  t:Any ):immutable.Set[Int/*TreeNT*/] = {
    var hedgeNTs = immutable.ListSet.Empty[Int];
    var tmpNTs = immutable.ListSet.Empty[Int];
    var result = immutable.ListSet.Empty[Int];

    //Console.println("isApplicableTree("+ initialNTs+","+t+")");
    for( val n <- initialNTs ) {
      //Console.println("checking nt "+n);
      for( val rule <- treeTransitions( n ) ) {
        //Console.println("checking rule"+rule);
        rule match {

          case AnyTreeRHS =>
            result = result + n;

          case LabelledRHS( AnyNode, newNT ) =>
            tmpNTs = tmpNTs + n;
            hedgeNTs = hedgeNTs + newNT;

          case LabelledRHS( TestLabel(test), newNT )
            if pgram.test( test, t ) =>
              //Console.println("success for test "+test);
              tmpNTs = tmpNTs + n;
              hedgeNTs = hedgeNTs + newNT;


          case _ =>
            //Console.println("no use");
        }
      }
    }
    if( !hedgeNTs.isEmpty ) {
      val applHedgeNTs = isApplicableHedge( hedgeNTs, getChildren( t ) );
        for( val tn <- tmpNTs ) {
          for( val rule <- treeTransitions( tn )) {
          rule match {

            case LabelledRHS( AnyNode, nt )
              if applHedgeNTs.contains( nt )  =>
                result = result + tn;

            case LabelledRHS( _ , nt ) /** type test in rule determined by ttn and nt */
              if  applHedgeNTs.contains( nt ) =>
                result = result + tn;

            case _ =>
          }
        }
      }
    }
    //Console.println("RET isApplicableTree"+ initialNTs+","+t+") " + result);
    result
  }

  /** given set of hedge nonterms initialNTs and input hedge h, returns
  **  { H | H in initialNTs,
  **        T -> a&lt;H> in rules,
  **        isApplicable( H, t.children )
  */
  def isApplicableHedge( initialNTs:Set[Int/*HedgeNT*/],
                         it:Iterator[Any] ):immutable.Set[Int/*HedgeNT*/] = {

      //Console.println("isApplicableHedge("+initialNTs+",...)");

    if( !it.hasNext ) {
      var set = immutable.ListSet.Empty[Int];
      for( val h <- initialNTs ) {
        if( pgram.isNullable( h )) { set = set + h; }
      }
      //Console.println("RET isApplicableHedge("+initialNTs+","+h+") " + set.toString());
      set /*+ EmptySeqNT */

    } else {
      val first = it.next;
      var treeNTs = immutable.ListSet.Empty[Int];

      for( val h <- initialNTs ) {
        for( val rule <- hedgeTransitions( h ) ) {
          /* all non-empty rules that start with some H in initialHedgeNTs */
          rule match {
            case ConsRHS( treeNT , _ ) => {
              treeNTs = treeNTs + treeNT
            }
            case _ =>
          }
        }
      }
      val applTreeNTs = isApplicableTree( treeNTs, first );

      var nextHedgeNTs = immutable.ListSet.Empty[Int/*HedgeNT*/];
      for( val h   <- initialNTs;
           val rule <- hedgeTransitions( h ) ) {
          /* all non-empty rules that start with some H in initialHedgeNTs */
          rule match {
            case ConsRHS( treeNT , nH )
              if( applTreeNTs.contains( treeNT )) =>
                nextHedgeNTs = nextHedgeNTs + nH

            case _ =>
          }
      }

      val applNextHedgeNTs = isApplicableHedge( nextHedgeNTs, it );

      var applHedgeNTs = immutable.ListSet.Empty[Int/*HedgeNT*/];

      for( val nH   <- applNextHedgeNTs;
           val h    <- initialNTs;
           val rule <- hedgeTransitions( h ) ) {
            /* all non-empty rules that start with some H in initialHedgeNTs */
            val H = nH;
            rule match {
              case ConsRHS( treeNT , H ) => {
                applHedgeNTs = applHedgeNTs + h
              }
              case _ =>
            }
           }
/*

      var applHNTs = immutable.ListSet.Empty[HedgeNT];

      // go through all chain rules starting with initialNTs2's
      for( val h <- initialNTs2,
           val g <- applHedgeNTs ){
          // and check whether they can derive an applHedgeNT
	  if( followChainRules( immutable.ListSet.Empty[HedgeNT] + h,
                                hedgeRules )
             .contains( g ) ) {
	       applHNTs = applHNTs + h
	     }
	}
      }
  */
      //Console.println("RET isApplicableHedge( " + initialNTs + "," + h + ") " + applHedgeNTs);
    /*  applHNTs */
applHedgeNTs /* no chain rules stuff needed */
    }
  }



}

