package scala.runtime.matching ;

import scala.collection.Set;
import scala.collection.Map;
import scala.collection.mutable;
import scala.collection.immutable ;

/** this class matches input against a grammar. A call to matchesT (matchesH)
**  returns the list of initial tree (hedge) nonterminals that generate the
**  input.
**  @author Burak Emir
**/
class Matcher( pgram:Grammar )  {

  val treeTransitions = pgram.treeTransitions;
  val hedgeTransitions = pgram.hedgeTransitions;

  /** convenience method */
  def singT( T:TreeNT ) = immutable.ListSet.Empty[TreeNT] + T;

  /** convenience method */
  def singH( H:HedgeNT ) = immutable.ListSet.Empty[HedgeNT] + H;

  /** top-level call
  **  @todo remove sanity check
  **  @return index of the first applicable nonterminal in initials
  **/
  def matchesT( input:Any ):Iterator[TreeNT] =
    if( !pgram.isSequenceType )
      isApplicableTree( pgram.treeInitials, input ).elements;
    else
      error("trying to match hedge against tree grammar");

  /** top-level call
  **  @todo remove sanity check
  **  @return index of the first applicable nonterminal in initials
  **/
  def matchesH( h:Seq[Any] ):Iterator[HedgeNT] =
    if( pgram.isSequenceType )
      isApplicableHedge( pgram.hedgeInitials, h.elements ).elements;
    else
      error("trying to match tree against hedge grammar");

  /** top-level call
  **/
  def isApplicableTree1( T:TreeNT, t:Any ):boolean =
    !isApplicableTree( singT( T ), t ).isEmpty;

  /** top-level call
  **/
  def isApplicableHedge1( H:HedgeNT, h:Iterator[Any] ):boolean =
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
  protected def isApplicableTree( initialNTs:Set[TreeNT],
                                  t:Any ):immutable.Set[TreeNT] = {
    var hedgeNTs = immutable.ListSet.Empty[HedgeNT];
    var tmpNTs = immutable.ListSet.Empty[TreeNT];
    var result = immutable.ListSet.Empty[TreeNT];

    //Console.println("isApplicableTree("+ initialNTs+","+t+")");
    for( val n <- initialNTs ) {
      for( val rule <- treeTransitions( n.i ) ) {
        val N = n;
        rule match {
          case TreeRule( N, test, newNT ) if pgram.test( test, t ) => {
            tmpNTs = tmpNTs + n;
            hedgeNTs = hedgeNTs + newNT;
          }
          case AnyTreeRule( N ) =>
            result = result + N;
          case AnyNodeRule( N, newNT ) =>
            tmpNTs = tmpNTs + n;
            hedgeNTs = hedgeNTs + newNT;
          case _ =>
        }
      }
    }
    if( !hedgeNTs.isEmpty ) {
      val applHedgeNTs = isApplicableHedge( hedgeNTs, getChildren( t ) );
        for( val tn <- tmpNTs ) {
          for( val rule <- treeTransitions( tn.i )) {
          rule match {
            case TreeRule( ttn, _ , nt ) /** type test in rule determined by ttn and nt */
            if (( ttn == tn ) && applHedgeNTs.contains( nt )) =>
              result = result + tn;
            case AnyNodeRule( ttn, nt )
            if (( ttn == tn ) && applHedgeNTs.contains( nt )) =>
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
  def isApplicableHedge( initialNTs:Set[HedgeNT],
                         it:Iterator[Any] ):immutable.Set[HedgeNT] = {

      //Console.println("isApplicableHedge("+initialNTs+","+h+")");

    if( !it.hasNext ) {
      var set = immutable.ListSet.Empty[HedgeNT];
      for( val h <- initialNTs ) {
        if( h.nullable ) { set = set + h; }
      }
      //Console.println("RET isApplicableHedge("+initialNTs+","+h+") " + set.toString());
      set /*+ EmptySeqNT */

    } else {
      val first = it.next;
      var treeNTs = immutable.ListSet.Empty[TreeNT];

      for( val h <- initialNTs ) {
        for( val rule <- hedgeTransitions( h.i ) ) {
          /* all non-empty rules that start with some H in initialHedgeNTs */
          rule match {
            case HedgeRule( _, treeNT , _ ) => {
              treeNTs = treeNTs + treeNT
            }
            case _ =>
          }
        }
      }
      val applTreeNTs = isApplicableTree( treeNTs, first );

      var nextHedgeNTs = immutable.ListSet.Empty[HedgeNT];
      for( val h   <- initialNTs;
          val rule <- hedgeTransitions( h.i ) ) {
          /* all non-empty rules that start with some H in initialHedgeNTs */
          rule match {
            case HedgeRule( _ , treeNT , nH )
            if( applTreeNTs.contains( treeNT ) ) =>
              nextHedgeNTs = nextHedgeNTs + nH

            case _ =>
          }
      }

      val applNextHedgeNTs = isApplicableHedge( nextHedgeNTs, it );

      var applHedgeNTs = immutable.ListSet.Empty[HedgeNT];

      for( val nH   <- applNextHedgeNTs;
           val h    <- initialNTs;
           val rule <- hedgeTransitions( h.i ) ) {
            /* all non-empty rules that start with some H in initialHedgeNTs */
            val H = nH;
            rule match {
              case HedgeRule( _, treeNT , H ) => {
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

