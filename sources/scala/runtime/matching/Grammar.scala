package scala.runtime.matching ;

import scala.collection.Set;
import scala.collection.Map ;
import scala.collection.immutable;
import scala.collection.mutable;

//val ruleOrder = new Order( rule_smaller, rule_eq );
/** runtime representation of patterns. This class treats all variable
 *  indices as sequence variables
 */
abstract class Grammar( theTreeTransitions:Map[Int,Set[TRule]],
                        theHedgeTransitions:Map[Int,Set[HRule]],
                        caseVars:Array[Int] ) {

  final val treeTransitions = theTreeTransitions ;
  final val hedgeTransitions = theHedgeTransitions ;
  /** for cases 1..n, holds max. variable index */
  final val vars = caseVars;

  val treeInitials:Set[TreeNT] ;
  val hedgeInitials:Set[HedgeNT] ;

  def test( test:int, inp:Any ):boolean ;

  final def isSequenceType = treeInitials.isEmpty;
}
