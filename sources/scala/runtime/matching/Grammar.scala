package scala.runtime.matching ;

import scala.collection.Set;
import scala.collection.Map ;
import scala.collection.immutable;
import scala.collection.mutable;

//val ruleOrder = new Order( rule_smaller, rule_eq );

case class PatternGrammar( treeRules:Set[Rule],
                           treeTransitions:Map[Int,Set[Rule]],
                           hedgeRules:Set[Rule],
                           hedgeTransitions:Map[Int,Set[Rule]],
                           thVars:mutable.Map[Int,List[Int]],
                           typeMapping:immutable.Map[Int,String/*Type*/],
                           make:NonTermFactory) {

  val isSequenceType = false;

  /** this mapping is only for debugging purposes. Given a variable index,
   ** it prints out the string representation of this variable
   */
  val debugVarMapping:mutable.Map[Int,String] =
    new mutable.HashMap[Int,String]();

  def hedgeRulesToString:String = {
    hedgeRules.foldLeft ("") {
      (s:String,r:Rule) => s + (r match {
        case HedgeRule( h, _ ) if ( make.hedgeInitials contains h ) => "*"
        case _ => " "
      }) + r.toString() + "\n"
    } ;
  }

  def treeRulesToString:String = {
    treeRules.foldLeft ("") {
      (s:String,r:Rule) => s + r.toString() + "\n"
    } ;
  }

  def toString:String = {
    val sb = new StringBuffer();
    sb.append( { if( treeRules.isEmpty )
      "Set of rules is EMPTY.\n" else treeRulesToString });
    sb.append( { if( hedgeRules.isEmpty )
      "Set of rules is EMPTY.\n" else hedgeRulesToString });
    /*
    sb.append( { if( bindMap.isEmpty )
      "Binding mapping is EMPTY.\n" else mapsToString });
    */
    sb.append( "hedgeInitials:"+make.hedgeInitials );
    sb.append( "treeInitials:"+make.treeInitials );
    sb.append( "\ntreeTransitions:"+treeTransitions );
    sb.append( "\nhedgeTransitions:"+hedgeTransitions );
    sb.append( "\nvariables :"+( thVars( 0 ).map { debugVarMapping }) );
    sb.append( "\ntypeMapping :"+ typeMapping.toString() );
    sb.toString()
  }
}



/*
def followChainRules( nset:Set[HedgeNT],
                     gram:Set[Rule] ):immutable.Set[HedgeNT] = {
                       //Console.println("followChainRules");
  var dirty = false;
var newnset = immutable.ListSet.Empty[HedgeNT].incl( nset );
for( val nt <- nset ) {
  for( val rule <- gram ) {
    val NT = nt;
rule match {
  case HedgeChainRule( NT, h ) => {
    if( !newnset.contains( h ) ) {
      dirty = true;
newnset = newnset + h;
    }
  }
          case _ =>
}
  }
}
if( dirty ) {
  followChainRules( newnset, gram );
} else {
  //Console.println("followChainRules output" + newnset);
  newnset
}
                     }
*/
