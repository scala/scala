package scala.util.automata ;

import scala.collection.{ Set, Map };

/** A nondeterministic automaton. States are integers, where
 *  0 is always the only initial state. Transitions are represented
 *  in the delta function. Default transitions are transitions that
 *  are taken when no other transitions can be applied.
 *  All states are reachable. Accepting states are those for which
 *  the partial function 'finals' is defined.
 */
abstract class NondetWordAutom {

  type T_label;

  val nstates:  Int;
  val finals:   PartialFunction[Int,Int] ;
  val delta:    Function1[Int,Map[T_label,List[Int]]];
  val default:  Array[List[Int]];

  /** returns true if the state is final */
  final def isFinal(state: Int)  = finals.isDefinedAt( state );

  /** returns tag of final state */
  final def finalTag(state: Int) = finals( state );

  /** returns true if the set of states contains at least one final state */
  final def containsFinal(Q: Set[Int]):Boolean = {
    val it = Q.elements;
    while( it.hasNext )
      if( isFinal( it.next ))
        return true;
    return false;
  }

  /** returns true if there are no accepting states */
  final def isEmpty = {
    var r = true;
    var j = 0; while( r && ( j < nstates )) {
      if(isFinal(j))
        r = false;
    }
    r
  }

  override def toString() = {
    val sb = new StringBuffer();
    sb.append("[nfa nstates=");
    sb.append(nstates);
    sb.append(" finals=");
    var map = new scala.collection.immutable.ListMap[Int,Int];
    var j = 0; while( j < nstates ) {
      if(finals.isDefinedAt(j))
        map = map.update(j,finals(j))
    }
    sb.append(map.toString());
    sb.append(" delta=\n");
    for( val i <- Iterator.range(0,nstates)) {
      sb.append( i );
      sb.append("->");
      sb.append(delta(i).toString());
      sb.append('\n');
      sb.append(" _>");
      sb.append(default(i).mkString("",",",""));
      sb.append('\n');
    }
    sb.toString();
  }
}
