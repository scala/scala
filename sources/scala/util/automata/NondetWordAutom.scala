package scala.util.automata ;

import scala.collection.{ Set, Map, mutable };

/** 0 is always the only initial state */
abstract class NondetWordAutom {

  type T_label;

  val nstates:  Int;
  val labels:   Set[T_label] ;
  val finals:   Map[Int,Int] ;
  val delta:    Function1[Int,Map[T_label,List[Int]]];
  val default:  Array[List[Int]];

  /** returns true if the state is final */
  final def isFinal(state: Int)  = finals.contains( state );

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

  /** returns true if there are no finite states */
  final def isEmpty = finals.isEmpty;

  override def toString() = {
    val sb = new StringBuffer();
    sb.append("[nfa nstates=");
    sb.append(nstates);
    sb.append(" labels=");
    sb.append(labels.toString());
    sb.append(" finals=");
    sb.append(finals.toString());
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
