package scala.util.automata ;

import scala.util.alphabet.Alphabet ;
import scala.collection.{ Set, Map, mutable };

/** 0 is always the only initial state */
abstract class NondetWordAutom[ A <: Alphabet ] {

  val nstates: Int;
  val labels:   Set[A] ;
  val finals:   Map[Int,Int] ;
  val delta:    Function1[Int,Map[A,List[Int]]];
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

}
