/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.automata

import scala.collection.{immutable, mutable, Set, Map}

/** A nondeterministic automaton. States are integers, where
 *  0 is always the only initial state. Transitions are represented
 *  in the delta function. Default transitions are transitions that
 *  are taken when no other transitions can be applied.
 *  All states are reachable. Accepting states are those for which
 *  the partial function 'finals' is defined.
 */
abstract class NondetWordAutom[T <: AnyRef]
{
  val nstates: Int
  val labels: Seq[T]

  val finals: Array[Int] // 0 means not final
  val delta: Array[Map[T, immutable.BitSet]]
  val default: Array[immutable.BitSet]

  /** returns true if the state is final */
  final def isFinal(state: Int) = finals(state) > 0

  /** returns tag of final state */
  final def finalTag(state: Int) = finals(state)

  /** returns true if the set of states contains at least one final state */
  final def containsFinal(Q: immutable.BitSet): Boolean = Q exists isFinal

  /** returns true if there are no accepting states */
  final def isEmpty = (0 until nstates) forall (x => !isFinal(x))

  /** returns a bitset with the next states for given state and label */
  def next(q: Int, a: T): immutable.BitSet = delta(q).get(a) getOrElse default(q)

  /** returns a bitset with the next states for given state and label */
  def next(Q: immutable.BitSet, a: T): immutable.BitSet = next(Q, next(_, a))
  def nextDefault(Q: immutable.BitSet): immutable.BitSet = next(Q, default)

  private def next(Q: immutable.BitSet, f: (Int) => immutable.BitSet): immutable.BitSet =
    (Q map f).foldLeft(immutable.BitSet.empty)(_ ++ _)

  override def toString = {
    val finalString = Map(0 until nstates filter isFinal map (j => j -> finals(j)) : _*).toString
    val deltaString = (0 until nstates) .
      map (i => "   %d->%s\n    _>%s\n".format(i, delta(i).toString, default(i).toString)) mkString

    "[NondetWordAutom  nstates=%d  finals=%s  delta=\n%s".format(nstates, finalString, deltaString)
  }
}
