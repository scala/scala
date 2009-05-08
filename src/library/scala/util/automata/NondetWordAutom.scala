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
abstract class NondetWordAutom[T <: AnyRef] {

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
  final def containsFinal(Q: immutable.BitSet): Boolean = {
    val it = Q.elements
    while (it.hasNext)
      if (isFinal(it.next))
        return true
    return false
  }

  /** returns true if there are no accepting states */
  final def isEmpty = {
    var r = true
    var j = 0; while(r && (j < nstates)) {
      if (isFinal(j))
        r = false
    }
    r
  }

  /** returns a bitset with the next states for given state and label */
  def next(q: Int, a: T): immutable.BitSet = {
    delta(q).get(a) match {
      case Some(bs) => bs
      case _        => default(q)
    }
  }

  /** returns a bitset with the next states for given state and label */
  def next(Q: immutable.BitSet, a: T): immutable.BitSet = {
    val x = new mutable.BitSet(nstates)
    for (q <- Q) {
      for (i <- next(q,a)) {
        x += i
      }
    }
    x.toImmutable
  }


  def nextDefault(Q: immutable.BitSet): immutable.BitSet = {
    val x = new mutable.BitSet(nstates)
    for (q <- Q) {
      for (i <- default(q)) { //@todo: OR
        x += i
      }
    }
    x.toImmutable
  }

  override def toString = {
    val sb = new StringBuilder("[NondetWordAutom  nstates=")
    sb.append(nstates)
    sb.append("  finals=")
    var map = scala.collection.immutable.Map[Int,Int]()
    var j = 0; while (j < nstates) {
      if (isFinal(j))
        map = map.add(j, finals(j));
      j += 1
    }
    sb.append(map.toString)
    sb.append("  delta=\n")
    for (i <- 0 until nstates) {
      sb.append("    ")
      sb.append( i )
      sb.append("->")
      sb.append(delta(i).toString)
      sb.append("\n    ")
      sb.append(" _>")
      sb.append(default(i).toString)
      sb.append('\n')
    }
    sb.toString()
  }
}
