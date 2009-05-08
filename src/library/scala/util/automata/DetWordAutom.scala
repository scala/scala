/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.automata


import scala.collection.{Set, Map}

/** A deterministic automaton. States are integers, where
 *  0 is always the only initial state. Transitions are represented
 *  in the delta function. A default transitions is one that
 *  is taken when no other transition can be taken.
 *  All states are reachable. Accepting states are those for which
 *  the partial function 'finals' is defined.
 *
 *  @author Burak Emir
 *  @version 1.0
 */
abstract class DetWordAutom[T <: AnyRef] {

  val nstates: Int
  val finals: Array[Int]
  val delta: Array[Map[T,Int]]
  val default: Array[Int]

  /**
   *  @param q ...
   *  @return  ...
   */
  def isFinal(q: Int) = finals(q) != 0

  /**
   *  @param q ...
   *  @return  ...
   */
  def isSink(q: Int) = delta(q).isEmpty && default(q) == q

  /**
   *  @param q     ...
   *  @param label ...
   *  @return      ...
   */
  def next(q: Int, label: T) = {
    delta(q).get(label) match {
      case Some(p) => p
      case _       => default(q)
    }
  }

  override def toString() = {
    val sb = new StringBuilder("[DetWordAutom  nstates=")
    sb.append(nstates)
    sb.append(" finals=")
    var map = scala.collection.immutable.Map[Int,Int]()
    var j = 0; while( j < nstates ) {
      if (j < finals.length)
        map = map.add(j, finals(j))
      j += 1
    }
    sb.append(map.toString())
    sb.append(" delta=\n")
    for (i <- 0 until nstates) {
      sb.append( i )
      sb.append("->")
      sb.append(delta(i).toString())
      sb.append('\n')
      if (i < default.length) {
        sb.append("_>")
        sb.append(default(i).toString())
        sb.append('\n')
      }
    }
    sb.toString()
  }
}
