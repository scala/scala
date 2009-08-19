/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.concurrent

import ops._

/**
 * Library for using join-calculus concurrent primitives in Scala.
 *
 * @author  Vincent Cremet
 * @version 1.0, 17/10/2003
 */
@deprecated("Will be removed.")
object jolib {

  type Pattern = List[Signal]

  type Rule = PartialFunction[List[Any], Unit]

  /////////////////// JOIN DEFINITION /////////////////////////

  class Join {

    private var ruls: List[(Pattern, Rule)] = null

    def canMatch(p: Pattern) =
      p forall { s => !s.queue.isEmpty }

    def values(p: Pattern): List[Any] =
      p map { s => s.queue.dequeue: Any }

    def rules(rs: (Pattern, Rule)*) =
      ruls = rs.asInstanceOf[List[(Pattern, Rule)]]

    def tryMatch =
      ruls find { case (p, _) => canMatch(p) } match {
        case None => () => ()
        case Some((p, r)) => {
          val args = values(p)
          () => spawn(r(args))
        }
      }

  }

  /////////////////// SIGNALS /////////////////////////

  abstract class Signal(join: Join) {
    type C
    val queue = new collection.mutable.Queue[C]
    def tryReduction(x: C) {
      val continuation = join synchronized {
        queue.enqueue(x)
        join.tryMatch
      }
      continuation()
    }
  }

  abstract class Asynchr(join: Join) extends Signal(join) {
    def apply(x: C): Unit = tryReduction(x)
  }

  abstract class Synchr[A](join: Join) extends Signal(join) {
    type C <: SyncVar[A]
    def apply(x: C): A = {
      tryReduction(x)
      x.get
    }
  }

}

