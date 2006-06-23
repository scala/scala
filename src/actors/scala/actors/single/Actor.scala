/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.single

/**
 * @author Philipp Haller
 */
abstract class Actor extends MailBox {
  def run: Unit = {}

  def start: Unit = {
    try { run }
    catch {
      case d:Done =>
        // do nothing
    }
  }

  private var pid: Pid = null

  def self: Pid = {
    if (pid == null) pid = new LocalPid(this)
    pid
  }

  def self_= (p: Pid) = pid = p

  def join(pid1: Pid, pid2: Pid, cont: List[Pid]): unit = {
    receive {
      case Pair(pid1, msg1) => receive {
        case Pair(pid2, msg2) => cont match {
          case x::xs => x ! Pair(self, Pair(Pair(msg1, msg2), xs))
        }
      }
      case Pair(pid2, msg2) => receive {
        case Pair(pid1, msg1) => cont match {
          case x::xs => x ! Pair(self, Pair(Pair(msg1, msg2), xs))
        }
      }
    }
  }

  def spawn(body: Actor => Unit): Pid = {
    val a = new Actor {
      override def run = body(this)
    }
    a.start
    a.self
  }

  def spawn(a: Actor): Pid = {
    a.start
    a.self
  }

  def spawnReceive(cases: PartialFunction[MailBox#Message,Unit]) = {
    val a = new Actor {
      override def run = receive(cases)
    }
    a.start
    a.self
  }

  def makeRef = Actor.makeRef
}

object Actor {
  private var counter = 0
  type Tag = int
  def makeRef: Tag = {
    counter = counter + 1
    counter
  }
}
