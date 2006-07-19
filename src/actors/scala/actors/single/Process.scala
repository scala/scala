/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.single

import scala.collection.mutable.HashSet

/**
 * @author Philipp Haller
 */
class Process extends scala.actors.Process with Actor[Any] {
  case class Exit(from: scala.actors.Process, reason: Symbol)

  private val links = new HashSet[scala.actors.Process]

  def link(to: scala.actors.Process): Unit = {
    links += to
    to.linkTo(this)
  }

  def linkTo(to: scala.actors.Process): Unit = links += to

  def unlink(from: scala.actors.Process): Unit = {
    links -= from
    from.unlinkFrom(this)
  }

  def unlinkFrom(from: scala.actors.Process): Unit = links -= from

  private var trapExit = false

  def processFlag(flag: Symbol, set: boolean) = {
    if (flag.name.equals("trapExit")) trapExit = set
  }

  def exit(reason: Symbol): Unit = {
    exitLinked(reason, new HashSet[Process])
    if (isAlive) isAlive = false
  }

  def exit(from: scala.actors.Process, reason: Symbol): Unit = {
    if (from == this) {
      exit(reason)
    }
    else {
      if (trapExit)
        this ! Exit(from, reason)
      else if (!reason.name.equals("normal"))
        exit(reason)
    }
  }

  def exitLinked(reason: Symbol, exitMarks: HashSet[Process]): Unit = {
    if (exitMarks contains this) {
      // we are marked, do nothing
    }
    else {
      exitMarks += this // mark this as exiting

      // exit linked processes
      val iter = links.elements
      while (iter.hasNext) {
        val linked = iter.next
        unlink(linked)
        linked.exit(this, reason)
      }
      exitMarks -= this
    }
  }
}
