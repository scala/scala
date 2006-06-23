/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.distributed

/**
 * @author Philipp Haller
 */
case class Name(node: Node, sym: Symbol, kernel: NetKernel) {
  def !(msg: AnyRef): unit = {
    kernel.namedSend(this, msg)
  }
}
