/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent



import scala.annotation.implicitNotFound



trait Blockable[+T] {
  @implicitNotFound(msg = "Blocking must be done by calling `block on b`, where `b` is the Blockable object.")
  def block()(implicit canblock: CanBlock): T
}




