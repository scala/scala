/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection
package generic

import annotation.bridge

trait GenericSeqCompanion[CC[X] <: Traversable[X]]
  extends GenericCompanion[CC] {

  @bridge
  override def empty[A]: CC[A] = super.empty[A]

  @bridge
  override def apply[A](elems: A*): CC[A] = super.apply(elems: _*)

}
