/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package generic

import mutable.Builder
import annotation.bridge

abstract class SetFactory[CC[X] <: Set[X] with SetLike[X, CC[X]]]
  extends GenSetFactory[CC] with GenericSeqCompanion[CC] {

  @bridge
  override def empty[A]: CC[A] = super.empty[A]

  @bridge
  override def apply[A](elems: A*): CC[A] = super.apply(elems: _*)
}
