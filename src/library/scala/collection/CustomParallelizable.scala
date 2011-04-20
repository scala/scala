/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

import parallel.Combiner

trait CustomParallelizable[+A, +ParRepr <: Parallel] extends Parallelizable[A, ParRepr] {
  override def par: ParRepr
  override protected[this] def parCombiner: Combiner[A, ParRepr] = throw new UnsupportedOperationException("")
}

