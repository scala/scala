/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.collection.generic

import Math.MAX_INT
import TraversableView.NoBuilder

/** A base class for views of Iterables.
 *  @author Martin Odersky
 *  @version 2.8
 */
trait IterableView[+A, +Coll <: Iterable[_]] extends IterableViewTemplate[A, Coll, IterableView[A, Coll]]

object IterableView {
  type Coll = TraversableView[_, _]
  implicit def builderFactory[A]: BuilderFactory[A, IterableView[A, Iterable[_]], Coll] = new BuilderFactory[A, IterableView[A, Iterable[_]], Coll] { def apply(from: Coll) = new NoBuilder }
}
