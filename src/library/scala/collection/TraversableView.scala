/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import generic._
import mutable.Builder
import TraversableView.NoBuilder

/** <p>
 *    A base class for views of <a href="../Traversable.html"
 *    target="ContentFrame"><code>Traversable<code></a>.<br/>
 *    Every subclass has to implenment the <code>foreach</code> method.
 *  </p>
 *
 *  @author Martin Odersky
 *  @version 2.8
 */
trait TraversableView[+A, +Coll] extends TraversableViewLike[A, Coll, TraversableView[A, Coll]]

object TraversableView {
  class NoBuilder[A] extends Builder[A, Nothing] {
    def +=(elem: A): this.type = this
    def iterator: Iterator[A] = Iterator.empty
    @deprecated("use `iterator' instead") def elements = iterator
    def result() = throw new UnsupportedOperationException("TraversableView.Builder.result")
    def clear() {}
  }
  type Coll = TraversableView[_, C] forSome {type C <: Traversable[_]}
  implicit def builderFactory[A]: BuilderFactory[A, TraversableView[A, Traversable[_]], Coll] = new BuilderFactory[A, TraversableView[A, Traversable[_]], Coll] { def apply(from: Coll) = new NoBuilder }
}
