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
import TraversableView.NoBuilder

/** A non-strict projection of an iterable.
 * @author Sean McDirmid
 * @author Martin Odersky
 * @version 2.8
 */
trait SeqView[+A, +Coll] extends SeqViewLike[A, Coll, SeqView[A, Coll]]

object SeqView {
  type Coll = TraversableView[_, C] forSome {type C <: Traversable[_]}
  implicit def builderFactory[A]: BuilderFactory[A, SeqView[A, Seq[_]], Coll] = new BuilderFactory[A, SeqView[A, Seq[_]], Coll] { def apply(from: Coll) = new NoBuilder }
}

