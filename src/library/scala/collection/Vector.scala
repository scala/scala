/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import generic._
import mutable.{Builder, ArrayBuffer}

/** <p>
 *    Sequences that support O(1) element access and O(1) length computation.
 *  </p>
 *  <p>
 *    This class does not add any methods to <code>Sequence</code> but
 *    overrides several methods with optimized implementations.
 *  </p>
 *
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait Vector[+A] extends Seq[A]
                    with GenericTraversableTemplate[A, Vector]
                    with VectorLike[A, Vector[A]] {
  override def companion: GenericCompanion[Vector] = Vector
}

object Vector extends SeqFactory[Vector] {
  implicit def builderFactory[A]: BuilderFactory[A, Vector[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, Vector[A]] = mutable.Vector.newBuilder[A]

  @deprecated("use collection.mutable.Vector instead") type Mutable[A] = scala.collection.mutable.Vector[A]
}
