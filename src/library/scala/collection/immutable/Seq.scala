/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package immutable

import generic._
import mutable.Builder

/** A subtrait of collection.Seq which represents sequences
 *  that cannot be mutated.
 *
 *  @since 2.8
 */
trait Seq[+A] extends Iterable[A]
                      with scala.collection.Seq[A]
                      with GenericTraversableTemplate[A, Seq]
                      with SeqLike[A, Seq[A]] {
  override def companion: GenericCompanion[Seq] = Seq
}

/**
 * @since 2.8
 */
object Seq extends SeqFactory[Seq] {
  implicit def builderFactory[A]: BuilderFactory[A, Seq[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, Seq[A]] = new mutable.ListBuffer
}
