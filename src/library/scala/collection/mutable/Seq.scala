/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._

/** A subtrait of <code>collection.Seq</code> which represents sequences
 *  that can be mutated.
 *  The class adds an <code>update</code> method to <code>collection.Seq</code>.
 *
 *  @since 2.8
 */
trait Seq[A] extends Iterable[A]
                     with scala.collection.Seq[A]
                     with GenericTraversableTemplate[A, Seq]
                     with SeqLike[A, Seq[A]] {
  override def companion: GenericCompanion[Seq] = Seq

  def update(idx: Int, elem: A)
}

/** A factory object for the trait <code>Seq</code>.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
object Seq extends SeqFactory[Seq] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Seq[A]] =
    new GenericCanBuildFrom[A] {
      def apply() = newBuilder[A]
    }
  def newBuilder[A]: Builder[A, Seq[A]] = new ArrayBuffer
}
