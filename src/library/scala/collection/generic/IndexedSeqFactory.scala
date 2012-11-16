/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package generic

import language.higherKinds

/** A template for companion objects of IndexedSeq and subclasses thereof.
 *
 *  @since 2.10
 */
abstract class IndexedSeqFactory[CC[X] <: IndexedSeq[X] with GenericTraversableTemplate[X, CC]] extends SeqFactory[CC] {
  override def ReusableCBF: GenericCanBuildFrom[Nothing] =
    scala.collection.IndexedSeq.ReusableCBF.asInstanceOf[GenericCanBuildFrom[Nothing]]
}
