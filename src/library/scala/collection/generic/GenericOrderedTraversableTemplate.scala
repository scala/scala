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
import annotation.unchecked.uncheckedVariance
import language.higherKinds

/** This trait represents collections classes which require
 *  ordered element types.
 *
 *  @author Aleksandar Prokopec
 */
trait GenericOrderedTraversableTemplate[+A, +CC[X] <: Traversable[X]] extends HasNewBuilder[A, CC[A] @uncheckedVariance] {
  implicit protected[this] val ord: Ordering[A]
  def orderedCompanion: GenericOrderedCompanion[CC]
  def genericOrderedBuilder[B](implicit ord: Ordering[B]): Builder[B, CC[B]] = orderedCompanion.newBuilder[B]
}

