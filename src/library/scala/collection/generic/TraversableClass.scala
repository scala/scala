/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// $Id: Traversable.scala 15188 2008-05-24 15:01:02Z stepancheg $
package scala.collection.generic

trait TraversableClass[+A, +CC[X] <: Traversable[X]] {

  /** The factory companion object that builds instances of class CC */

  def companion: Companion[CC]
  /** The builder that builds instances of CC[A] */

  protected[this] def newBuilder: Builder[A, CC[A]] = companion.newBuilder[A]

  /** The generic builder that builds instances of CC at arbitrary element types. */
  def genericBuilder[B]: Builder[B, CC[B]] = companion.newBuilder[B]
}

