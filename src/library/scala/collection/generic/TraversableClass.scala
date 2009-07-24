/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.generic
import scala.collection._

import annotation.unchecked.uncheckedVariance

trait TraversableClass[+A, +CC[X] <: Traversable[X]] {

  def foreach[U](f: A => U): Unit
  def head: A
  def isEmpty: Boolean

  /** The factory companion object that builds instances of class CC */
  def companion: Companion[CC]

  /** The builder that builds instances of CC[A] */
  protected[this] def newBuilder: Builder[A, CC[A]] = companion.newBuilder[A]

  /** The generic builder that builds instances of CC at arbitrary element types. */
  def genericBuilder[B]: Builder[B, CC[B]] = companion.newBuilder[B]

  def unzip[A1, A2](implicit toPair: A => (A1, A2)): (CC[A1], CC[A2]) = {
    val b1 = genericBuilder[A1]
    val b2 = genericBuilder[A2]
    for (xy <- this) {
      val (x, y) = toPair(xy)
      b1 += x
      b2 += y
    }
    (b1.result, b2.result)
  }

  def flatten[B](implicit toTraversable: A => Traversable[B]): CC[B] = {
    val b = genericBuilder[B]
    for (xs <- this)
      b ++= toTraversable(xs)
    b.result
  }

  def transpose[B](implicit toTraversable: A => Traversable[B]): CC[CC[B] @uncheckedVariance] = {
    val bs: Array[Builder[B, CC[B]]] = head.map(_ => genericBuilder[B]).toArray
    for (xs <- this) {
      var i = 0
      for (x <- toTraversable(xs)) {
        bs(i) += x
        i += 1
      }
    }
    val bb = genericBuilder[CC[B]]
    for (b <- bs) bb += b.result
    bb.result
  }
}

