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

  def unzip[A1, A2](implicit asPair: A => /*<:<!!!*/ (A1, A2)): (CC[A1], CC[A2]) = {
    val b1 = genericBuilder[A1]
    val b2 = genericBuilder[A2]
    for (xy <- this) {
      val (x, y) = asPair(xy)
      b1 += x
      b2 += y
    }
    (b1.result, b2.result)
  }

  def flatten[B](implicit asTraversable: A => /*<:<!!!*/ Traversable[B]): CC[B] = {
    val b = genericBuilder[B]
    for (xs <- this)
      b ++= asTraversable(xs)
    b.result
  }

  def transpose[B](implicit asTraversable: A => /*<:<!!!*/ Traversable[B]): CC[CC[B] @uncheckedVariance] = {
    val bs: Vector[Builder[B, CC[B]]] = asTraversable(head).map(_ => genericBuilder[B]).toVector
    for (xs <- this) {
      var i = 0
      for (x <- asTraversable(xs)) {
        bs(i) += x
        i += 1
      }
    }
    val bb = genericBuilder[CC[B]]
    for (b <- bs) bb += b.result
    bb.result
  }
}

