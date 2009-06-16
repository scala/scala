/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.generic

abstract class Companion[+CC[X] <: Traversable[X]] {
  type Coll = CC[_]

  def newBuilder[A]: Builder[A, CC[A]]

 /** The empty iterable of type CC */
  def empty[A]: CC[A] = newBuilder[A].result

  /** Creates an iterable of type CC with specified elements */
  def apply[A](args: A*): CC[A] = {
    val b = newBuilder[A]
    b ++= args
    b.result
  }
}

