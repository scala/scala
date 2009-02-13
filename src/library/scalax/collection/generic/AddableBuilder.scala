/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ListBuffer.scala 14378 2008-03-13 11:39:05Z dragos $

package scalax.collection.generic

import collection.mutable.ListBuffer
import collection.immutable.{List, Nil, ::}

class AddableBuilder[CC[B] <: Iterable[B] with Addable[CC[B], B], A](empty: CC[A]) extends Builder[CC, A] {
  protected var elems: CC[A] = empty
  def +=(x: A) { elems = elems + x }
  def elements: Iterator[A] = elems.elements
  def clear() { elems = empty }
  def result: CC[A] = elems
}
