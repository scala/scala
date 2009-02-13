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

abstract class LazyBuilder[+CC[B], A] extends Builder[CC, A] {
  protected var parts = new ListBuffer[Iterator[A]]
  def +=(x: A) = { parts += Iterator.single(x) }
  override def ++=(xs: Iterator[A]) { parts += xs }
  override def ++=(xs: Iterable[A]) { parts += xs.elements }
  def elements: Iterator[A] = Iterator.iteratorIteratorWrapper(parts.elements).flatten // !!! can remove the wrapper once new compiler is in starr
  def result: CC[A]
  def clear() { parts.clear() }
}
