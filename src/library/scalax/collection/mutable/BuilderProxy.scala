/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ListBuffer.scala 14378 2008-03-13 11:39:05Z dragos $

package scalax.collection.mutable

abstract class BuilderProxy[+CC[B], A] extends Builder[CC, A] with Proxy {
  val self: Builder[DD, A] forSome { type DD[X] }
  def +=(x: A) = self += x
  def elements: Iterator[A] = self.elements
  def result: CC[A]
  def clear() = self.clear()
  override def ++=(xs: Iterator[A]) = self ++= xs
  override def ++=(xs: Iterable[A]) = self ++= xs
}

