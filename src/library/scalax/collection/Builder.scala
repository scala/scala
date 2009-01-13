/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ListBuffer.scala 14378 2008-03-13 11:39:05Z dragos $

package scalax.collection


trait Builder[+CC[B], A] extends mutable.Appendable[A] {
  def +=(x: A)
  def elements: Iterator[A]
  def result: CC[A]
  override def ++=(xs: Iterator[A]) { for (x <- xs) this += x }
  override def ++=(xs: Iterable[A]) { for (x <- xs) this += x }
}

