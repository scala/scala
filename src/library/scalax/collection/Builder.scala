/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ListBuffer.scala 14378 2008-03-13 11:39:05Z dragos $

package scalax.collection


trait Builder[+C[B], A] extends Iterable[A] {
  def +=(x: A)
  def ++=(xs: Iterator[A])
  def ++=(xs: Iterable[A])
  def result: C[A]
}
