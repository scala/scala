/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ListBuffer.scala 14378 2008-03-13 11:39:05Z dragos $

package scalax.collection.generic

import generic._

trait Builder[+CC[B], A] extends Growable[A] {
  def +=(x: A)
  def elements: Iterator[A]
  def result: CC[A]

  def mapResult[DD[B]](f: CC[A] => DD[A]) =
    new Builder[DD, A] with Proxy {
      val self = Builder.this
      def +=(x: A) = self += x
      def elements: Iterator[A] = self.elements
      def clear() = self.clear()
      override def ++=(xs: Iterator[A]) = self ++= xs
      override def ++=(xs: collection.Iterable[A]) = self ++= xs
      def result: DD[A] = f(Builder.this.result)
    }
}

