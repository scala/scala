/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Buffer.scala 15799 2008-08-15 18:23:54Z odersky $


package scalax.runtime

import collection.immutable.Vector
import collection.mutable.{ArrayBuffer, BuilderProxy}
import collection.generic.covariant.VectorTemplate

object StringVector {

  implicit def unbox(sv: StringVector[Char]): String = sv.mkString

}

@cloneable
abstract class StringVector[+A] extends VectorTemplate[StringVector, A] with Vector[A] with Boxed[String] {

  /** The length of the string */
  def length: Int

  /** The element at given index */
  def apply(idx: Int): A

  /** Creates new builder for this collection */
  def newBuilder[B] = new BuilderProxy[StringVector, B] {
    val self = new ArrayBuffer[B]
    def result: StringVector[B] = new StringVector[B] {
      def length = self.length
      def apply(n: Int) = self.apply(n)
      override def foreach(f: B => Unit) = self.foreach(f)
    }
  }

  def unbox: String = {
    val sb = new StringBuilder
    for (x <- this)
      sb append x.asInstanceOf[Char]
    sb.toString
  }

  override def toString = mkString

}


