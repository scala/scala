/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RichString.scala 11110 2007-05-21 12:40:17Z mcdirmid $


package scala.runtime


import Predef._
import scala.collection.mutable.{Buffer,ArrayBuffer}

final class RichStringBuilder(val self : StringBuilder) extends RandomAccessSeq.Mutable[Char] with Proxy with Buffer[Char] {
  override def length = self.length
  override def apply(idx : Int) = self.charAt(idx)
  override def mkString = self.toString
  override def update(idx : Int, c : Char) = self.setCharAt(idx, c)
  override def +=(c: Char): Unit = self append c
  override def ++=(iter: Iterable[Char]): Unit = iter match {
  case str : RichString => self append str.self
  case str : Array[Char] => self append str
  case iter => super.++=(iter)
  }
  override def ++(iter: Iterable[Char]): RichStringBuilder = { this ++= iter; this }
  override def ++[B >: Char](that : Iterable[B]) : RandomAccessSeq[B] = {
    val buf = new ArrayBuffer[B]
    this copyToBuffer buf
    that copyToBuffer buf
    buf
  }


  override def insertAll(idx: Int, iter: Iterable[Char]): Unit = iter match {
  case str : RichString => self.insert(idx, str)
  case str : Array[Char] => self.insert(idx, str)
  case iter =>
    val i = iter.elements
    var jdx = idx
    while (i.hasNext) {
      self.insert(jdx, i.next)
      jdx = jdx + 1
    }
  }
  override def +:(c : Char) = self.insert(0, c)
  def ensureSize(size : Int) = self.ensureCapacity(size)
  override def remove(idx : Int) = {
    val c = self.charAt(idx)
    self.deleteCharAt(idx)
    c
  }
  override def clear = self.setLength(0)
}
