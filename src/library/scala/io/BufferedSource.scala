/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.io

import java.io.{ InputStream, BufferedReader, InputStreamReader }
import Source.DefaultBufSize

/** This object provides convenience methods to create an iterable
 *  representation of a source file.
 *
 *  @author  Burak Emir, Paul Phillips
 */
class BufferedSource(inputStream: InputStream)(implicit codec: Codec = Codec.default) extends Source
{
  def reader() = new InputStreamReader(inputStream, codec.decoder)
  def bufferedReader() = new BufferedReader(reader(), DefaultBufSize)

  // It would be nice if we could do something like this:
  //   Stream continually getc() takeWhile (_ != -1) map (_.toChar) iterator
  // ...but the Stream is not collected so memory grows without bound

  class ContinuallyIterator[T](cond: T => Boolean, body: => T) extends BufferedIterator[T] {
    private[this] var hd: T = body
    def head = hd
    def hasNext = cond(hd)
    def next = {
      val res = hd
      hd = body
      res
    }
  }

  override val iter = {
    val reader = bufferedReader()
    new ContinuallyIterator[Int](_ != -1, codec wrap reader.read()) map (_.toChar)
  }
}

