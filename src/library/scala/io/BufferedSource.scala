/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.io

import java.io.{ InputStream, BufferedReader, InputStreamReader }
import Source.DefaultBufSize
import scala.collection.Iterator

/** This object provides convenience methods to create an iterable
 *  representation of a source file.
 *
 *  @author  Burak Emir, Paul Phillips
 */
class BufferedSource(inputStream: InputStream, bufferSize: Int)(implicit val codec: Codec) extends Source {
  def this(inputStream: InputStream)(implicit codec: Codec) = this(inputStream, DefaultBufSize)(codec)
  def reader() = new InputStreamReader(inputStream, codec.decoder)
  def bufferedReader() = new BufferedReader(reader(), bufferSize)

  override val iter = {
    val reader = bufferedReader()
    Iterator continually (codec wrap reader.read()) takeWhile (_ != -1) map (_.toChar)
  }

  class BufferedLineIterator extends Iterator[String] {
    val bufReader = BufferedSource.this.bufferedReader()
    var nextLine = bufReader.readLine

    override def hasNext = nextLine != null
    override def next(): String = {
      val result = nextLine
      nextLine = bufReader.readLine
      result
    }
  }

  override def getLines(): Iterator[String] = new BufferedLineIterator
}

