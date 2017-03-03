/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.io

import java.io.{ InputStream, BufferedReader, InputStreamReader, PushbackReader }
import Source.DefaultBufSize
import scala.collection.{ Iterator, AbstractIterator }

/** This object provides convenience methods to create an iterable
 *  representation of a source file.
 *
 *  @author  Burak Emir, Paul Phillips
 */
class BufferedSource(inputStream: InputStream, bufferSize: Int)(implicit val codec: Codec) extends Source {
  def this(inputStream: InputStream)(implicit codec: Codec) = this(inputStream, DefaultBufSize)(codec)
  def reader() = new InputStreamReader(inputStream, codec.decoder)
  def bufferedReader() = new BufferedReader(reader(), bufferSize)

  // The same reader has to be shared between the iterators produced
  // by iter and getLines. This is because calling hasNext can cause a
  // block of data to be read from the stream, which will then be lost
  // to getLines if it creates a new reader, even though next() was
  // never called on the original.
  private var charReaderCreated = false
  private lazy val charReader = {
    charReaderCreated = true
    bufferedReader()
  }

  override lazy val iter = (
    Iterator
    continually (codec wrap charReader.read())
    takeWhile (_ != -1)
    map (_.toChar)
  )

  private def decachedReader: BufferedReader = {
    // Don't want to lose a buffered char sitting in iter either. Yes,
    // this is ridiculous, but if I can't get rid of Source, and all the
    // Iterator bits are designed into Source, and people create Sources
    // in the repl, and the repl calls toString for the result line, and
    // that calls hasNext to find out if they're empty, and that leads
    // to chars being buffered, and no, I don't work here, they left a
    // door unlocked.
    // To avoid inflicting this silliness indiscriminately, we can
    // skip it if the char reader was never created: and almost always
    // it will not have been created, since getLines will be called
    // immediately on the source.
    if (charReaderCreated && iter.hasNext) {
      val pb = new PushbackReader(charReader)
      pb unread iter.next().toInt
      new BufferedReader(pb, bufferSize)
    }
    else charReader
  }


  class BufferedLineIterator extends AbstractIterator[String] with Iterator[String] {
    private val lineReader = decachedReader
    var nextLine: String = null

    override def hasNext = {
      if (nextLine == null)
        nextLine = lineReader.readLine

      nextLine != null
    }
    override def next(): String = {
      val result = {
        if (nextLine == null) lineReader.readLine
        else try nextLine finally nextLine = null
      }
      if (result == null) Iterator.empty.next()
      else result
    }
  }

  override def getLines(): Iterator[String] = new BufferedLineIterator

  /** Efficiently converts the entire remaining input into a string. */
  override def mkString = {
    // Speed up slurping of whole data set in the simplest cases.
    val allReader = decachedReader
    val sb = new StringBuilder
    val buf = new Array[Char](bufferSize)
    var n = 0
    while (n != -1) {
      n = allReader.read(buf)
      if (n>0) sb.appendAll(buf, 0, n)
    }
    sb.result
  }
}
