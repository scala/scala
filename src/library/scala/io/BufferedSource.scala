/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.io

import java.io.{ InputStream, BufferedReader, InputStreamReader, PushbackReader }
import Source.DefaultBufSize
import scala.collection.{ Iterator, AbstractIterator }
import scala.collection.mutable.StringBuilder

/** This object provides convenience methods to create an iterable
 *  representation of a source file.
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
  private[this] var charReaderCreated = false
  private[this] lazy val charReader = {
    charReaderCreated = true
    bufferedReader()
  }

  override val iter = (
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
    private[this] val lineReader = decachedReader
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

  /** Efficiently appends the entire remaining input.
   *
   *  Note: This function may temporarily load the entire buffer into
   *  memory.
   */
  override def addString(sb: StringBuilder, start: String, sep: String, end: String): sb.type =
    if (sep.isEmpty) {
      val allReader = decachedReader
      val buf = new Array[Char](bufferSize)
      val jsb = sb.underlying

      if (start.length != 0) jsb.append(start)
      var n = allReader.read(buf)
      while (n != -1) {
        jsb.append(buf, 0, n)
        n = allReader.read(buf)
      }
      if (end.length != 0) jsb.append(end)
      sb
    // This case is expected to be uncommon, so we're reusing code at
    // the cost of temporary memory allocations.
    // mkString will callback into BufferedSource.addString to read
    // the Buffer into a String, and then we use StringOps.addString
    // for the interspersing of sep.
    } else mkString.addString(sb, start, sep, end)
}
