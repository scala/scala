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

package scala
package io

import scala.collection.{AbstractIterator, BufferedIterator}
import java.io.{Closeable, FileInputStream, FileNotFoundException, InputStream, PrintStream, File => JFile}
import java.net.{URI, URL}

import scala.annotation.nowarn

/** This object provides convenience methods to create an iterable
 *  representation of a source file.
 */
object Source {
  val DefaultBufSize = 2048

  /** Creates a `Source` from System.in.
   */
  def stdin = fromInputStream(System.in)

  /** Creates a Source from an Iterable.
   *
   *  @param    iterable  the Iterable
   *  @return   the Source
   */
  def fromIterable(iterable: Iterable[Char]): Source = new Source {
    val iter = iterable.iterator
  } withReset(() => fromIterable(iterable))

  /** Creates a Source instance from a single character.
   */
  def fromChar(c: Char): Source = fromIterable(Array(c))

  /** creates Source from array of characters, with empty description.
   */
  def fromChars(chars: Array[Char]): Source = fromIterable(chars)

  /** creates Source from a String, with no description.
   */
  def fromString(s: String): Source = fromIterable(s)

  /** creates Source from file with given name, setting its description to
   *  filename.
   */
  def fromFile(name: String)(implicit codec: Codec): BufferedSource =
    fromFile(new JFile(name))(codec)

  /** creates Source from file with given name, using given encoding, setting
   *  its description to filename.
   */
  def fromFile(name: String, enc: String): BufferedSource =
    fromFile(name)(Codec(enc))

  /** creates `source` from file with given file `URI`.
   */
  def fromFile(uri: URI)(implicit codec: Codec): BufferedSource =
    fromFile(new JFile(uri))(codec)

  /** creates Source from file with given file: URI
   */
  def fromFile(uri: URI, enc: String): BufferedSource =
    fromFile(uri)(Codec(enc))

  /** creates Source from file, using default character encoding, setting its
   *  description to filename.
   */
  def fromFile(file: JFile)(implicit codec: Codec): BufferedSource =
    fromFile(file, Source.DefaultBufSize)(codec)

  /** same as fromFile(file, enc, Source.DefaultBufSize)
   */
  def fromFile(file: JFile, enc: String): BufferedSource =
    fromFile(file)(Codec(enc))

  def fromFile(file: JFile, enc: String, bufferSize: Int): BufferedSource =
    fromFile(file, bufferSize)(Codec(enc))

  /** Creates Source from `file`, using given character encoding, setting
   *  its description to filename. Input is buffered in a buffer of size
   *  `bufferSize`.
   */
  def fromFile(file: JFile, bufferSize: Int)(implicit codec: Codec): BufferedSource = {
    val inputStream = new FileInputStream(file)

    createBufferedSource(
      inputStream,
      bufferSize,
      () => fromFile(file, bufferSize)(codec),
      () => inputStream.close()
    )(codec) withDescription s"file:${file.getAbsolutePath}"
  }

  /** Create a `Source` from array of bytes, decoding
   *  the bytes according to codec.
   *
   *  @return      the created `Source` instance.
   */
  def fromBytes(bytes: Array[Byte])(implicit codec: Codec): Source =
    fromString(new String(bytes, codec.name))

  def fromBytes(bytes: Array[Byte], enc: String): Source =
    fromBytes(bytes)(Codec(enc))

  /** Create a `Source` from array of bytes, assuming
   *  one byte per character (ISO-8859-1 encoding.)
   */
  @deprecated("Use `fromBytes` and specify an encoding", since="2.13.9")
  def fromRawBytes(bytes: Array[Byte]): Source =
    fromString(new String(bytes, Codec.ISO8859.charSet))

  /** creates `Source` from file with given file: URI
   */
  def fromURI(uri: URI)(implicit codec: Codec): BufferedSource =
    fromFile(new JFile(uri))(codec)

  /** same as fromURL(new URL(s))(Codec(enc))
   */
  def fromURL(s: String, enc: String): BufferedSource =
    fromURL(s)(Codec(enc))

  /** same as fromURL(new URL(s))
   */
  def fromURL(s: String)(implicit codec: Codec): BufferedSource =
    fromURL(new URL(s))(codec)

  /** same as fromInputStream(url.openStream())(Codec(enc))
   */
  def fromURL(url: URL, enc: String): BufferedSource =
    fromURL(url)(Codec(enc))

  /** same as fromInputStream(url.openStream())(codec)
   */
  def fromURL(url: URL)(implicit codec: Codec): BufferedSource =
    fromInputStream(url.openStream())(codec)

  /** Reads data from inputStream with a buffered reader, using the encoding
   *  in implicit parameter codec.
   *
   *  @param  inputStream  the input stream from which to read
   *  @param  bufferSize   buffer size (defaults to Source.DefaultBufSize)
   *  @param  reset        a () => Source which resets the stream (if unset, reset() will throw an Exception)
   *  @param  close        a () => Unit method which closes the stream (if unset, close() will do nothing)
   *  @param  codec        (implicit) a scala.io.Codec specifying behavior (defaults to Codec.default)
   *  @return              the buffered source
   */
  def createBufferedSource(
    inputStream: InputStream,
    bufferSize: Int = DefaultBufSize,
    reset: () => Source = null,
    close: () => Unit = null
  )(implicit codec: Codec): BufferedSource = {
    // workaround for default arguments being unable to refer to other parameters
    val resetFn = if (reset == null) () => createBufferedSource(inputStream, bufferSize, reset, close)(codec) else reset

    new BufferedSource(inputStream, bufferSize)(codec) withReset resetFn withClose close
  }

  def fromInputStream(is: InputStream, enc: String): BufferedSource =
    fromInputStream(is)(Codec(enc))

  def fromInputStream(is: InputStream)(implicit codec: Codec): BufferedSource =
    createBufferedSource(is, reset = () => fromInputStream(is)(codec), close = () => is.close())(codec)

  /** Reads data from a classpath resource, using either a context classloader (default) or a passed one.
   *
   *  @param  resource     name of the resource to load from the classpath
   *  @param  classLoader  classloader to be used, or context classloader if not specified
   *  @return              the buffered source
   */
  def fromResource(resource: String, classLoader: ClassLoader = Thread.currentThread().getContextClassLoader())(implicit codec: Codec): BufferedSource =
    Option(classLoader.getResourceAsStream(resource)) match {
      case Some(in) => fromInputStream(in)
      case None     => throw new FileNotFoundException(s"resource '$resource' was not found in the classpath from the given classloader.")
    }

}

/** An iterable representation of source data.
 *  It may be reset with the optional [[reset]] method.
 *
 *  Subclasses must supply [[scala.io.Source.iter the underlying iterator]].
 *
 *  Error handling may be customized by overriding the [[scala.io.Source.report report]] method.
 *
 *  The [[scala.io.Source.ch current input]] and [[scala.io.Source.pos position]],
 *  as well as the [[scala.io.Source.next next character]] methods delegate to
 *  [[scala.io.Source#Positioner the positioner]].
 *
 *  The default positioner encodes line and column numbers in the position passed to [[report]].
 *  This behavior can be changed by supplying a
 *  [[scala.io.Source.withPositioning(pos:* custom positioner]].
 *
 */
abstract class Source extends Iterator[Char] with Closeable {
  /** the actual iterator */
  protected val iter: Iterator[Char]

  // ------ public values

  /** description of this source, default empty */
  var descr: String = ""
  var nerrors = 0
  var nwarnings = 0

  private def lineNum(line: Int): String = (getLines() drop (line - 1) take 1).mkString

  class LineIterator extends AbstractIterator[String] with Iterator[String] {
    private[this] val sb = new StringBuilder

    lazy val iter: BufferedIterator[Char] = Source.this.iter.buffered
    def isNewline(ch: Char): Boolean = ch == '\r' || ch == '\n'
    def getc(): Boolean = iter.hasNext && {
      val ch = iter.next()
      if (ch == '\n') false
      else if (ch == '\r') {
        if (iter.hasNext && iter.head == '\n')
          iter.next()

        false
      }
      else {
        sb append ch
        true
      }
    }
    def hasNext: Boolean = iter.hasNext
    def next(): String = {
      sb.clear()
      while (getc()) { }
      sb.toString
    }
  }

  /** Returns an iterator who returns lines (NOT including newline character(s)).
   *  It will treat any of \r\n, \r, or \n as a line separator (longest match) - if
   *  you need more refined behavior you can subclass Source#LineIterator directly.
   */
  def getLines(): Iterator[String] = new LineIterator()

  /** Returns `'''true'''` if this source has more characters.
   */
  def hasNext: Boolean = iter.hasNext

  /** Returns next character.
   */
  def next(): Char = positioner.next()

  @nowarn("cat=deprecation")
  class Positioner(encoder: Position) {
    def this() = this(RelaxedPosition)
    /** the last character returned by next. */
    var ch: Char = _

    /** position of last character returned by next */
    var pos = 0

    /** current line and column */
    var cline = 1
    var ccol = 1

    /** default col increment for tabs '\t', set to 4 initially */
    var tabinc = 4

    def next(): Char = {
      ch = iter.next()
      pos = encoder.encode(cline, ccol)
      ch match {
        case '\n' =>
          ccol = 1
          cline += 1
        case '\t' =>
          ccol += tabinc
        case _ =>
          ccol += 1
      }
      ch
    }
  }
  /** A Position implementation which ignores errors in
   *  the positions.
   */
  @nowarn("cat=deprecation")
  object RelaxedPosition extends Position {
    def checkInput(line: Int, column: Int): Unit = ()
  }
  object RelaxedPositioner extends Positioner(RelaxedPosition) { }
  object NoPositioner extends Positioner(Position) {
    override def next(): Char = iter.next()
  }
  def ch: Char = positioner.ch
  def pos: Int = positioner.pos

  /** Reports an error message to the output stream `out`.
   *
   *  @param pos the source position (line/column)
   *  @param msg the error message to report
   *  @param out PrintStream to use (optional: defaults to `Console.err`)
   */
  def reportError(
    pos: Int,
    msg: String,
    out: PrintStream = Console.err): Unit =
  {
    nerrors += 1
    report(pos, msg, out)
  }

  private def spaces(n: Int) = List.fill(n)(' ').mkString
  /**
   *  @param pos the source position (line/column)
   *  @param msg the error message to report
   *  @param out PrintStream to use
   */
  def report(pos: Int, msg: String, out: PrintStream): Unit = {
    val line  = Position line pos
    val col   = Position column pos

    out println "%s:%d:%d: %s%s%s^".format(descr, line, col, msg, lineNum(line), spaces(col - 1))
  }

  /**
   *  @param pos the source position (line/column)
   *  @param msg the warning message to report
   *  @param out PrintStream to use (optional: defaults to `Console.out`)
   */
  def reportWarning(
    pos: Int,
    msg: String,
    out: PrintStream = Console.out): Unit =
  {
    nwarnings += 1
    report(pos, "warning! " + msg, out)
  }

  private[this] var resetFunction: () => Source = null
  private[this] var closeFunction: () => Unit = null
  private[this] var positioner: Positioner = RelaxedPositioner

  def withReset(f: () => Source): this.type = {
    resetFunction = f
    this
  }
  def withClose(f: () => Unit): this.type = {
    closeFunction = f
    this
  }
  def withDescription(text: String): this.type = {
    descr = text
    this
  }
  /** Change or disable the positioner. */
  def withPositioning(on: Boolean): this.type = {
    positioner = if (on) RelaxedPositioner else NoPositioner
    this
  }
  def withPositioning(pos: Positioner): this.type = {
    positioner = pos
    this
  }

  /** The close() method closes the underlying resource. */
  def close(): Unit = {
    if (closeFunction != null) closeFunction()
  }

  /** The reset() method creates a fresh copy of this Source. */
  def reset(): Source =
    if (resetFunction != null) resetFunction()
    else throw new UnsupportedOperationException("Source's reset() method was not set.")
}
