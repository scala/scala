/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.io

import java.io.{ FileInputStream, InputStream, PrintStream, File => JFile }
import java.net.{ URI, URL }

/** This object provides convenience methods to create an iterable
 *  representation of a source file.
 *
 *  @author  Burak Emir, Paul Phillips
 *  @version 1.0, 19/08/2004
 */
object Source {
  val DefaultBufSize = 2048

  /** Creates a <code>Source</code> from System.in.
   */
  def stdin = fromInputStream(System.in)

  /** Creates a <code>Source</code> from an Iterable.
   *
   *  @param    iterable  the Iterable
   *  @return   the <code>Source</code> instance.
   */
  def fromIterable(iterable: Iterable[Char]): Source = new Source {
    val iter = iterable.iterator
  } withReset(() => fromIterable(iterable))

  /** Creates a <code>Source</code> instance from a single character.
   *
   *  @param c ...
   *  @return  the create <code>Source</code> instance.
   */
  def fromChar(c: Char): Source = fromIterable(Array(c))

  /** creates Source from array of characters, with empty description.
   *
   *  @param chars ...
   *  @return      ...
   */
  def fromChars(chars: Array[Char]): Source = fromIterable(chars)

  /** creates Source from string, with empty description.
   *
   *  @param s ...
   *  @return  ...
   */
  def fromString(s: String): Source = fromIterable(s)

  /** Create a <code>Source</code> from array of bytes, decoding
   *  the bytes according to codec.
   *
   *  @param bytes ...
   *  @param enc   ...
   *  @return      the created <code>Source</code> instance.
   */
  def fromBytes(bytes: Array[Byte])(implicit codec: Codec = Codec.default): Source =
    fromString(new String(bytes, codec.name))

  /** Create a <code>Source</code> from array of bytes, assuming
   *  one byte per character (ISO-8859-1 encoding.)
   */
  def fromRawBytes(bytes: Array[Byte]): Source = fromString(new String(bytes, Codec.ISO8859.name))

  /** creates Source from file with given name, setting
   *  its description to filename.
   */
  def fromPath(name: String)(implicit codec: Codec = Codec.default): Source = fromFile(new JFile(name))

  /** creates <code>Source</code> from file with given file: URI
   */
  def fromURI(uri: URI)(implicit codec: Codec = Codec.default): Source = fromFile(new JFile(uri))

  /** same as fromInputStream(url.openStream())(codec)
   */
  def fromURL(url: URL)(implicit codec: Codec = Codec.default): Source =
    fromInputStream(url.openStream())(codec)

  /** Creates Source from <code>file</code>, using given character encoding,
   *  setting its description to filename. Input is buffered in a buffer of
   *  size <code>bufferSize</code>.
   */
  def fromFile(file: JFile, bufferSize: Int = DefaultBufSize)(implicit codec: Codec = Codec.default): Source = {
    val inputStream = new FileInputStream(file)

    fromInputStream(
      inputStream,
      bufferSize,
      () => fromFile(file, bufferSize)(codec),
      () => inputStream.close()
    ) withDescription ("file:" + file.getAbsolutePath)
  }

  /** Reads data from <code>inputStream</code> with a buffered reader,
   *  using encoding in implicit parameter <code>codec</code>.
   *
   *  @param  inputStream  the input stream from which to read
   *  @param  bufferSize   buffer size (defaults to Source.DefaultBufSize)
   *  @param  reset        a () => Source which resets the stream (if unset, reset() will throw an Exception)
   *  @param  codec        (implicit) a scala.io.Codec specifying behavior (defaults to Codec.default)
   *  @return              the buffered source
   */
  def fromInputStream(
    inputStream: InputStream,
    bufferSize: Int = DefaultBufSize,
    reset: () => Source = null,
    close: () => Unit = null
  )(implicit codec: Codec = Codec.default): Source =
  {
    // workaround for default arguments being unable to refer to other parameters
    val resetFn = if (reset == null) () => fromInputStream(inputStream, bufferSize, reset, close) else reset
    new BufferedSource(inputStream)(codec) .
      withReset (resetFn) .
      withClose (close)
  }
}

// Coming Soon?
//
// abstract class Source2[T] extends Iterable[T] { }
//
// abstract class ByteSource() extends Source2[Byte] { }
//
// abstract class CharSource(implicit codec: Codec = Codec.default) extends Source2[Char] { }

/** The class <code>Source</code> implements an iterable representation
 *  of source data.  Calling method <code>reset</code> returns an identical,
 *  resetted source, where possible.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
abstract class Source extends Iterator[Char]
{
  /** the actual iterator */
  protected val iter: Iterator[Char]

  // ------ public values

  /** description of this source, default empty */
  var descr: String = ""

  var nerrors = 0
  var nwarnings = 0

  /** convenience method, returns given line (not including newline)
   *  from Source.
   *
   *  @param line the line index, first line is 1
   *  @return     the character string of the specified line.
   *
   */
  def getLine(line: Int): String = getLines() drop (line - 1) next

  class LineIterator(separator: String) extends Iterator[String] {
    require(separator.length == 1 || separator.length == 2, "Line separator may be 1 or 2 characters only.")
    lazy val iter: BufferedIterator[Char] = Source.this.iter.buffered
    // For two character newline sequences like \r\n, we peek at
    // the iterator head after seeing \r, and drop the \n if present.
    val isNewline: Char => Boolean = {
      val firstCh = separator(0)
      if (separator.length == 1) (_ == firstCh)
      else (ch: Char) => (ch == firstCh) && iter.hasNext && {
        val res = iter.head == separator(1)
        if (res) { iter.next }  // drop the second character
        res
      }
    }
    private[this] val sb = new StringBuilder

    private def getc() =
      if (!iter.hasNext) false
      else {
        val ch = iter.next
        if (isNewline(ch)) false
        else {
          sb append ch
          true
        }
      }

    def hasNext = iter.hasNext
    def next = {
      sb.clear
      while (getc()) { }
      sb.toString
    }
  }

  /** returns an iterator who returns lines (NOT including newline character(s)).
   *  If no separator is given, the platform-specific value "line.separator" is used.
   *  a line ends in \r, \n, or \r\n.
   */
  def getLines(separator: String = compat.Platform.EOL): Iterator[String] =
    new LineIterator(separator)

  /** Returns <code>true</code> if this source has more characters.
   */
  def hasNext = iter.hasNext

  /** Returns next character.
   */
  def next: Char = positioner.next

  class Positioner {
    /** the last character returned by next. */
    var ch: Char = _

    /** position of last character returned by next */
    var pos = 0

    /** current line and column */
    var cline = 1
    var ccol = 1

    /** default col increment for tabs '\t', set to 4 initially */
    var tabinc = 4

    def next: Char = {
      ch = iter.next
      pos = Position.encode(cline, ccol)
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
  object NoPositioner extends Positioner {
    override def next: Char = iter.next
  }
  def ch = positioner.ch
  def pos = positioner.pos

  /** Reports an error message to the output stream <code>out</code>.
   *
   *  @param pos the source position (line/column)
   *  @param msg the error message to report
   *  @param out PrintStream to use (optional: defaults to <code>Console.err</code>)
   */
  def reportError(
    pos: Int,
    msg: String,
    out: PrintStream = Console.err)
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
  def report(pos: Int, msg: String, out: PrintStream) {
    val line = Position line pos
    val col = Position column pos

    out println "%s:%d:%d: %s%s%s^".format(descr, line, col, msg, getLine(line), spaces(col - 1))
  }

  /**
   *  @param pos the source position (line/column)
   *  @param msg the warning message to report
   *  @param out PrintStream to use (optional: defaults to <code>Console.out</code>)
   */
  def reportWarning(
    pos: Int,
    msg: String,
    out: PrintStream = Console.out)
  {
    nwarnings += 1
    report(pos, "warning! " + msg, out)
  }

  private[this] var resetFunction: () => Source = null
  private[this] var closeFunction: () => Unit = null
  private[this] var positioner: Positioner = new Positioner

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
  // we'd like to default to no positioning, but for now we break
  // less by defaulting to status quo.
  def withPositioning(on: Boolean): this.type = {
    positioner = if (on) new Positioner else NoPositioner
    this
  }

  /** The close() method closes the underlying resource. */
  def close(): Unit     =
    if (closeFunction != null) closeFunction()

  /** The reset() method creates a fresh copy of this Source. */
  def reset(): Source =
    if (resetFunction != null) resetFunction()
    else throw new UnsupportedOperationException("Source's reset() method was not set.")
}
