/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
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
  val NoReset: () => Source = () => throw new UnsupportedOperationException()

  /** Creates a <code>Source</code> from System.in.
   */
  def stdin = fromInputStream(System.in)

  def fromIterable(iterable: Iterable[Char]): Source = new Source {
    def reset() = fromIterable(iterable)
    val iter = iterable.iterator
  }

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

  /** Create a <code>Source</code> from array of bytes, with
   *  empty description.
   *
   *  @param bytes ...
   *  @param enc   ...
   *  @return      the created <code>Source</code> instance.
   */
  def fromBytes(bytes: Array[Byte])(implicit codec: Codec = Codec.default): Source =
    fromString(new String(bytes, codec.name))

  /** creates Source from file with given name, setting
   *  its description to filename.
   */
  def fromPath(name: String)(implicit codec: Codec = Codec.default): Source = fromFile(new JFile(name))

  /** creates <code>Source</code> from file with given file: URI
   */
  def fromURI(uri: URI)(implicit codec: Codec = Codec.default): Source = fromFile(new JFile(uri))

  /** Creates Source from <code>file</code>, using given character encoding,
   *  setting its description to filename. Input is buffered in a buffer of
   *  size <code>bufferSize</code>.
   */
  def fromFile(file: JFile, bufferSize: Int = DefaultBufSize)(implicit codec: Codec = Codec.default): Source = {
    val inputStream = new FileInputStream(file)
    setFileDescriptor(file,
      BufferedSource.fromInputStream(inputStream, bufferSize, () => fromFile(file, bufferSize)(codec)))
  }

  /** This method sets the descr property of the given source to a string of the form "file:"+path
   *  @param file the file whose path we want to describe
   *  @param s    the source whose property we set
   *  @return     s
   */
  private def setFileDescriptor(file: JFile, source: Source): Source = {
    source.descr = "file:" + file.getAbsolutePath
    source
  }

  /** same as fromInputStream(url.openStream(), enc)
   */
  def fromURL(url: URL)(implicit codec: Codec = Codec.default): Source =
    fromInputStream(url.openStream())

  /** same as BufferedSource.fromInputStream(is)
   */
  def fromInputStream(inputStream: InputStream)(implicit codec: Codec = Codec.default): Source =
    BufferedSource.fromInputStream(inputStream, DefaultBufSize, () => fromInputStream(inputStream)(codec))
}

/** The class <code>Source</code> implements an iterable representation
 *  of source files. Calling method <code>reset</code> returns an identical,
 *  resetted source.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
abstract class Source extends Iterator[Char] {

  // ------ protected values

  /** the actual iterator */
  protected val iter: Iterator[Char]

  protected var cline = 1
  protected var ccol = 1

  // ------ public values

  /** position of last character returned by next*/
  var pos = 0

  /** the last character returned by next.
   *  the value before the first call to next is undefined.
   */
  var ch: Char = _

  /** description of this source, default empty */
  var descr: String = ""

  var nerrors = 0
  var nwarnings = 0

  /** default col increment for tabs '\t', set to 4 initially
   */
  var tabinc = 4

  //
  // -- methods
  //

  /** convenience method, returns given line (not including newline)
   *  from Source.
   *
   *  @param line the line index, first line is 1
   *  @return     the character string of the specified line.
   *  @throws scala.compat.Platform.IllegalArgumentException
   *
   */
  def getLine(line: Int): String = { // faster than getLines.drop(line).next
    // todo: should @throws scala.compat.Platform.IndexOutOfBoundsException
    if (line < 1) throw new IllegalArgumentException(line.toString)
    val buf = new StringBuilder
    val it = reset
    var i = 0

    while (it.hasNext && i < (line-1))
      if ('\n' == it.next)
        i += 1;

    if (!it.hasNext) // this should not happen
      throw new IllegalArgumentException(
        "line " + line + " does not exist"
      )

    var ch = it.next
    while (it.hasNext && '\n' != ch) {
      buf append ch
      ch = it.next
    }

    if ('\n' != ch)
      buf append ch

    val res = buf.toString()
    buf setLength 0  // hopefully help collector to deallocate StringBuilder
    res
  }

  /** returns an iterator who returns lines (including newline character).
   *  a line ends in \n.
   */
  def getLines: Iterator[String] = new Iterator[String] {
    val buf = new StringBuilder
    def next = {
      var ch = iter.next
      while (ch != '\n' && iter.hasNext) {
        buf append ch
        ch = iter.next
      }
      buf append ch
      val res = buf.toString()
      buf setLength 0  // clean things up for next call of "next"
      res
    }
    def hasNext = iter.hasNext
  }
  /** Returns <code>true</code> if this source has more characters.
   */
  def hasNext = iter.hasNext

  /** returns next character and has the following side-effects: updates
   *  position (ccol and cline) and assigns the character to ch
   */
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

  /**
   *  @param pos the source position (line/column)
   *  @param msg the error message to report
   *  @param out PrintStream to use
   */
  def report(pos: Int, msg: String, out: PrintStream) {
    val buf = new StringBuilder
    val line = Position.line(pos)
    val col = Position.column(pos)
    buf.append(descr + ":" + line + ":" + col + ": " + msg)
    buf append getLine(line)
    var i = 1
    while (i < col) {
      buf append ' '
      i += 1
    }
    buf append '^'
    out.println(buf.toString)
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

  /** The reset() method creates a fresh copy of this Source. */
  def reset(): Source
}
