/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.io


import java.io.{BufferedInputStream, File, FileInputStream, InputStream,
                PrintStream}
import java.net.{URI, URL}
import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.Charset

/** This object provides convenience methods to create an iterable
 *  representation of a source file.
 *
 *  @author  Burak Emir
 *  @version 1.0, 19/08/2004
 */
object Source {

  val DefaultBufSize = 2048

  val NoReset: () => Source = () => throw new UnsupportedOperationException()

  /** Creates a <code>Source</code> instance from the given array of bytes,
   *  with empty description.
   *
   *  @param bytes ...
   *  @return      the created <code>Source</code> instance.
   */
  def fromBytes(bytes: Array[Byte]): Source =
    fromString(new String(bytes))

  /** Creates Source from array of bytes with given encoding, with
   *  empty description.
   *
   *  @param bytes ...
   *  @param enc   ...
   *  @return      ...
   */
  def fromBytes(bytes: Array[Byte], enc: String): Source =
    fromString(new String(bytes, enc))

  /** Creates a <code>Source</code> instance from a single character.
   *
   *  @param c ...
   *  @return  the create <code>Source</code> instance.
   */
  def fromChar(c: Char): Source = {
    val it = Iterator.single(c)
    new Source {
      def reset() = fromChar(c)
      val iter = it
    }
  }

  /** creates Source from array of characters, with empty description.
   *
   *  @param chars ...
   *  @return      ...
   */
  def fromChars(chars: Array[Char]): Source = {
    val it = chars.iterator
    new Source {
      def reset() = fromChars(chars)
      val iter = it
    }
  }

  /** creates Source from string, with empty description.
   *
   *  @param s ...
   *  @return  ...
   */
  def fromString(s: String): Source = {
    val it = s.iterator
    new Source {
      def reset() = fromString(s)
      val iter = it
    }
  }

  /** creates Source from file with given name, setting its description to
   *  filename.
   */
  def fromFile(name: String): Source =
    fromFile(name, util.Properties.encodingString)

  /** creates Source from file with given name, using given encoding, setting
   *  its description to filename.
   */
  def fromFile(name: String, enc: String): Source =
    fromFile(new File(name), enc)

  /** creates <code>Source</code> from file with given file: URI
   */
  def fromFile(uri: URI): Source =
    fromFile(uri, util.Properties.encodingString)

  /** creates Source from file with given file: URI
   */
  def fromFile(uri: URI, enc: String): Source =
    fromFile(new File(uri), enc)

  /** creates Source from file, using default character encoding, setting its
   *  description to filename.
   */
  def fromFile(file: File): Source =
    fromFile(file, util.Properties.encodingString, Source.DefaultBufSize)

  /** same as fromFile(file, enc, Source.DefaultBufSize)
   */
  def fromFile(file: File, enc: String): Source =
    fromFile(file, enc, Source.DefaultBufSize)

  /** Creates Source from <code>file</code>, using given character encoding,
   *  setting its description to filename. Input is buffered in a buffer of
   *  size <code>bufferSize</code>.
   */
  def fromFile(file: File, enc: String, bufferSize: Int): Source = {
    val inpStream = new FileInputStream(file)
    val size = if (bufferSize > 0) bufferSize else Source.DefaultBufSize
    setFileDescriptor(file,
      BufferedSource.fromInputStream(inpStream, enc, size, { () => fromFile(file, enc, size)}))
  }

  /** This method sets the descr property of the given source to a string of the form "file:"+path
   *  @param file the file whose path we want to describe
   *  @param s    the source whose property we set
   *  @return     s
   */
  private def setFileDescriptor(file: File, s: Source): Source = {
    s.descr = new StringBuilder("file:").append(file.getAbsolutePath()).toString();
    s
  }

  /**
   *  @param s    ...
   *  @return     ...
   */
  @deprecated("use fromURL(s, enc)")
  def fromURL(s: String): Source =
    fromURL(new URL(s))

  /** same as fromURL(new URL(s), enc)
   */
  def fromURL(s: String, enc:String): Source =
    fromURL(new URL(s), enc)

  /**
   *  @param url  the source URL
   *  @return     ...
   */
  @deprecated("use fromURL(url, enc)")
  def fromURL(url: URL): Source = {
    val it = new Iterator[Char] {
      var data: Int = _
      def hasNext = {data != -1}
      def next = {val x = data.asInstanceOf[Char]; data = bufIn.read(); x}
      val in = url.openStream()
      val bufIn = new BufferedInputStream(in)
      data = bufIn.read()
    }
    val s = new Source {
      def reset() = fromURL(url)
      val iter = it
    }
    s.descr = url.toString()
    s
  }

  /** same as fromInputStream(url.openStream(), enc)
   */
  def fromURL(url: URL, enc:String): Source =
    fromInputStream(url.openStream(), enc)

  /** reads data from <code>istream</code> into a byte array, and calls
   *  <code>fromBytes</code> with given encoding <code>enc</code>.
   *  If <code>maxlen</code> is given, reads not more bytes than <code>maxlen</code>;
   *  if <code>maxlen</code> was not given, or <code>was &lt;= 0</code>, then
   *  whole <code>istream</code> is read and closed afterwards.
   *
   *  @param istream the input stream from which to read
   *  @param enc the encoding to apply to the bytes
   *  @param maxlen optionally, a positive int specifying maximum number of bytes to read
   */
  @deprecated
  def fromInputStream(istream: InputStream, enc: String, maxlen: Option[Int]): Source = {
    val limit = maxlen match { case Some(i) => i; case None => 0 }
    val bi = new BufferedInputStream(istream, Source.DefaultBufSize)
    val bytes = new collection.mutable.ArrayBuffer[Byte]()
    var b = 0
    var i = 0
    while ( {b = bi.read; i += 1; b} != -1 && (limit <= 0 || i < limit)) {
      bytes += b.toByte;
    }
    if(limit <= 0) bi.close
    fromBytes(bytes.toArray, enc)
  }

  /** same as BufferedSource.fromInputStream(is, enc, Source.DefaultBufSize)
   */
  def fromInputStream(is: InputStream, enc: String): Source =
    BufferedSource.fromInputStream(is, enc, Source.DefaultBufSize, { () => fromInputStream(is, enc) })

  /** same as BufferedSource.fromInputStream(is, "utf-8", Source.DefaultBufSize) */
  def fromInputStream(is: InputStream): Source =
    BufferedSource.fromInputStream(is, "utf-8", Source.DefaultBufSize, { () => fromInputStream(is) })

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

  /** Reports an error message to console.
   *
   *  @param pos the source position (line/column)
   *  @param msg the error message to report
   */
  def reportError(pos: Int, msg: String) {
    reportError(pos, msg, Console.out)
  }

  /** Reports an error message to the output stream <code>out</code>.
   *
   *  @param pos the source position (line/column)
   *  @param msg the error message to report
   *  @param out ...
   */
  def reportError(pos: Int, msg: String, out: PrintStream) {
    nerrors += 1
    report(pos, msg, out)
  }

  /**
   *  @param pos the source position (line/column)
   *  @param msg the error message to report
   *  @param out ...
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

  /** Reports a warning message to <code>Console.out</code>.
   *
   *  @param pos the source position (line/column)
   *  @param msg the warning message to report
   */
  def reportWarning(pos: Int, msg: String) {
    reportWarning(pos, msg, Console.out)
  }

  /**
   *  @param pos the source position (line/column)
   *  @param msg the warning message to report
   *  @param out ...
   */
  def reportWarning(pos: Int, msg: String, out: PrintStream) {
    nwarnings += 1
    report(pos, "warning! " + msg, out)
  }

  /** the actual reset method */
  def reset(): Source

}
