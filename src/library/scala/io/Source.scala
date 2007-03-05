/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.io


import java.io.{File, FileInputStream, InputStream, PrintStream}

import compat.StringBuilder

/** This object provides convenience methods to create an iterable
 *  representation of a source file.
 *
 *  @author  Burak Emir
 *  @version 1.0, 19/08/2004
 */
object Source {

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
    val it = Iterator.fromArray(chars)
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
    val it = Iterator.fromString(s)
    new Source {
      def reset() = fromString(s)
      val iter = it
    }
  }

  /** creates Source from file with given name, setting its description to
   *  filename.
   */
  def fromFile(name: String): Source =
    fromFile(new File(name))

  /** creates Source from file with given name, using given encoding, setting
   *  its description to filename.
   */
  def fromFile(name: String, enc: String): Source =
    fromFile(new File(name), enc)

  /** creates Source from file with given file: URI
   */
  def fromFile(uri: java.net.URI): Source =
    fromFile(new File(uri))

  /** creates Source from file, using default character encoding, setting its
   *  description to filename.
   */
  def fromFile(file: java.io.File): Source = {
    val arr: Array[Byte] = new Array[Byte](file.length().asInstanceOf[Int])
    val is = new FileInputStream(file)
    is.read(arr)
    val s = fromBytes(arr)
    return setFileDescriptor(file, s)
  }

  /** Creates Source from file, using given character encoding, setting its
   *  description to filename.
   *
   *  @param file ...
   *  @param enc  ...
   *  @return     ...
   */
  def fromFile(file: java.io.File, enc: String): Source = {
    val arr: Array[Byte] = new Array[Byte](file.length().asInstanceOf[Int])
    val is = new FileInputStream(file)
    is.read(arr)
    val s = fromBytes(arr, enc)
    s.descr = file.getName()
    return setFileDescriptor(file, s)
  }

  /**
   *  @param file ...
   *  @param s    ...
   *  @return     ...
   */
  def setFileDescriptor(file: File, s: Source): Source = {
    s.descr = new StringBuilder().append( "file:" ).append(file.getAbsolutePath()).toString();
    s
  }

  /**
   *  @param s    ...
   *  @return     ...
   */
  def fromURL(s: String): Source =
    fromURL(new java.net.URL(s))

  /**
   *  @param url  ...
   *  @return     ...
   */
  def fromURL(url: java.net.URL): Source = {
    val it = new Iterator[Char] {
      var data: Int = _
      def hasNext = {data != -1}
      def next = {val x = data.asInstanceOf[char]; data = bufIn.read(); x}
      val in = url.openStream()
      val bufIn = new java.io.BufferedInputStream(in)
      data = bufIn.read()
    }
    val s = new Source {
      def reset() = fromURL(url)
      val iter = it
    }
    s.descr = url.toString()
    s
  }

  /** reads data from inputstream into a byte array, and calls fromBytes with given encoding.
   *  If maxlen is given, reads not more bytes than maxlen. if maxlen was not given, or was <= 0, then
   *  whole inputstream is read and closed afterwards.
   *
   *  @param istream the input stream from which to read
   *  @param enc the encoding to apply to the bytes
   *  @param maxlen optionally, a positive int specifying maximum number of bytes to read
   */
  def fromInputStream(istream: InputStream, enc: String, maxlen: Option[Int]): Source = {
    val BUFSIZE = 1024
    val limit = maxlen match { case Some(i) => i; case None => 0 }
    val bi = new java.io.BufferedInputStream(istream, BUFSIZE)
    val bytes = new collection.mutable.ArrayBuffer[Byte]()
    var b = 0
    var i = 0
    while( {b = bi.read; i = i + 1; b} != -1 && (limit <= 0 || i < limit)) {
      bytes += b.toByte;
    }
    if(limit <= 0) bi.close
    fromBytes(bytes.toArray, enc)
  }

  /** same as fromInputStream(is, enc, None) */
  def fromInputStream(is: InputStream, enc: String): Source =
    fromInputStream(is, enc, None)

  /** same as fromInputStream(is, "utf-8", None) */
  def fromInputStream(is: InputStream): Source =
    fromInputStream(is, "utf-8", None)

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
   *  @param line the line index.
   *  @return     the character string of the specified line.
   *  @throws scala.compat.Platform.IllegalArgumentException
   */
  def getLine(line: Int): String = { // faster than getLines.drop(line).next
    val buf = new StringBuilder()
    val it = reset
    var i = 0

    while (it.hasNext && i < (line-1))
      if ('\n' == it.next)
        i = i + 1;

    if (!it.hasNext) // this should not happen
      throw new IllegalArgumentException(
        "line " + line + " does not exist?!"
      );

    var ch = it.next
    while (it.hasNext && '\n' != ch) {
      buf.append(ch)
      ch = it.next
    }
    val res = buf.toString()
    buf.setLength(0) // hopefully help collector to deallocate StringBuilder
    res
  }

  /** returns an iterator who returns lines (including newline character).
   *  a line ends in \n.
   */
  def getLines: Iterator[String] = new Iterator[String] {
    val buf = new StringBuilder
    def next = {
      var ch = iter.next
      while(ch != '\n' && iter.hasNext) {
        buf.append(ch)
        ch = iter.next
      }
      buf.append(ch)
      val res = buf.toString()
      buf.setLength(0) // clean things up for next call of "next"
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
  def next = {
    ch = iter.next
    pos = Position.encode(cline,ccol)
    ch match {
      case '\n' =>
        ccol = 1
        cline = cline + 1
      case '\t' =>
        ccol = ccol + tabinc
      case _ =>
        ccol = ccol + 1
    }
    ch
  }

  /** Reports an error message to console.
   *
   *  @param pos ...
   *  @param msg the error message to report
   */
  def reportError(pos: Int, msg: String): Unit =
    reportError(pos, msg, java.lang.System.out)

  /** Reports an error message to the output stream <code>out</code>.
   *
   *  @param pos ...
   *  @param msg the error message to report
   *  @param out ...
   */
  def reportError(pos: Int, msg: String, out: PrintStream): Unit = {
    nerrors = nerrors + 1
    report(pos, msg, out)
  }

  /**
   *  @param pos ...
   *  @param msg the error message to report
   *  @param out ...
   */
  def report(pos: Int, msg: String, out: PrintStream): Unit = {
    val buf = new StringBuilder
    val line = Position.line(pos)
    val col = Position.column(pos)
    buf.append(descr + ":" + line + ":" + col + ": " + msg)
    buf.append(getLine(line))
    var i = 1
    while (i < col) {
      buf.append(' ')
      i = i + 1
    }
    buf.append('^')
    out.println(buf.toString)
  }

  /** Reports a warning message to <code>java.lang.System.out</code>.
   *
   *  @param pos ...
   *  @param msg the warning message to report
   */
  def reportWarning(pos: Int, msg: String): Unit =
    reportWarning(pos, msg, java.lang.System.out)

  /**
   *  @param pos ...
   *  @param msg the warning message to report
   *  @param out ...
   */
  def reportWarning(pos: Int, msg: String, out: PrintStream): Unit = {
    nwarnings = nwarnings + 1
    report(pos, "warning! " + msg, out)
  }

  /** the actual reset method */
  def reset(): Source

}
