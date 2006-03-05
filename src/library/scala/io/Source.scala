/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.io;


import java.io.{ File, FileInputStream, PrintStream };

/** convenience methods to create an iterable representation of a source
 *  file
 *  @author buraq
 */
object Source {

  /** creates Source from array of bytes, with empty description
   */
  def fromBytes(bytes: Array[Byte]): Source =
    fromString(new String(bytes));

  /** creates Source from array of bytes with given encoding, with empty description
   */
  def fromBytes(bytes: Array[Byte], enc: String): Source =
    fromString(new String(bytes, enc));

  /** creates Source from a single char */
  def fromChar(c: Char): Source = {
    val it = Iterator.single(c);
    new Source {
      def reset = fromChar(c);
      val iter = it;
    }
  }
  /** creates Source from array of characters, with empty description
   */
  def fromChars(chars: Array[Char]): Source = {
    val it = Iterator.fromArray(chars);
    new Source {
      def reset = fromChars(chars);
      val iter = it;
    }
  }
  /** creates Source from string, with empty description
   */
  def fromString(s: String): Source = {
    val it = Iterator.fromString(s);
    new Source {
      def reset = fromString(s);
      val iter = it;
    }
  }

  /** creates Source from file with given name, setting its description to
   *  filename.
   */
  def fromFile(name: String): Source =
    fromFile( new File( name ));

  /** creates Source from file with given name, using given encoding, setting
   *  its description to filename.
   */
  def fromFile(name: String, enc: String): Source =
    fromFile( new File( name ), enc);

  /** creates Source from file with given file: URI
   */
  def fromFile(uri: java.net.URI): Source =
    fromFile(new File(uri));

  /** creates Source from file, using default character encoding, setting its
   *  description to filename.
   */
  def fromFile(file: java.io.File): Source = {
    val arr: Array[Byte] = new Array[Byte]( file.length().asInstanceOf[Int] );
    val is = new FileInputStream( file );
    is.read( arr );
    val s = fromBytes(arr);
    return setFileDescriptor(file,s);
  }

  /** creates Source from file, using given character encoding, setting its
   *  description to filename.
   */
  def fromFile(file: java.io.File, enc: String): Source = {
    val arr: Array[Byte] = new Array[Byte]( file.length().asInstanceOf[Int] );
    val is = new FileInputStream( file );
    is.read( arr );
    val s = fromBytes(arr, enc);
    s.descr = file.getName();
    return setFileDescriptor(file,s);
  }

  def setFileDescriptor(file: File, s: Source): Source = {
    s.descr = new StringBuffer()
              .append( "file:" )
              .append( file.getAbsolutePath() )
              .toString();
    s
  }

  def fromURL(s:String): Source =
    fromURL(new java.net.URL(s));

  def fromURL(url: java.net.URL): Source = {
    val it = new Iterator[Char] {
      var data: Int = _;
      def hasNext = {data != -1};
      def next = {val x = data.asInstanceOf[char]; data = bufIn.read(); x}
      val in = url.openStream();
      val bufIn = new java.io.BufferedInputStream(in);
      data = bufIn.read()
    }
    val s = new Source {
      def reset = fromURL(url);
      val iter = it;
    };
    s.descr = url.toString();
    s
  }

}

/** an iterable representation of source files.
 *  calling method reset returns an identical, resetted source
 *
 *  @author buraq
 */
abstract class Source extends Iterator[Char] {


  // ------ protected values

  /** the actual iterator */
  protected val iter: Iterator[Char];

  protected var cline = 1;
  protected var ccol = 1;

  // ------ public values

  /** position of last character returned by next*/
  var pos = 0;

  /** the last character returned by next.
   *  the value before the first call to next is undefined.
   */
  var ch: Char = _;

  /** description of this source, default empty */
  var descr: String = "";

  var nerrors = 0;
  var nwarnings = 0;

  /** default col increment for tabs '\t', set to 4 initially
   */
  var tabinc = 4;

  //
  // -- methods
  //

  /** convenience method, returns given line (not including newline)
   *  from Source
   */
  def getLine(line: Int): String = {
    val buf = new StringBuffer();
    val it = reset;
    var i = 0;

    while( it.hasNext && i < (line-1))
      if('\n' == it.next)
        i = i + 1;

    if(!it.hasNext) { // this should not happen
      throw new java.lang.IllegalArgumentException(
        "line "+line+" does not exist?!"
      );
    }
    var ch = it.next;
    while(it.hasNext && '\n' != ch) {
      buf.append( ch );
      ch = it.next;
    }
    val res = buf.toString();
    buf.setLength( 0 );
    res
  }

  /** returns true if this source has more characters
   */
  def hasNext = iter.hasNext;

  /** returns next character and has the following side-effects: updates
   *  position (ccol and cline) and assigns the character to ch
   */
  def next = {
    ch = iter.next;
    pos = Position.encode(cline,ccol);
    ch match {
      case '\n' =>
        ccol = 1;
        cline = cline + 1;
      case '\t' =>
        ccol = ccol + tabinc;
      case _ =>
        ccol = ccol + 1;
    }
    ch
  };


  /** reports an error message to console */
  def reportError(pos: Int, msg: String): Unit = {
    report(pos, msg, java.lang.System.out);
  }

  def reportError(pos: Int, msg: String, out: PrintStream): Unit = {
    nerrors = nerrors + 1;
    report(pos, msg, java.lang.System.out);
  }

  def report(pos: Int, msg: String, out: PrintStream): Unit = {
    val line = Position.line(pos);
    val col = Position.column(pos);
    Console.println(descr+":"+line+":"+col+": "+msg);
    Console.println(getLine(line));
    var i = 1;
    while( i < col ) {
      Console.print(' ');
      i = i + 1;
    }
    Console.println('^');
  }

  /** reports a warning message to java.lang.System.out */
  def reportWarning(pos: Int, msg: String): Unit =
    reportWarning(pos, msg, java.lang.System.out);

  def reportWarning(pos: Int, msg: String, out: PrintStream): Unit = {
    nwarnings = nwarnings + 1;
    report(pos, "warning! "+msg, out);
  }

  /** the actual reset method */
  def reset: Source;

}
