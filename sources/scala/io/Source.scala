/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.io;

/** convenience methods to create an iterable representation of a source file
 *  @author Burak Emir
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

  /** creates Source from array of characters, with empty description
   */
  def fromChars(chars: Array[Char]): Source = {
    val it = Iterator.fromArray(chars);
    new Source {
      val iter = it;
    }
  }
  /** creates Source from string, with empty description
   */
  def fromString(s: String): Source = {
    val it = Iterator.fromString(s);
    new Source {
      val iter = it;
    }
  }

  /** creates Source from file with given name, setting its description to
   *  filename.
   */
  def fromFile(name: String): Source =
    fromFile( new java.io.File( name ));

  /** creates Source from file with given name, using given encoding, setting
   *  its description to filename.
   */
  def fromFile(name: String, enc: String): Source =
    fromFile( new java.io.File( name ), enc);

  /** creates Source from file, using default character encoding, setting its
   *  description to filename.
   */
  def fromFile(file: java.io.File): Source = {
    val arr: Array[Byte] = new Array[Byte]( file.length().asInstanceOf[Int] );
    val is = new java.io.FileInputStream( file );
    is.read( arr );
    val s = fromBytes(arr);
    s.descr = file.getName();
    s
  }

  /** creates Source from file, using given character encoding, setting its
   *  description to filename.
   */
  def fromFile(file: java.io.File, enc: String): Source = {
    val arr: Array[Byte] = new Array[Byte]( file.length().asInstanceOf[Int] );
    val is = new java.io.FileInputStream( file );
    is.read( arr );
    val s = fromBytes(arr, enc);
    s.descr = file.getName();
    s
  }
}

/** an iterable representation of source files.
 *  @author Burak Emir
 */
abstract class Source extends Iterator[Char] {
  /** default col increment for tabs '\t', set to 4 */
  val tabinc = 4;

  protected val iter: Iterator[Char];
  /** returns next characters, updates positions and assigns the character
   *  ch as side effect
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
  /** returns true if this source has more characters
   */
  def hasNext = iter.hasNext;

  var cline = 1;
  var ccol = 1;
  /** position of last character returned by next*/
  var pos = 0;

  /** the last character returned by next.
   *  the value before the first call to next is undefined
   */
  var ch: Char = _;
  /** description of this source */
  var descr:String = "";
}
