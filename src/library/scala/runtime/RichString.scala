/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime

import scala.util.matching.Regex
import collection.generic._
//import collection.mutable.StringBuilder
import collection.immutable.Vector

object RichString {

  def newBuilder: Builder[Char, RichString] = new StringBuilder() mapResult (new RichString(_))
  implicit def builderFactory: BuilderFactory[Char, RichString, RichString] = new BuilderFactory[Char, RichString, RichString] { def apply(from: RichString) = newBuilder }
  implicit def builderFactory2: BuilderFactory[Char, RichString, String] = new BuilderFactory[Char, RichString, String] { def apply(from: String) = newBuilder }

  // just statics for rich string.
  private final val LF: Char = 0x0A
  private final val FF: Char = 0x0C
  private final val CR: Char = 0x0D
  private final val SU: Char = 0x1A
}

import RichString._

class RichString(val self: String) extends Proxy with Vector[Char] with VectorTemplate[Char, RichString] with PartialFunction[Int, Char] with Ordered[String] with Boxed {

  /** Creates a string builder buffer as builder for this class */
  override protected[this] def newBuilder = RichString.newBuilder

  /** Return element at index `n`
   *  @throws   IndexOutofBoundsException if the index is not valid
   */
  def apply(n: Int): Char = self charAt n

  def length: Int = self.length

  override def mkString = self
  override def toString = self

  /** return n times the current string
   */
  def * (n: Int): String = {
    val buf = new StringBuilder
    for (i <- 0 until n) buf append self
    buf.toString
  }

  override def compare(other: String) = self compareTo other

  private def isLineBreak(c: Char) = c == LF || c == FF

  /** <p>
   *    Strip trailing line end character from this string if it has one.
   *    A line end character is one of
   *  </p>
   *  <ul style="list-style-type: none;">
   *    <li>LF - line feed   (0x0A hex)</li>
   *    <li>FF - form feed   (0x0C hex)</li>
   *  </ul>
   *  <p>
   *    If a line feed character LF is preceded by a carriage return CR
   *    (0x0D hex), the CR character is also stripped (Windows convention).
   *  </p>
   */
  def stripLineEnd: String = {
    val len = self.length
    if (len == 0) self
    else {
      val last = apply(len - 1)
      if (isLineBreak(last))
        self.substring(0, if (last == LF && len >= 2 && apply(len - 2) == CR) len - 2 else len - 1)
      else
        self
    }
  }

  /** <p>
   *    Return all lines in this string in an iterator, including trailing
   *    line end characters.
   *  </p>
   *  <p>
   *    The number of strings returned is one greater than the number of line
   *    end characters in this string. For an empty string, a single empty
   *    line is returned. A line end character is one of
   *  </p>
   *  <ul style="list-style-type: none;">
   *    <li>LF - line feed   (0x0A hex)</li>
   *    <li>FF - form feed   (0x0C hex)</li>
   *  </ul>
   */
  def linesWithSeparators: Iterator[String] = new Iterator[String] {
    private val len = self.length
    private var index = 0
    def hasNext: Boolean = index < len
    def next(): String = {
      if (index >= len) throw new NoSuchElementException("next on empty iterator")
      val start = index
      while (index < len && !isLineBreak(apply(index))) index += 1
      index += 1
      self.substring(start, index min len)
    }
  }

  /** Return all lines in this string in an iterator, excluding trailing line
   *  end characters, i.e. apply <code>.stripLineEnd</code> to all lines
   *  returned by <code>linesWithSeparators</code>.
   */
  def lines: Iterator[String] =
    linesWithSeparators map (line => new RichString(line).stripLineEnd)

  /** Return all lines in this string in an iterator, excluding trailing line
   *  end characters, i.e. apply <code>.stripLineEnd</code> to all lines
   *  returned by <code>linesWithSeparators</code>.
   */
  def linesIterator: Iterator[String] =
    linesWithSeparators map (line => new RichString(line).stripLineEnd)

  /** Returns this string with first character converted to upper case */
  def capitalize: String =
    if (self == null) null
    else if (self.length == 0) ""
    else {
      val chars = self.toCharArray
      chars(0) = chars(0).toUpperCase
      new String(chars)
    }

  /** Returns this string with the given <code>prefix</code> stripped. */
  def stripPrefix(prefix: String) =
    if (self.startsWith(prefix)) self.substring(prefix.length)
    else self

  /** Returns this string with the given <code>suffix</code> stripped. */
  def stripSuffix(suffix: String) =
    if (self.endsWith(suffix)) self.substring(0, self.length() - suffix.length)
    else self

  /** <p>
   *    For every line in this string:
   *  </p>
   *  <blockquote>
   *     Strip a leading prefix consisting of blanks or control characters
   *     followed by <code>marginChar</code> from the line.
   *  </blockquote>
   */
  def stripMargin(marginChar: Char): String = {
    val buf = new StringBuilder
    for (line <- linesWithSeparators) {
      val len = line.length
      var index = 0
      while (index < len && line.charAt(index) <= ' ') index += 1
      buf append
        (if (index < len && line.charAt(index) == marginChar) line.substring(index + 1) else line)
    }
    buf.toString
  }

  /** <p>
   *    For every line in this string:
   *  </p>
   *  <blockquote>
   *     Strip a leading prefix consisting of blanks or control characters
   *     followed by <code>|</code> from the line.
   *  </blockquote>
   */
  def stripMargin: String = stripMargin('|')

  private def escape(ch: Char): String = "\\Q" + ch + "\\E"

  @throws(classOf[java.util.regex.PatternSyntaxException])
  def split(separator: Char): Array[String] = self.split(escape(separator))

  @throws(classOf[java.util.regex.PatternSyntaxException])
  def split(separators: Array[Char]): Array[String] = {
    val re = separators.foldLeft("[")(_+escape(_)) + "]"
    self.split(re)
  }

  /** You can follow a string with `.r', turning
   *  it into a Regex. E.g.
   *
   *  """A\w*""".r   is the regular expression for identifiers starting with `A'.
   */
  def r: Regex = new Regex(self)

  def toBoolean: Boolean = parseBoolean(self)
  def toByte: Byte       = java.lang.Byte.parseByte(self)
  def toShort: Short     = java.lang.Short.parseShort(self)
  def toInt: Int         = java.lang.Integer.parseInt(self)
  def toLong: Long       = java.lang.Long.parseLong(self)
  def toFloat: Float     = java.lang.Float.parseFloat(self)
  def toDouble: Double   = java.lang.Double.parseDouble(self)

  private def parseBoolean(s: String): Boolean =
    if (s != null) s.toLowerCase match {
      case "true" => true
      case "false" => false
      case _ => throw new NumberFormatException("For input string: \""+s+"\"")
    }
    else
      throw new NumberFormatException("For input string: \"null\"")

  /* !!! causes crash?
  def toArray: Array[Char] = {
    val result = new Array[Char](length)
    self.getChars(0, length, result, 0)
    result
  }
  */

  /** <p>
   *  Uses the underlying string as a pattern (in a fashion similar to
   *  printf in C), and uses the supplied arguments to fill in the
   *  holes.
   *  </p>
   *  <p>
   *    The interpretation of the formatting patterns is described in
   *    <a href="" target="contentFrame" class="java/util/Formatter">
   *    <code>java.util.Formatter</code></a>.
   *  </p>
   *
   *  @param args the arguments used to instantiating the pattern.
   *  @throws java.lang.IllegalArgumentException
   */
  def format(args : Any*) : String =
    java.lang.String.format(self, args.asInstanceOf[Seq[AnyRef]].toArray: _*)

  /** <p>
   *  Like format(args*) but takes an initial Locale parameter
   *  which influences formatting as in java.lang.String's format.
   *  </p>
   *  <p>
   *    The interpretation of the formatting patterns is described in
   *    <a href="" target="contentFrame" class="java/util/Formatter">
   *    <code>java.util.Formatter</code></a>.
   *  </p>
   *
   *  @param locale an instance of java.util.Locale
   *  @param args the arguments used to instantiating the pattern.
   *  @throws java.lang.IllegalArgumentException
   */
  def format(l: java.util.Locale, args: Any*): String =
    java.lang.String.format(l, self, args.asInstanceOf[Seq[AnyRef]].toArray: _*)
}

