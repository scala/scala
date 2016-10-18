/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package immutable

import mutable.{ ArrayBuilder, Builder }
import scala.util.matching.Regex
import scala.math.ScalaNumber
import scala.reflect.ClassTag

/** A companion object for the `StringLike` containing some constants.
 *  @since 2.8
 */
object StringLike {
  // just statics for companion class.
  private final val LF = 0x0A
  private final val FF = 0x0C
  private final val CR = 0x0D
  private final val SU = 0x1A
}

import StringLike._

/** A trait describing stringlike collections.
 *
 *  @tparam Repr   The type of the actual collection inheriting `StringLike`.
 *
 *  @since 2.8
 *  @define Coll `String`
 *  @define coll string
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
trait StringLike[+Repr] extends Any with scala.collection.IndexedSeqOptimized[Char, Repr] with Ordered[String] {
self =>

  /** Creates a string builder buffer as builder for this class */
  protected[this] def newBuilder: Builder[Char, Repr]

  /** Return element at index `n`
   *  @throws   IndexOutOfBoundsException if the index is not valid
   */
  def apply(n: Int): Char = toString charAt n

  def length: Int = toString.length

  override def mkString = toString

  override def slice(from: Int, until: Int): Repr = {
    val start = from max 0
    val end   = until min length

    if (start >= end) newBuilder.result()
    else (newBuilder ++= toString.substring(start, end)).result()
  }

  /** Return the current string concatenated `n` times.
   */
  def * (n: Int): String = {
    val buf = new StringBuilder
    for (i <- 0 until n) buf append toString
    buf.toString
  }

  override def compare(other: String) = toString compareTo other

  private def isLineBreak(c: Char) = c == LF || c == FF

  /**
   *  Strip trailing line end character from this string if it has one.
   *
   *  A line end character is one of
   *  - `LF` - line feed   (`0x0A` hex)
   *  - `FF` - form feed   (`0x0C` hex)
   *
   *  If a line feed character `LF` is preceded by a carriage return `CR`
   *  (`0x0D` hex), the `CR` character is also stripped (Windows convention).
   */
  def stripLineEnd: String = {
    val len = toString.length
    if (len == 0) toString
    else {
      val last = apply(len - 1)
      if (isLineBreak(last))
        toString.substring(0, if (last == LF && len >= 2 && apply(len - 2) == CR) len - 2 else len - 1)
      else
        toString
    }
  }

  /** Return all lines in this string in an iterator, including trailing
   *  line end characters.
   *
   *  The number of strings returned is one greater than the number of line
   *  end characters in this string. For an empty string, a single empty
   *  line is returned. A line end character is one of
   *  - `LF` - line feed   (`0x0A` hex)
   *  - `FF` - form feed   (`0x0C` hex)
   */
  def linesWithSeparators: Iterator[String] = new AbstractIterator[String] {
    val str = self.toString
    private val len = str.length
    private var index = 0
    def hasNext: Boolean = index < len
    def next(): String = {
      if (index >= len) throw new NoSuchElementException("next on empty iterator")
      val start = index
      while (index < len && !isLineBreak(apply(index))) index += 1
      index += 1
      str.substring(start, index min len)
    }
  }

  /** Return all lines in this string in an iterator, excluding trailing line
   *  end characters, i.e., apply `.stripLineEnd` to all lines
   *  returned by `linesWithSeparators`.
   */
  def lines: Iterator[String] =
    linesWithSeparators map (line => new WrappedString(line).stripLineEnd)

  /** Return all lines in this string in an iterator, excluding trailing line
   *  end characters, i.e., apply `.stripLineEnd` to all lines
   *  returned by `linesWithSeparators`.
   */
  @deprecated("Use `lines` instead.","2.11.0")
  def linesIterator: Iterator[String] =
    linesWithSeparators map (line => new WrappedString(line).stripLineEnd)

  /** Returns this string with first character converted to upper case.
   * If the first character of the string is capitalized, it is returned unchanged.
   * This method does not convert characters outside the Basic Multilingual Plane (BMP).
   */
  def capitalize: String =
    if (toString == null) null
    else if (toString.length == 0) ""
    else if (toString.charAt(0).isUpper) toString
    else {
      val chars = toString.toCharArray
      chars(0) = chars(0).toUpper
      new String(chars)
    }

  /** Returns this string with the given `prefix` stripped. If this string does not
   *  start with `prefix`, it is returned unchanged.
   */
  def stripPrefix(prefix: String) =
    if (toString.startsWith(prefix)) toString.substring(prefix.length)
    else toString

  /** Returns this string with the given `suffix` stripped. If this string does not
   *  end with `suffix`, it is returned unchanged.
   */
  def stripSuffix(suffix: String) =
    if (toString.endsWith(suffix)) toString.substring(0, toString.length() - suffix.length)
    else toString

  /** Replace all literal occurrences of `literal` with the string `replacement`.
   *  This is equivalent to [[java.lang.String#replaceAll]] except that both arguments
   *  are appropriately quoted to avoid being interpreted as metacharacters.
   *
   *  @param    literal     the string which should be replaced everywhere it occurs
   *  @param    replacement the replacement string
   *  @return               the resulting string
   */
  def replaceAllLiterally(literal: String, replacement: String): String = {
    val arg1 = Regex.quote(literal)
    val arg2 = Regex.quoteReplacement(replacement)

    toString.replaceAll(arg1, arg2)
  }

  /** For every line in this string:
   *
   *  Strip a leading prefix consisting of blanks or control characters
   *  followed by `marginChar` from the line.
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

  /** For every line in this string:
   *
   *  Strip a leading prefix consisting of blanks or control characters
   *  followed by `|` from the line.
   */
  def stripMargin: String = stripMargin('|')

  private def escape(ch: Char): String = "\\Q" + ch + "\\E"

  def split(separator: Char): Array[String] = {
    val thisString = toString
    var pos = thisString.indexOf(separator)

    if (pos != -1) {
      val res = new ArrayBuilder.ofRef[String]

      var prev = 0
      do {
        res += thisString.substring(prev, pos)
        prev = pos + 1
        pos = thisString.indexOf(separator, prev)
      } while (pos != -1)

      if (prev != thisString.length)
        res += thisString.substring(prev, thisString.length)

      val initialResult = res.result()
      pos = initialResult.length
      while (pos > 0 && initialResult(pos - 1).isEmpty) pos = pos - 1
      if (pos != initialResult.length) {
        val trimmed = new Array[String](pos)
        Array.copy(initialResult, 0, trimmed, 0, pos)
        trimmed
      } else initialResult
    } else Array[String](thisString)
  }

  @throws(classOf[java.util.regex.PatternSyntaxException])
  def split(separators: Array[Char]): Array[String] = {
    val re = separators.foldLeft("[")(_+escape(_)) + "]"
    toString.split(re)
  }

  /** You can follow a string with `.r`, turning it into a `Regex`. E.g.
   *
   *  `"""A\w*""".r`   is the regular expression for identifiers starting with `A`.
   */
  def r: Regex = r()

  /** You can follow a string with `.r(g1, ... , gn)`, turning it into a `Regex`,
   *  with group names g1 through gn.
   *
   *  `"""(\d\d)-(\d\d)-(\d\d\d\d)""".r("month", "day", "year")` matches dates
   *  and provides its subcomponents through groups named "month", "day" and
   *  "year".
   *
   *  @param groupNames The names of the groups in the pattern, in the order they appear.
   */
  def r(groupNames: String*): Regex = new Regex(toString, groupNames: _*)

  /**
   * @throws java.lang.IllegalArgumentException - If the string does not contain a parsable boolean.
   */
  def toBoolean: Boolean = parseBoolean(toString)
  /**
   * @throws java.lang.NumberFormatException - If the string does not contain a parsable byte.
   */
  def toByte: Byte       = java.lang.Byte.parseByte(toString)
  /**
   * @throws java.lang.NumberFormatException - If the string does not contain a parsable short.
   */
  def toShort: Short     = java.lang.Short.parseShort(toString)
  /**
   * @throws java.lang.NumberFormatException  - If the string does not contain a parsable int.
   */
  def toInt: Int         = java.lang.Integer.parseInt(toString)
  /**
   * @throws java.lang.NumberFormatException  - If the string does not contain a parsable long.
   */
  def toLong: Long       = java.lang.Long.parseLong(toString)
  /**
   * @throws java.lang.NumberFormatException - If the string does not contain a parsable float.
   */
  def toFloat: Float     = java.lang.Float.parseFloat(toString)
  /**
   * @throws java.lang.NumberFormatException - If the string does not contain a parsable double.
   */
  def toDouble: Double   = java.lang.Double.parseDouble(toString)

  private def parseBoolean(s: String): Boolean =
    if (s != null) s.toLowerCase match {
      case "true" => true
      case "false" => false
      case _ => throw new IllegalArgumentException("For input string: \""+s+"\"")
    }
    else
      throw new IllegalArgumentException("For input string: \"null\"")

  override def toArray[B >: Char : ClassTag]: Array[B] =
    toString.toCharArray.asInstanceOf[Array[B]]

  private def unwrapArg(arg: Any): AnyRef = arg match {
    case x: ScalaNumber => x.underlying
    case x              => x.asInstanceOf[AnyRef]
  }

  /** Uses the underlying string as a pattern (in a fashion similar to
   *  printf in C), and uses the supplied arguments to fill in the
   *  holes.
   *
   *    The interpretation of the formatting patterns is described in
   *    <a href="" target="contentFrame" class="java/util/Formatter">
   *    `java.util.Formatter`</a>, with the addition that
   *    classes deriving from `ScalaNumber` (such as [[scala.BigInt]] and
   *    [[scala.BigDecimal]]) are unwrapped to pass a type which `Formatter`
   *    understands.
   *
   *  @param args the arguments used to instantiating the pattern.
   *  @throws java.lang.IllegalArgumentException
   */
  def format(args : Any*): String =
    java.lang.String.format(toString, args map unwrapArg: _*)

  /** Like `format(args*)` but takes an initial `Locale` parameter
   *  which influences formatting as in `java.lang.String`'s format.
   *
   *    The interpretation of the formatting patterns is described in
   *    <a href="" target="contentFrame" class="java/util/Formatter">
   *    `java.util.Formatter`</a>, with the addition that
   *    classes deriving from `ScalaNumber` (such as `scala.BigInt` and
   *    `scala.BigDecimal`) are unwrapped to pass a type which `Formatter`
   *    understands.
   *
   *  @param l    an instance of `java.util.Locale`
   *  @param args the arguments used to instantiating the pattern.
   *  @throws java.lang.IllegalArgumentException
   */
  def formatLocal(l: java.util.Locale, args: Any*): String =
    java.lang.String.format(l, toString, args map unwrapArg: _*)
}
