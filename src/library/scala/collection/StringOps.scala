package scala
package collection

import java.lang.{IllegalArgumentException, StringBuilder}
import java.util.{Arrays, NoSuchElementException}
import scala.util.matching.Regex
import scala.math.{Ordered, ScalaNumber}
import scala.reflect.ClassTag

object StringOps {
  // just statics for companion class.
  private final val LF = 0x0A
  private final val FF = 0x0C
  private final val CR = 0x0D
  private final val SU = 0x1A
}

import StringOps._

final class StringOps(private val s: String)
  extends AnyVal
    with IterableOnce[Char]
    with collection.IndexedSeqOps[Char, immutable.IndexedSeq, String]
    with collection.StrictOptimizedIterableOps[Char, immutable.IndexedSeq, String]
    with Ordered[String] {

  def toIterable = StringView(s)
  override def view: StringView = StringView(s)
  protected def coll: String = s
  override def toSeq: immutable.Seq[Char] = s.toCharArray

  protected def fromSpecificIterable(coll: Iterable[Char]): String = {
    val sb = new StringBuilder
    for (ch <- coll) sb.append(ch)
    sb.toString
  }

  def iterableFactory = immutable.IndexedSeq

  protected def newSpecificBuilder() = new mutable.StringBuilder

  def length = s.length

  @throws[StringIndexOutOfBoundsException]
  def apply(i: Int) = s.charAt(i)

  override def className = "String"

  // Overloaded version of `map` that gives back a string, where the inherited
  //  version gives back a sequence.
  /** Builds a new collection by applying a function to all elements of this $coll.
    *
    *  @param f      the function to apply to each element.
    *  @return       a new String resulting from applying the given function
    *                `f` to each element of this String and collecting the results.
    */
  def map(f: Char => Char): String = {
    val len = s.length
    val dst = new Array[Char](len)
    var i = 0
    while (i < len) {
      dst(i) = f(s charAt i)
      i += 1
    }
    new String(dst)
  }

  // Overloaded version of `flatMap` that gives back a string, where the inherited
  // version gives back a sequence.
  /** Builds a new collection by applying a function to all elements of this String
    *  and using the elements of the resulting collections.
    *
    *  @param f      the function to apply to each element.
    *  @return       a new String resulting from applying the given string-valued function
    *                `f` to each element of this String and concatenating the results.
    */
  def flatMap(f: Char => String): String = {
    val len = s.length
    val sb = new StringBuilder
    var i = 0
    while (i < len) {
      sb append f(s.charAt(i))
      i += 1
    }
    sb.toString
  }

  // Overloaded version of `++` that gives back a string, where the inherited
  //  version gives back a sequence.
  /** Returns a new String containing the elements from the left hand operand followed by the elements from the
    *  right hand operand.
    *
    *  @param suffix   the traversable to append.
    *  @return       a new String which contains all elements
    *                of this String followed by all elements of `suffix`.
    */
  def concat(suffix: IterableOnce[Char]): String = {
    val sb = new StringBuilder(s)
    for (ch <- suffix.iterator()) sb.append(ch)
    sb.toString
  }

  // Overloaded version of `padTo` that gives back a string, where the inherited
  //  version gives back a sequence.
  /** Returns a String with a Char appended until a given target length is reached.
    *
    *  @param   len   the target length
    *  @param   elem  the padding value
    *  @return a String consisting of
    *          this String followed by the minimal number of occurrences of `elem` so
    *          that the resulting String has a length of at least `len`.
    */
  def padTo(len: Int, elem: Char): String = {
    val sLen = s.length
    if (sLen >= len) s else {
      val sb = new StringBuilder(len)
      sb.append(s)
      // With JDK 11, this can written as:
      // sb.append(String.valueOf(elem).repeat(len - sLen))
      var i = sLen
      while (i < len) {
        sb.append(elem)
        i += 1
      }
      sb.toString
    }
  }

  /** Alias for `concat` */
  @`inline` def ++(suffix: IterableOnce[Char]): String = concat(suffix)

  /** Another overloaded version of `++`. */
  def ++(xs: String): String = s + xs

  /** A copy of the String with an element prepended */
  def prepended(c: Char): String =
    new StringBuilder(s.length + 1).append(c).append(s).toString

  /** Alias for `prepended` */
  @`inline` def +: (c: Char): String = prepended(c)

  /** A copy of the String with an element appended */
  def appended(c: Char): String =
    new StringBuilder(s.length + 1).append(s).append(c).toString

  /** Alias for `appended` */
  @`inline` def :+ (c: Char): String = appended(c)

  /** Produces a new String where a slice of characters in this String is replaced by another String.
    *
    * Patching at negative indices is the same as patching starting at 0.
    * Patching at indices at or larger than the length of the original String appends the patch to the end.
    * If more values are replaced than actually exist, the excess is ignored.
    *
    *  @param  from     the index of the first replaced char
    *  @param  other    the replacement String
    *  @param  replaced the number of chars to drop in the original String
    *  @return          a new String consisting of all chars of this String
    *                   except that `replaced` chars starting from `from` are replaced
    *                   by `other`.
    */
  def patch(from: Int, other: String, replaced: Int): String =
    fromSpecificIterable(new View.Patched(this, from, other, replaced)) //TODO optimize

  /** A copy of this string with one single replaced element.
    *  @param  index  the position of the replacement
    *  @param  elem   the replacing element
    *  @return a new string which is a copy of this string with the element at position `index` replaced by `elem`.
    *  @throws IndexOutOfBoundsException if `index` does not satisfy `0 <= index < length`.
    */
  def updated(index: Int, elem: Char): String = {
    val sb = new StringBuilder(s.length).append(s)
    sb.setCharAt(index, elem)
    sb.toString
  }

  // override for performance
  /** Tests whether this $coll contains the given character.
   *
   *  @param elem  the character to test.
   *  @return     `true` if this $coll has an element that is equal (as
   *              determined by `==`) to `elem`, `false` otherwise.
   */
  override def contains[C >: Char](elem: C): Boolean = elem match {
    case c: Char => s.indexOf(c) >= 0
    case _ => super.contains(elem)
  }

  override def toString = s

  override def mkString = s

  override def slice(from: Int, until: Int): String = {
    val start = from max 0
    val end   = until min length

    if (start >= end) ""
    else s.substring(start, end)
  }

  // Note: String.repeat is added in JDK 11.
  /** Return the current string concatenated `n` times.
   */
  def *(n: Int): String = {
    val sb = new StringBuilder(s.length * n)
    var i = 0
    while (i < n) {
      sb.append(s)
      i += 1
    }
    sb.toString
  }

  override def compare(other: String) = s compareTo other

  @inline private def isLineBreak(c: Char) = c == LF || c == FF

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
    val len = s.length
    if (len == 0) s
    else {
      val last = apply(len - 1)
      if (isLineBreak(last))
        s.substring(0, if (last == LF && len >= 2 && apply(len - 2) == CR) len - 2 else len - 1)
      else
        s
    }
  }

  /** Return all lines in this string in an iterator, including trailing
   *  line end characters.
   *
   *  This method is analogous to `s.split(EOL).toIterator`,
   *  except that any existing line endings are preserved in the result strings,
   *  and the empty string yields an empty iterator.
   *
   *  A line end character is one of
   *  - `LF` - line feed   (`0x0A`)
   *  - `FF` - form feed   (`0x0C`)
   */
  def linesWithSeparators: Iterator[String] = new Iterator[String] {
    private val len = s.length
    private var index = 0
    def hasNext: Boolean = index < len
    def next(): String = {
      if (index >= len) throw new NoSuchElementException("next on empty iterator")
      val start = index
      while (index < len && !isLineBreak(apply(index))) index += 1
      index += 1
      s.substring(start, index min len)
    }
  }

  /** Return all lines in this string in an iterator, excluding trailing line
   *  end characters; i.e., apply `.stripLineEnd` to all lines
   *  returned by `linesWithSeparators`.
   */
  def lines: Iterator[String] =
    linesWithSeparators map (_.stripLineEnd)

  /** Returns this string with first character converted to upper case.
   * If the first character of the string is capitalized, it is returned unchanged.
   * This method does not convert characters outside the Basic Multilingual Plane (BMP).
   */
  def capitalize: String =
    if (s == null || s.length == 0 || !s.charAt(0).isLower) s
    else updated(0, s.charAt(0).toUpper)

  /** Returns this string with the given `prefix` stripped. If this string does not
   *  start with `prefix`, it is returned unchanged.
   */
  def stripPrefix(prefix: String) =
    if (s startsWith prefix) s.substring(prefix.length)
    else s

  /** Returns this string with the given `suffix` stripped. If this string does not
   *  end with `suffix`, it is returned unchanged.
   */
  def stripSuffix(suffix: String) =
    if (s endsWith suffix) s.substring(0, s.length - suffix.length)
    else s

  /** Replace all literal occurrences of `literal` with the literal string `replacement`.
   *  This method is equivalent to [[java.lang.String#replace]].
   *
   *  @param    literal     the string which should be replaced everywhere it occurs
   *  @param    replacement the replacement string
   *  @return               the resulting string
   */
  def replaceAllLiterally(literal: String, replacement: String): String = s.replace(literal, replacement)

  /** For every line in this string:
   *
   *  Strip a leading prefix consisting of blanks or control characters
   *  followed by `marginChar` from the line.
   */
  def stripMargin(marginChar: Char): String = {
    val sb = new StringBuilder(s.length)
    for (line <- linesWithSeparators) {
      val len = line.length
      var index = 0
      while (index < len && line.charAt(index) <= ' ') index += 1
      sb.append {
        if (index < len && line.charAt(index) == marginChar) line.substring(index + 1)
        else line
      }
    }
    sb.toString
  }

  /** For every line in this string:
   *
   *  Strip a leading prefix consisting of blanks or control characters
   *  followed by `|` from the line.
   */
  def stripMargin: String = stripMargin('|')

  private def escape(ch: Char): String = if (
    (ch >= 'a') && (ch <= 'z') ||
    (ch >= 'A') && (ch <= 'Z') ||
    (ch >= '0' && ch <= '9')) ch.toString
    else "\\" + ch

  /** Split this string around the separator character
   *
   * If this string is the empty string, returns an array of strings
   * that contains a single empty string.
   *
   * If this string is not the empty string, returns an array containing
   * the substrings terminated by the start of the string, the end of the
   * string or the separator character, excluding empty trailing substrings
   *
   * If the separator character is a surrogate character, only split on
   * matching surrogate characters if they are not part of a surrogate pair
   *
   * The behaviour follows, and is implemented in terms of <a href="http://docs.oracle.com/javase/7/docs/api/java/lang/String.html#split%28java.lang.String%29">String.split(re: String)</a>
   *
   *
   * @example {{{
   * "a.b".split('.') //returns Array("a", "b")
   *
   * //splitting the empty string always returns the array with a single
   * //empty string
   * "".split('.') //returns Array("")
   *
   * //only trailing empty substrings are removed
   * "a.".split('.') //returns Array("a")
   * ".a.".split('.') //returns Array("", "a")
   * "..a..".split('.') //returns Array("", "", "a")
   *
   * //all parts are empty and trailing
   * ".".split('.') //returns Array()
   * "..".split('.') //returns Array()
   *
   * //surrogate pairs
   * val high = 0xD852.toChar
   * val low = 0xDF62.toChar
   * val highstring = high.toString
   * val lowstring = low.toString
   *
   * //well-formed surrogate pairs are not split
   * val highlow = highstring + lowstring
   * highlow.split(high) //returns Array(highlow)
   *
   * //bare surrogate characters are split
   * val bare = "_" + highstring + "_"
   * bare.split(high) //returns Array("_", "_")
   *
   *  }}}
   *
   * @param separator the character used as a delimiter
   */
  def split(separator: Char): Array[String] = s.split(escape(separator))


  @throws(classOf[java.util.regex.PatternSyntaxException])
  def split(separators: Array[Char]): Array[String] = {
    val re = separators.foldLeft("[")(_+escape(_)) + "]"
    s.split(re)
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
  def r(groupNames: String*): Regex = new Regex(s, groupNames: _*)

  /**
   * @throws java.lang.IllegalArgumentException  If the string does not contain a parsable `Boolean`.
   */
  def toBoolean: Boolean = parseBoolean(s)
  /**
   * Parse as a `Byte` (string must contain only decimal digits and optional leading `-`).
   * @throws java.lang.NumberFormatException  If the string does not contain a parsable `Byte`.
   */
  def toByte: Byte       = java.lang.Byte.parseByte(s)
  /**
   * Parse as a `Short` (string must contain only decimal digits and optional leading `-`).
   * @throws java.lang.NumberFormatException  If the string does not contain a parsable `Short`.
   */
  def toShort: Short     = java.lang.Short.parseShort(s)
  /**
   * Parse as an `Int` (string must contain only decimal digits and optional leading `-`).
   * @throws java.lang.NumberFormatException  If the string does not contain a parsable `Int`.
   */
  def toInt: Int         = java.lang.Integer.parseInt(s)
  /**
   * Parse as a `Long` (string must contain only decimal digits and optional leading `-`).
   * @throws java.lang.NumberFormatException  If the string does not contain a parsable `Long`.
   */
  def toLong: Long       = java.lang.Long.parseLong(s)
  /**
    * Parse as a `Float` (surrounding whitespace is removed with a `trim`).
    * @throws java.lang.NumberFormatException  If the string does not contain a parsable `Float`.
    * @throws java.lang.NullPointerException  If the string is null.
   */
  def toFloat: Float     = java.lang.Float.parseFloat(s)
  /**
    * Parse as a `Double` (surrounding whitespace is removed with a `trim`).
    * @throws java.lang.NumberFormatException  If the string does not contain a parsable `Double`.
    * @throws java.lang.NullPointerException  If the string is null.
   */
  def toDouble: Double   = java.lang.Double.parseDouble(s)

  private def parseBoolean(s: String): Boolean =
    if (s != null) s.toLowerCase match {
      case "true" => true
      case "false" => false
      case _ => throw new IllegalArgumentException("For input string: \""+s+"\"")
    }
    else
      throw new IllegalArgumentException("For input string: \"null\"")

  override def toArray[B >: Char : ClassTag]: Array[B] =
    s.toCharArray.asInstanceOf[Array[B]]

  private def unwrapArg(arg: Any): AnyRef = arg match {
    case x: ScalaNumber => x.underlying
    case x              => x.asInstanceOf[AnyRef]
  }

  /** Uses the underlying string as a pattern (in a fashion similar to
   *  printf in C), and uses the supplied arguments to fill in the
   *  holes.
   *
   *    The interpretation of the formatting patterns is described in
   *    [[java.util.Formatter]], with the addition that
   *    classes deriving from `ScalaNumber` (such as [[scala.BigInt]] and
   *    [[scala.BigDecimal]]) are unwrapped to pass a type which `Formatter`
   *    understands.
   *
   *  @param args the arguments used to instantiating the pattern.
   *  @throws java.lang.IllegalArgumentException
   */
  def format(args : Any*): String =
    java.lang.String.format(s, args map unwrapArg: _*)

  /** Like `format(args*)` but takes an initial `Locale` parameter
   *  which influences formatting as in `java.lang.String`'s format.
   *
   *    The interpretation of the formatting patterns is described in
   *    [[java.util.Formatter]], with the addition that
   *    classes deriving from `ScalaNumber` (such as `scala.BigInt` and
   *    `scala.BigDecimal`) are unwrapped to pass a type which `Formatter`
   *    understands.
   *
   *  @param l    an instance of `java.util.Locale`
   *  @param args the arguments used to instantiating the pattern.
   *  @throws java.lang.IllegalArgumentException
   */
  def formatLocal(l: java.util.Locale, args: Any*): String =
    java.lang.String.format(l, s, args map unwrapArg: _*)
}

case class StringView(s: String) extends AbstractIndexedView[Char] {
  def length = s.length
  @throws[StringIndexOutOfBoundsException]
  def apply(n: Int) = s.charAt(n)
  override def className = "StringView"
}

