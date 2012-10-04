/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


/**
 * This package is concerned with regular expression (regex) matching against strings,
 * with the main goal of pulling out information from those matches, or replacing
 * them with something else.
 *
 * There are four classes and three objects, with most of them being members of
 * Regex companion object. [[scala.util.matching.Regex]] is the class users instantiate
 * to do regular expression matching.
 *
 * The remaining classes and objects in the package are used in the following way:
 *
 * * The companion object to [[scala.util.matching.Regex]] just contains the other members.
 * * [[scala.util.matching.Regex.Match]] makes more information about a match available.
 * * [[scala.util.matching.Regex.MatchIterator]] is used to iterate over multiple matches.
 * * [[scala.util.matching.Regex.MatchData]] is just a base trait for the above classes.
 * * [[scala.util.matching.Regex.Groups]] extracts group from a [[scala.util.matching.Regex.Match]]
 *   without recomputing the match.
 * * [[scala.util.matching.Regex.Match]] converts a [[scala.util.matching.Regex.Match]]
 *   into a [[java.lang.String]].
 *
 */
package scala.util.matching

import scala.collection.AbstractIterator
import java.util.regex.{ Pattern, Matcher }

/** This class provides methods for creating and using regular expressions.
 *  It is based on the regular expressions of the JDK since 1.4.
 *
 *  Its main goal is to extract strings that match a pattern, or the subgroups
 *  that make it up. For that reason, it is usually used with for comprehensions
 *  and matching (see methods for examples).
 *
 *  A Regex is created from a [[java.lang.String]] representation of the
 *  regular expression pattern^1^. That pattern is compiled
 *  during construction, so frequently used patterns should be declared outside
 *  loops if performance is of concern. Possibly, they might be declared on a
 *  companion object, so that they need only to be initialized once.
 *
 *  The canonical way of creating regex patterns is by using the method `r`, provided
 *  on [[java.lang.String]] through an implicit conversion into
 *  [[scala.collection.immutable.WrappedString]]. Using triple quotes to write these
 *  strings avoids having to quote the backslash character (`\`).
 *
 *  Using the constructor directly, on the other hand, makes
 *  it possible to declare names for subgroups in the pattern.
 *
 *  For example, both declarations below generate the same regex, but the second
 *  one associate names with the subgroups.
 *
 *  {{{
 *  val dateP1 = """(\d\d\d\d)-(\d\d)-(\d\d)""".r
 *  val dateP2 = new scala.util.matching.Regex("""(\d\d\d\d)-(\d\d)-(\d\d)""", "year", "month", "day")
 *  }}}
 *
 *  There are two ways of using a `Regex` to find a pattern: calling methods on
 *  Regex, such as `findFirstIn` or `findAllIn`, or using it as an extractor in a
 *  pattern match.
 *
 *  Note, however, that when Regex is used as an extractor in a pattern match, it
 *  only succeeds if the whole text can be matched. For this reason, one usually
 *  calls a method to find the matching substrings, and then use it as an extractor
 *  to break match into subgroups.
 *
 *  As an example, the above patterns can be used like this:
 *
 *  {{{
 *  val dateP1(year, month, day) = "2011-07-15"
 *
 *  // val dateP1(year, month, day) = "Date 2011-07-15" // throws an exception at runtime
 *
 *  val copyright: String = dateP1 findFirstIn "Date of this document: 2011-07-15" match {
 *    case Some(dateP1(year, month, day)) => "Copyright "+year
 *    case None                           => "No copyright"
 *  }
 *
 *  val copyright: Option[String] = for {
 *    dateP1(year, month, day) <- dateP1 findFirstIn "Last modified 2011-07-15"
 *  } yield year

 *  def getYears(text: String): Iterator[String] = for (dateP1(year, _, _) <- dateP1 findAllIn text) yield year
 *  def getFirstDay(text: String): Option[String] = for (m <- dateP2 findFirstMatchIn text) yield m group "day"
 *  }}}
 *
 *  Regex does not provide a method that returns a [[scala.Boolean]]. One can
 *  use [[java.lang.String]] `matches` method, or, if `Regex` is preferred,
 *  either ignore the return value or test the `Option` for emptyness. For example:
 *
 *  {{{
 *  def hasDate(text: String): Boolean = (dateP1 findFirstIn text).nonEmpty
 *  def printLinesWithDates(lines: Traversable[String]) {
 *    lines foreach { line =>
 *      dateP1 findFirstIn line foreach { _ => println(line) }
 *    }
 *  }
 *  }}}
 *
 *  There are also methods that can be used to replace the patterns
 *  on a text. The substitutions can be simple replacements, or more
 *  complex functions. For example:
 *
 *  {{{
 *  val months = Map( 1 -> "Jan", 2 -> "Feb", 3 -> "Mar",
 *                    4 -> "Apr", 5 -> "May", 6 -> "Jun",
 *                    7 -> "Jul", 8 -> "Aug", 9 -> "Sep",
 *                    10 -> "Oct", 11 -> "Nov", 12 -> "Dec")
 *
 *  import scala.util.matching.Regex.Match
 *  def reformatDate(text: String) = dateP2 replaceAllIn ( text, (m: Match) =>
 *    "%s %s, %s" format (months(m group "month" toInt), m group "day", m group "year")
 *  )
 *  }}}
 *
 *  You can use special pattern syntax constructs like `(?idmsux-idmsux)`¹ to switch
 *  various regex compilation options like `CASE_INSENSITIVE` or `UNICODE_CASE`.
 *
 *  @note ¹ A detailed description is available in [[java.util.regex.Pattern]].
 *  @see [[java.util.regex.Pattern]]
 *
 *  @author  Thibaud Hottelier
 *  @author  Philipp Haller
 *  @author  Martin Odersky
 *  @version 1.1, 29/01/2008
 *
 *  @param pattern    The compiled pattern
 *  @param groupNames A mapping from names to indices in capture groups
 *
 *  @define replacementString
 *  In the replacement String, a dollar sign (`$`) followed by a number will be
 *  interpreted as a reference to a group in the matched pattern, with numbers
 *  1 through 9 corresponding to the first nine groups, and 0 standing for the
 *  whole match. Any other character is an error. The backslash (`\`) character
 *  will be interpreted as an escape character, and can be used to escape the
 *  dollar sign. One can use [[scala.util.matching.Regex]]'s `quoteReplacement`
 *  to automatically escape these characters.
 */
@SerialVersionUID(-2094783597747625537L)
class Regex private[matching](val pattern: Pattern, groupNames: String*) extends Serializable {
  outer =>

  import Regex._

  /**
    *  @param regex      A string representing a regular expression
    *  @param groupNames A mapping from names to indices in capture groups
    */
  def this(regex: String, groupNames: String*) = this(Pattern.compile(regex), groupNames: _*)

  /** Tries to match a [[java.lang.CharSequence]].
   *  If the match succeeds, the result is a list of the matching
   *  groups (or a `null` element if a group did not match any input).
   *  If the pattern specifies no groups, then the result will be an empty list
   *  on a successful match.
   *
   *  This method attempts to match the entire input by default; to find the next
   *  matching subsequence, use an unanchored Regex.

   *  For example:
   *
   *  {{{
   *  val p1 = "ab*c".r
   *  val p1Matches = "abbbc" match {
   *    case p1() => true
   *    case _    => false
   *  }
   *  val p2 = "a(b*)c".r
   *  val numberOfB = "abbbc" match {
   *    case p2(b) => Some(b.length)
   *    case _     => None
   *  }
   *  val p3 = "b*".r.unanchored
   *  val p3Matches = "abbbc" match {
   *    case p3() => true
   *    case _    => false
   *  }
   *  }}}
   *
   *  @param  s     The string to match
   *  @return       The matches
   */
  def unapplySeq(s: CharSequence): Option[Seq[String]] = {
    val m = pattern matcher s
    if (runMatcher(m)) Some(1 to m.groupCount map m.group)
    else None
  }

  /** Tries to match on a [[scala.util.matching.Regex.Match]].
   *  A previously failed match results in None.
   *  If a successful match was made against the current pattern, then that result is used.
   *  Otherwise, this Regex is applied to the previously matched input,
   *  and the result of that match is used.
   */
  def unapplySeq(m: Match): Option[Seq[String]] = 
    if (m.matched == null) None
    else if (m.matcher.pattern == this.pattern) Some(1 to m.groupCount map m.group)
    else unapplySeq(m.matched)

  @deprecated("Extracting a match result from anything but a CharSequence or Match is deprecated", "2.10.0")
  def unapplySeq(target: Any): Option[List[String]] = target match {
    case s: CharSequence =>
      val m = pattern matcher s
      if (runMatcher(m)) Some((1 to m.groupCount).toList map m.group)
      else None
    case m: Match        => unapplySeq(m.matched)
    case _               => None
  }

  //  @see UnanchoredRegex
  protected def runMatcher(m: Matcher) = m.matches()

  /** Return all matches of this regexp in given character sequence as a [[scala.util.matching.Regex.MatchIterator]],
   *  which is a special [[scala.collection.Iterator]] that returns the
   *  matched strings, but can also be converted into a normal iterator
   *  that returns objects of type [[scala.util.matching.Regex.Match]]
   *  that can be queried for data such as the text that precedes the
   *  match, subgroups, etc.
   *
   *  @param source The text to match against.
   *  @return       A [[scala.util.matching.Regex.MatchIterator]] of all matches.
   *  @example      {{{for (words <- """\w+""".r findAllIn "A simple example.") yield words}}}
   */
  def findAllIn(source: CharSequence) = new Regex.MatchIterator(source, this, groupNames)


  /** Return all matches of this regexp in given character sequence as a
   *  [[scala.collection.Iterator]] of [[scala.util.matching.Regex.Match]].
   *
   *  @param source The text to match against.
   *  @return       A [[scala.collection.Iterator]] of [[scala.util.matching.Regex.Match]] for all matches.
   *  @example      {{{for (words <- """\w+""".r findAllMatchIn "A simple example.") yield words.start}}}
   */
  def findAllMatchIn(source: CharSequence): Iterator[Match] = {
    val matchIterator = findAllIn(source)
    new Iterator[Match] {
      def hasNext = matchIterator.hasNext
      def next: Match = {
        matchIterator.next;
        new Match(matchIterator.source, matchIterator.matcher, matchIterator.groupNames).force
      }
    }
  }

  /** Return optionally first matching string of this regexp in given character sequence,
   *  or None if it does not exist.
   *
   *  @param source The text to match against.
   *  @return       An [[scala.Option]] of the first matching string in the text.
   *  @example      {{{"""\w+""".r findFirstIn "A simple example." foreach println // prints "A"}}}
   */
  def findFirstIn(source: CharSequence): Option[String] = {
    val m = pattern.matcher(source)
    if (m.find) Some(m.group) else None
  }

  /** Return optionally first match of this regexp in given character sequence,
   *  or None if it does not exist.
   *
   *  The main difference between this method and `findFirstIn` is that the (optional) return
   *  type for this is [[scala.util.matching.Regex.Match]], through which more
   *  data can be obtained about the match, such as the strings that precede and follow it,
   *  or subgroups.
   *
   *  @param source The text to match against.
   *  @return       A [[scala.Option]] of [[scala.util.matching.Regex.Match]] of the first matching string in the text.
   *  @example      {{{("""[a-z]""".r findFirstMatchIn "A simple example.") map (_.start) // returns Some(2), the index of the first match in the text}}}
   */
  def findFirstMatchIn(source: CharSequence): Option[Match] = {
    val m = pattern.matcher(source)
    if (m.find) Some(new Match(source, m, groupNames)) else None
  }

  /** Return optionally match of this regexp at the beginning of the
   *  given character sequence, or None if regexp matches no prefix
   *  of the character sequence.
   *
   *  The main difference from this method to `findFirstIn` is that this
   *  method will not return any matches that do not begin at the start
   *  of the text being matched against.
   *
   *  @param source The text to match against.
   *  @return       A [[scala.Option]] of the matched prefix.
   *  @example      {{{"""[a-z]""".r findPrefixOf "A simple example." // returns None, since the text does not begin with a lowercase letter}}}
   */
  def findPrefixOf(source: CharSequence): Option[String] = {
    val m = pattern.matcher(source)
    if (m.lookingAt) Some(m.group) else None
  }

  /** Return optionally match of this regexp at the beginning of the
   *  given character sequence, or None if regexp matches no prefix
   *  of the character sequence.
   *
   *  The main difference from this method to `findFirstMatchIn` is that
   *  this method will not return any matches that do not begin at the
   *  start of the text being matched against.
   *
   *  @param source The text to match against.
   *  @return       A [[scala.Option]] of the [[scala.util.matching.Regex.Match]] of the matched string.
   *  @example      {{{"""\w+""".r findPrefixMatchOf "A simple example." map (_.after) // returns Some(" simple example.")}}}
   */
  def findPrefixMatchOf(source: CharSequence): Option[Match] = {
    val m = pattern.matcher(source)
    if (m.lookingAt) Some(new Match(source, m, groupNames)) else None
  }

  /** Replaces all matches by a string.
   *
   *  $replacementString
   *
   *  @param target      The string to match
   *  @param replacement The string that will replace each match
   *  @return            The resulting string
   *  @example           {{{"""\d+""".r replaceAllIn ("July 15", "<NUMBER>") // returns "July <NUMBER>"}}}
   */
  def replaceAllIn(target: CharSequence, replacement: String): String = {
    val m = pattern.matcher(target)
    m.replaceAll(replacement)
  }

  /**
   * Replaces all matches using a replacer function. The replacer function takes a
   * [[scala.util.matching.Regex.Match]] so that extra information can be obtained
   * from the match. For example:
   *
   * {{{
   * import scala.util.matching.Regex
   * val datePattern = new Regex("""(\d\d\d\d)-(\d\d)-(\d\d)""", "year", "month", "day")
   * val text = "From 2011-07-15 to 2011-07-17"
   * val repl = datePattern replaceAllIn (text, m => m.group("month")+"/"+m.group("day"))
   * }}}
   *
   * $replacementString
   *
   * @param target      The string to match.
   * @param replacer    The function which maps a match to another string.
   * @return            The target string after replacements.
   */
  def replaceAllIn(target: CharSequence, replacer: Match => String): String = {
    val it = new Regex.MatchIterator(target, this, groupNames).replacementData
    it foreach (md => it replace replacer(md))
    it.replaced
  }

  /**
   * Replaces some of the matches using a replacer function that returns an [[scala.Option]].
   * The replacer function takes a [[scala.util.matching.Regex.Match]] so that extra
   * information can be btained from the match. For example:
   *
   * {{{
   * import scala.util.matching.Regex._
   *
   * val map = Map("x" -> "a var", "y" -> """some $ and \ signs""")
   * val text = "A text with variables %x, %y and %z."
   * val varPattern = """%(\w+)""".r
   * val mapper = (m: Match) => map get (m group 1) map (quoteReplacement(_))
   * val repl = varPattern replaceSomeIn (text, mapper)
   * }}}
   *
   * $replacementString
   *
   * @param target      The string to match.
   * @param replacer    The function which optionally maps a match to another string.
   * @return            The target string after replacements.
   */
  def replaceSomeIn(target: CharSequence, replacer: Match => Option[String]): String = {
    val it = new Regex.MatchIterator(target, this, groupNames).replacementData
    for (matchdata <- it ; replacement <- replacer(matchdata))
      it replace replacement

    it.replaced
  }

  /** Replaces the first match by a string.
   *
   *  $replacementString
   *
   *  @param target      The string to match
   *  @param replacement The string that will replace the match
   *  @return            The resulting string
   */
  def replaceFirstIn(target: CharSequence, replacement: String): String = {
    val m = pattern.matcher(target)
    m.replaceFirst(replacement)
  }

  /** Splits the provided character sequence around matches of this regexp.
   *
   *  @param toSplit The character sequence to split
   *  @return        The array of strings computed by splitting the
   *                 input around matches of this regexp
   */
  def split(toSplit: CharSequence): Array[String] =
    pattern.split(toSplit)

  /** Create a new Regex with the same pattern, but no requirement that
   *  the entire String matches in extractor patterns.  For instance, the strings
   *  shown below lead to successful matches, where they would not otherwise.
   *
   *  {{{
   *  val dateP1 = """(\d\d\d\d)-(\d\d)-(\d\d)""".r.unanchored
   *
   *  val dateP1(year, month, day) = "Date 2011-07-15"
   *
   *  val copyright: String = "Date of this document: 2011-07-15" match {
   *    case dateP1(year, month, day) => "Copyright "+year
   *    case _                        => "No copyright"
   *  }
   *  }}}
   *
   *  @return        The new unanchored regex
   */
  def unanchored: UnanchoredRegex = new Regex(pattern, groupNames: _*) with UnanchoredRegex { override def anchored = outer }
  def anchored: Regex             = this

  def regex: String = pattern.pattern

  /** The string defining the regular expression */
  override def toString = regex
}

trait UnanchoredRegex extends Regex {
  override protected def runMatcher(m: Matcher) = m.find()
  override def unanchored = this
}

/** This object defines inner classes that describe
 *  regex matches and helper objects. The class hierarchy
 *  is as follows:
 *
 *  {{{
 *            MatchData
 *            /      \
 *   MatchIterator  Match
 *  }}}
 *
 */
object Regex {

  /** This class provides methods to access
   *  the details of a match.
   */
  trait MatchData {

    /** The source from where the match originated */
    val source: CharSequence

    /** The names of the groups, or some empty sequence if one defined */
    val groupNames: Seq[String]

    /** The number of subgroups in the pattern (not all of these need to match!) */
    def groupCount: Int

    /** The index of the first matched character, or -1 if nothing was matched */
    def start: Int

    /** The index of the first matched character in group `i`,
     *  or -1 if nothing was matched for that group */
    def start(i: Int): Int

    /** The index of the last matched character, or -1 if nothing was matched */
    def end: Int

    /** The index following the last matched character in group `i`,
     *  or -1 if nothing was matched for that group */
    def end(i: Int): Int

    /** The matched string, or `null` if nothing was matched */
    def matched: String =
      if (start >= 0) source.subSequence(start, end).toString
      else null

    /** The matched string in group `i`,
     *  or `null` if nothing was matched */
    def group(i: Int): String =
      if (start(i) >= 0) source.subSequence(start(i), end(i)).toString
      else null

    /** All matched subgroups, i.e. not including group(0) */
    def subgroups: List[String] = (1 to groupCount).toList map group

    /** The char sequence before first character of match,
     *  or `null` if nothing was matched */
    def before: CharSequence =
      if (start >= 0) source.subSequence(0, start)
      else null

    /** The char sequence before first character of match in group `i`,
     *  or `null` if nothing was matched for that group  */
    def before(i: Int): CharSequence =
      if (start(i) >= 0) source.subSequence(0, start(i))
      else null

    /** Returns char sequence after last character of match,
     *  or `null` if nothing was matched */
    def after: CharSequence =
      if (end >= 0) source.subSequence(end, source.length)
      else null

    /** The char sequence after last character of match in group `i`,
     *  or `null` if nothing was matched for that group  */
    def after(i: Int): CharSequence =
      if (end(i) >= 0) source.subSequence(end(i), source.length)
      else null

    private lazy val nameToIndex: Map[String, Int] = Map[String, Int]() ++ ("" :: groupNames.toList).zipWithIndex

    /** Returns the group with given name
     *
     *  @param id The group name
     *  @return   The requested group
     *  @throws   NoSuchElementException if the requested group name is not defined
     */
    def group(id: String): String = nameToIndex.get(id) match {
      case None => throw new NoSuchElementException("group name "+id+" not defined")
      case Some(index) => group(index)
    }

    /** The matched string; equivalent to `matched.toString` */
    override def toString = matched

  }

  /** Provides information about a succesful match.
   */
  class Match(val source: CharSequence,
              private[matching] val matcher: Matcher,
              val groupNames: Seq[String]) extends MatchData {

    /** The index of the first matched character */
    val start = matcher.start

    /** The index following the last matched character */
    val end = matcher.end

    /** The number of subgroups */
    def groupCount = matcher.groupCount

    private lazy val starts: Array[Int] =
      ((0 to groupCount) map matcher.start).toArray
    private lazy val ends: Array[Int] =
      ((0 to groupCount) map matcher.end).toArray

    /** The index of the first matched character in group `i` */
    def start(i: Int) = starts(i)

    /** The index following the last matched character in group `i` */
    def end(i: Int) = ends(i)

    /** The match itself with matcher-dependent lazy vals forced,
     *  so that match is valid even once matcher is advanced
     */
    def force: this.type = { starts; ends; this }
  }

  /** An extractor object for Matches, yielding the matched string
   *
   *  This can be used to help writing replacer functions when you
   *  are not interested in match data. For example:
   *
   *  {{{
   *  import scala.util.matching.Regex.Match
   *  """\w+""".r replaceAllIn ("A simple example.", _ match { case Match(s) => s.toUpperCase })
   *  }}}
   *
   */
  object Match {
    def unapply(m: Match): Some[String] = Some(m.matched)
  }

  /** An extractor object that yields the groups in the match. Using an extractor
   *  rather than the original regex avoids recomputing the match.
   *
   *  {{{
   *  import scala.util.matching.Regex.Groups
   *
   *  val datePattern = """(\d\d\d\d)-(\d\d)-(\d\d)""".r
   *  val text = "The doc spree happened on 2011-07-15."
   *  val day = datePattern replaceAllIn(text, _ match { case Groups(year, month, day) => month+"/"+day })
   *  }}}
   */
  object Groups {
    def unapplySeq(m: Match): Option[Seq[String]] = if (m.groupCount > 0) Some(1 to m.groupCount map m.group) else None
  }

  /** A class to step through a sequence of regex matches
   */
  class MatchIterator(val source: CharSequence, val regex: Regex, val groupNames: Seq[String])
  extends AbstractIterator[String] with Iterator[String] with MatchData { self =>

    protected[Regex] val matcher = regex.pattern.matcher(source)
    private var nextSeen = false

    /** Is there another match? */
    def hasNext: Boolean = {
      if (!nextSeen) nextSeen = matcher.find()
      nextSeen
    }

    /** The next matched substring of `source` */
    def next(): String = {
      if (!hasNext) throw new NoSuchElementException
      nextSeen = false
      matcher.group
    }

    override def toString = super[AbstractIterator].toString

    /** The index of the first matched character */
    def start: Int = matcher.start

    /** The index of the first matched character in group `i` */
    def start(i: Int): Int = matcher.start(i)

    /** The index of the last matched character */
    def end: Int = matcher.end

    /** The index following the last matched character in group `i` */
    def end(i: Int): Int = matcher.end(i)

    /** The number of subgroups */
    def groupCount = matcher.groupCount

    /** Convert to an iterator that yields MatchData elements instead of Strings */
    def matchData: Iterator[Match] = new AbstractIterator[Match] {
      def hasNext = self.hasNext
      def next = { self.next; new Match(source, matcher, groupNames).force }
    }

    /** Convert to an iterator that yields MatchData elements instead of Strings and has replacement support */
    private[matching] def replacementData = new AbstractIterator[Match] with Replacement {
      def matcher = self.matcher
      def hasNext = self.hasNext
      def next = { self.next; new Match(source, matcher, groupNames).force }
    }
  }

  /**
   * A trait able to build a string with replacements assuming it has a matcher.
   * Meant to be mixed in with iterators.
   */
  private[matching] trait Replacement {
    protected def matcher: Matcher

    private var sb = new java.lang.StringBuffer

    def replaced = {
      val newsb = new java.lang.StringBuffer(sb)
      matcher.appendTail(newsb)
      newsb.toString
    }

    def replace(rs: String) = matcher.appendReplacement(sb, rs)
  }

  /** Quotes replacement strings to be used in replacement methods.
   *
   *  Replacement methods give special meaning to backslashes (`\`) and
   *  dollar signs (`$`) in replacement strings, so they are not treated
   *  as literals. This method escapes these characters so the resulting
   *  string can be used as a literal replacement representing the input
   *  string.
   *
   *  @param text The string one wishes to use as literal replacement.
   *  @return A string that can be used to replace matches with `text`.
   *  @example {{{"CURRENCY".r.replaceAllIn(input, Regex quoteReplacement "US$")}}}
   */
  def quoteReplacement(text: String): String = Matcher quoteReplacement text
}
