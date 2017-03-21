/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

/**
 * This package is concerned with regular expression (regex) matching against strings,
 * with the main goal of pulling out information from those matches, or replacing
 * them with something else.
 *
 * [[scala.util.matching.Regex]] is the class users instantiate to do regular expression matching.
 *
 * The companion object to [[scala.util.matching.Regex]] contains supporting members:
 * * [[scala.util.matching.Regex.Match]] makes more information about a match available.
 * * [[scala.util.matching.Regex.MatchIterator]] is used to iterate over matched strings.
 * * [[scala.util.matching.Regex.MatchData]] is just a base trait for the above classes.
 * * [[scala.util.matching.Regex.Groups]] extracts group from a [[scala.util.matching.Regex.Match]]
 *   without recomputing the match.
 */
package scala.util.matching

import scala.collection.AbstractIterator
import java.util.regex.{ Pattern, Matcher }

/** A regular expression is used to determine whether a string matches a pattern
 *  and, if it does, to extract or transform the parts that match.
 *
 *  === Usage ===
 *  This class delegates to the [[java.util.regex]] package of the Java Platform.
 *  See the documentation for [[java.util.regex.Pattern]] for details about
 *  the regular expression syntax for pattern strings.
 *
 *  An instance of `Regex` represents a compiled regular expression pattern.
 *  Since compilation is expensive, frequently used `Regex`es should be constructed
 *  once, outside of loops and perhaps in a companion object.
 *
 *  The canonical way to create a `Regex` is by using the method `r`, provided
 *  implicitly for strings:
 *
 *  {{{
 *  val date = raw"(\d{4})-(\d{2})-(\d{2})".r
 *  }}}
 *
 *  Since escapes are not processed in multi-line string literals, using triple quotes
 *  avoids having to escape the backslash character, so that `"\\d"` can be written `"""\d"""`.
 *  The same result is achieved with certain interpolators, such as `raw"\d".r` or
 *  a custom interpolator `r"\d"` that also compiles the `Regex`.
 *
 *  === Extraction ===
 *  To extract the capturing groups when a `Regex` is matched, use it as
 *  an extractor in a pattern match:
 *
 *  {{{
 *  "2004-01-20" match {
 *    case date(year, month, day) => s"$year was a good year for PLs."
 *  }
 *  }}}
 *
 *  To check only whether the `Regex` matches, ignoring any groups,
 *  use a sequence wildcard:
 *
 *  {{{
 *  "2004-01-20" match {
 *    case date(_*) => "It's a date!"
 *  }
 *  }}}
 *
 *  That works because a `Regex` extractor produces a sequence of strings.
 *  Extracting only the year from a date could also be expressed with
 *  a sequence wildcard:
 *
 *  {{{
 *  "2004-01-20" match {
 *    case date(year, _*) => s"$year was a good year for PLs."
 *  }
 *  }}}
 *
 *  In a pattern match, `Regex` normally matches the entire input.
 *  However, an unanchored `Regex` finds the pattern anywhere
 *  in the input.
 *
 *  {{{
 *  val embeddedDate = date.unanchored
 *  "Date: 2004-01-20 17:25:18 GMT (10 years, 28 weeks, 5 days, 17 hours and 51 minutes ago)" match {
 *    case embeddedDate("2004", "01", "20") => "A Scala is born."
 *  }
 *  }}}
 *
 *  === Find Matches ===
 *  To find or replace matches of the pattern, use the various find and replace methods.
 *  For each method, there is a version for working with matched strings and
 *  another for working with `Match` objects.
 *
 *  For example, pattern matching with an unanchored `Regex`, as in the previous example,
 *  can also be accomplished using `findFirstMatchIn`. The `findFirst` methods return an `Option`
 *  which is non-empty if a match is found, or `None` for no match:
 *
 *  {{{
 *  val dates = "Important dates in history: 2004-01-20, 1958-09-05, 2010-10-06, 2011-07-15"
 *  val firstDate = date.findFirstIn(dates).getOrElse("No date found.")
 *  val firstYear = for (m <- date.findFirstMatchIn(dates)) yield m.group(1)
 *  }}}
 *
 *  To find all matches:
 *
 *  {{{
 *  val allYears = for (m <- date.findAllMatchIn(dates)) yield m.group(1)
 *  }}}
 *
 *  To iterate over the matched strings, use `findAllIn`, which returns a special iterator
 *  that can be queried for the `MatchData` of the last match:
 *
 *  {{{
 *  val mi = date.findAllIn(dates)
 *  while (mi.hasNext) {
 *    val d = mi.next
 *    if (mi.group(1).toInt < 1960) println(s"$d: An oldie but goodie.")
 *  }
 *  }}}
 *
 *  Although the `MatchIterator` returned by `findAllIn` is used like any `Iterator`,
 *  with alternating calls to `hasNext` and `next`, `hasNext` has the additional
 *  side effect of advancing the underlying matcher to the next unconsumed match.
 *  This effect is visible in the `MatchData` representing the "current match".
 *
 *  {{{
 *  val r = "(ab+c)".r
 *  val s = "xxxabcyyyabbczzz"
 *  r.findAllIn(s).start    // 3
 *  val mi = r.findAllIn(s)
 *  mi.hasNext              // true
 *  mi.start                // 3
 *  mi.next()               // "abc"
 *  mi.start                // 3
 *  mi.hasNext              // true
 *  mi.start                // 9
 *  mi.next()               // "abbc"
 *  }}}
 *
 *  The example shows that methods on `MatchData` such as `start` will advance to
 *  the first match, if necessary. It also shows that `hasNext` will advance to
 *  the next unconsumed match, if `next` has already returned the current match.
 *
 *  The current `MatchData` can be captured using the `matchData` method.
 *  Alternatively, `findAllMatchIn` returns an `Iterator[Match]`, where there
 *  is no interaction between the iterator and `Match` objects it has already produced.
 *
 *  Note that `findAllIn` finds matches that don't overlap. (See [[findAllIn]] for more examples.)
 *
 *  {{{
 *  val num = raw"(\d+)".r
 *  val all = num.findAllIn("123").toList  // List("123"), not List("123", "23", "3")
 *  }}}
 *
 *  === Replace Text ===
 *  Text replacement can be performed unconditionally or as a function of the current match:
 *
 *  {{{
 *  val redacted    = date.replaceAllIn(dates, "XXXX-XX-XX")
 *  val yearsOnly   = date.replaceAllIn(dates, m => m.group(1))
 *  val months      = (0 to 11).map { i => val c = Calendar.getInstance; c.set(2014, i, 1); f"$c%tb" }
 *  val reformatted = date.replaceAllIn(dates, _ match { case date(y,m,d) => f"${months(m.toInt - 1)} $d, $y" })
 *  }}}
 *
 *  Pattern matching the `Match` against the `Regex` that created it does not reapply the `Regex`.
 *  In the expression for `reformatted`, each `date` match is computed once. But it is possible to apply a
 *  `Regex` to a `Match` resulting from a different pattern:
 *
 *  {{{
 *  val docSpree = """2011(?:-\d{2}){2}""".r
 *  val docView  = date.replaceAllIn(dates, _ match {
 *    case docSpree() => "Historic doc spree!"
 *    case _          => "Something else happened"
 *  })
 *  }}}
 *
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
 *  will be interpreted as an escape character and can be used to escape the
 *  dollar sign. Use `Regex.quoteReplacement` to escape these characters.
 */
@SerialVersionUID(-2094783597747625537L)
class Regex private[matching](val pattern: Pattern, groupNames: String*) extends Serializable {
  outer =>

  import Regex._

  /** Compile a regular expression, supplied as a string, into a pattern that
   *  can be matched against inputs.
   *
   *  If group names are supplied, they can be used this way:
   *
   *  {{{
   *  val namedDate  = new Regex("""(\d\d\d\d)-(\d\d)-(\d\d)""", "year", "month", "day")
   *  val namedYears = for (m <- namedDate findAllMatchIn dates) yield m group "year"
   *  }}}
   *
   *  Group names supplied to the constructor are preferred to inline group names
   *  when retrieving matched groups by name. Not all platforms support inline names.
   *
   *  This constructor does not support options as flags, which must be
   *  supplied as inline flags in the pattern string: `(?idmsux-idmsux)`.
   *
   *  @param regex      The regular expression to compile.
   *  @param groupNames Names of capturing groups.
   */
  def this(regex: String, groupNames: String*) = this(Pattern.compile(regex), groupNames: _*)

  /** Tries to match a [[java.lang.CharSequence]].
   *
   *  If the match succeeds, the result is a list of the matching
   *  groups (or a `null` element if a group did not match any input).
   *  If the pattern specifies no groups, then the result will be an empty list
   *  on a successful match.
   *
   *  This method attempts to match the entire input by default; to find the next
   *  matching subsequence, use an unanchored `Regex`.
   *
   *  For example:
   *
   *  {{{
   *  val p1 = "ab*c".r
   *  val p1Matches = "abbbc" match {
   *    case p1() => true               // no groups
   *    case _    => false
   *  }
   *  val p2 = "a(b*)c".r
   *  val p2Matches = "abbbc" match {
   *    case p2(_*) => true             // any groups
   *    case _      => false
   *  }
   *  val numberOfB = "abbbc" match {
   *    case p2(b) => Some(b.length)    // one group
   *    case _     => None
   *  }
   *  val p3 = "b*".r.unanchored
   *  val p3Matches = "abbbc" match {
   *    case p3() => true               // find the b's
   *    case _    => false
   *  }
   *  val p4 = "a(b*)(c+)".r
   *  val p4Matches = "abbbcc" match {
   *    case p4(_*) => true             // multiple groups
   *    case _      => false
   *  }
   *  val allGroups = "abbbcc" match {
   *    case p4(all @ _*) => all mkString "/" // "bbb/cc"
   *    case _            => ""
   *  }
   *  val cGroup = "abbbcc" match {
   *    case p4(_, c) => c
   *    case _        => ""
   *  }
   *  }}}
   *
   *  @param  s     The string to match
   *  @return       The matches
   */
  def unapplySeq(s: CharSequence): Option[List[String]] = s match {
    case null => None
    case _    =>
      val m = pattern matcher s
      if (runMatcher(m)) Some((1 to m.groupCount).toList map m.group)
      else None
  }

  /** Tries to match the String representation of a [[scala.Char]].
   *
   *  If the match succeeds, the result is the first matching
   *  group if any groups are defined, or an empty Sequence otherwise.
   *
   *  For example:
   *
   *  {{{
   *  val cat = "cat"
   *  // the case must consume the group to match
   *  val r = """(\p{Lower})""".r
   *  cat(0) match { case r(x) => true }
   *  cat(0) match { case r(_) => true }
   *  cat(0) match { case r(_*) => true }
   *  cat(0) match { case r() => true }     // no match
   *
   *  // there is no group to extract
   *  val r = """\p{Lower}""".r
   *  cat(0) match { case r(x) => true }    // no match
   *  cat(0) match { case r(_) => true }    // no match
   *  cat(0) match { case r(_*) => true }   // matches
   *  cat(0) match { case r() => true }     // matches
   *
   *  // even if there are multiple groups, only one is returned
   *  val r = """((.))""".r
   *  cat(0) match { case r(_) => true }    // matches
   *  cat(0) match { case r(_,_) => true }  // no match
   *  }}}
   *
   *  @param  c     The Char to match
   *  @return       The match
   */
  def unapplySeq(c: Char): Option[List[Char]] = {
    val m = pattern matcher c.toString
    if (runMatcher(m)) {
      if (m.groupCount > 0) Some((m group 1).toList) else Some(Nil)
    } else None
  }

  /** Tries to match on a [[scala.util.matching.Regex.Match]].
   *
   *  A previously failed match results in None.
   *
   *  If a successful match was made against the current pattern, then that result is used.
   *
   *  Otherwise, this Regex is applied to the previously matched input,
   *  and the result of that match is used.
   */
  def unapplySeq(m: Match): Option[List[String]] =
    if (m == null || m.matched == null) None
    else if (m.matcher.pattern == this.pattern) Some((1 to m.groupCount).toList map m.group)
    else unapplySeq(m.matched)

  /** Tries to match target.
   *  @param target The string to match
   *  @return       The matches
   */
  @deprecated("extracting a match result from anything but a CharSequence or Match is deprecated", "2.11.0")
  def unapplySeq(target: Any): Option[List[String]] = target match {
    case s: CharSequence =>
      val m = pattern matcher s
      if (runMatcher(m)) Some((1 to m.groupCount).toList map m.group)
      else None
    case m: Match => unapplySeq(m.matched)
    case _ => None
  }

  //  @see UnanchoredRegex
  protected def runMatcher(m: Matcher) = m.matches()

  /** Return all non-overlapping matches of this `Regex` in the given character
   *  sequence as a [[scala.util.matching.Regex.MatchIterator]],
   *  which is a special [[scala.collection.Iterator]] that returns the
   *  matched strings but can also be queried for more data about the last match,
   *  such as capturing groups and start position.
   *
   *  A `MatchIterator` can also be converted into an iterator
   *  that returns objects of type [[scala.util.matching.Regex.Match]],
   *  such as is normally returned by `findAllMatchIn`.
   *
   *  Where potential matches overlap, the first possible match is returned,
   *  followed by the next match that follows the input consumed by the
   *  first match:
   *
   *  {{{
   *  val hat  = "hat[^a]+".r
   *  val hathaway = "hathatthattthatttt"
   *  val hats = hat.findAllIn(hathaway).toList                     // List(hath, hattth)
   *  val pos  = hat.findAllMatchIn(hathaway).map(_.start).toList   // List(0, 7)
   *  }}}
   *
   *  To return overlapping matches, it is possible to formulate a regular expression
   *  with lookahead (`?=`) that does not consume the overlapping region.
   *
   *  {{{
   *  val madhatter = "(h)(?=(at[^a]+))".r
   *  val madhats   = madhatter.findAllMatchIn(hathaway).map {
   *    case madhatter(x,y) => s"$x$y"
   *  }.toList                                       // List(hath, hatth, hattth, hatttt)
   *  }}}
   *
   *  Attempting to retrieve match information after exhausting the iterator
   *  results in [[java.lang.IllegalStateException]].
   *  See [[scala.util.matching.Regex.MatchIterator]] for details.
   *
   *  @param source The text to match against.
   *  @return       A [[scala.util.matching.Regex.MatchIterator]] of matched substrings.
   *  @example      {{{for (words <- """\w+""".r findAllIn "A simple example.") yield words}}}
   */
  def findAllIn(source: CharSequence) = new Regex.MatchIterator(source, this, groupNames)

  /** Return all non-overlapping matches of this regexp in given character sequence as a
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
        matchIterator.next()
        new Match(matchIterator.source, matchIterator.matcher, matchIterator.groupNames).force
      }
    }
  }

  /** Return an optional first matching string of this `Regex` in the given character sequence,
   *  or None if there is no match.
   *
   *  @param source The text to match against.
   *  @return       An [[scala.Option]] of the first matching string in the text.
   *  @example      {{{"""\w+""".r findFirstIn "A simple example." foreach println // prints "A"}}}
   */
  def findFirstIn(source: CharSequence): Option[String] = {
    val m = pattern.matcher(source)
    if (m.find) Some(m.group) else None
  }

  /** Return an optional first match of this `Regex` in the given character sequence,
   *  or None if it does not exist.
   *
   *  If the match is successful, the [[scala.util.matching.Regex.Match]] can be queried for
   *  more data.
   *
   *  @param source The text to match against.
   *  @return       A [[scala.Option]] of [[scala.util.matching.Regex.Match]] of the first matching string in the text.
   *  @example      {{{("""[a-z]""".r findFirstMatchIn "A simple example.") map (_.start) // returns Some(2), the index of the first match in the text}}}
   */
  def findFirstMatchIn(source: CharSequence): Option[Match] = {
    val m = pattern.matcher(source)
    if (m.find) Some(new Match(source, m, groupNames)) else None
  }

  /** Return an optional match of this `Regex` at the beginning of the
   *  given character sequence, or None if it matches no prefix
   *  of the character sequence.
   *
   *  Unlike `findFirstIn`, this method will only return a match at
   *  the beginning of the input.
   *
   *  @param source The text to match against.
   *  @return       A [[scala.Option]] of the matched prefix.
   *  @example      {{{"""\p{Lower}""".r findPrefixOf "A simple example." // returns None, since the text does not begin with a lowercase letter}}}
   */
  def findPrefixOf(source: CharSequence): Option[String] = {
    val m = pattern.matcher(source)
    if (m.lookingAt) Some(m.group) else None
  }

  /** Return an optional match of this `Regex` at the beginning of the
   *  given character sequence, or None if it matches no prefix
   *  of the character sequence.
   *
   *  Unlike `findFirstMatchIn`, this method will only return a match at
   *  the beginning of the input.
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
   * val repl = datePattern replaceAllIn (text, m => s"${m group "month"}/${m group "day"}")
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
   * information can be obtained from the match. For example:
   *
   * {{{
   * import scala.util.matching.Regex._
   *
   * val vars = Map("x" -> "a var", "y" -> """some $ and \ signs""")
   * val text = "A text with variables %x, %y and %z."
   * val varPattern = """%(\w+)""".r
   * val mapper = (m: Match) => vars get (m group 1) map (quoteReplacement(_))
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
   *  the entire String matches in extractor patterns.
   *
   *  Normally, matching on `date` behaves as though the pattern were
   *  enclosed in anchors, `"^pattern$"`.
   *
   *  The unanchored `Regex` behaves as though those anchors were removed.
   *
   *  Note that this method does not actually strip any matchers from the pattern.
   *
   *  Calling `anchored` returns the original `Regex`.
   *
   *  {{{
   *  val date = """(\d\d\d\d)-(\d\d)-(\d\d)""".r.unanchored
   *
   *  val date(year, month, day) = "Date 2011-07-15"                       // OK
   *
   *  val copyright: String = "Date of this document: 2011-07-15" match {
   *    case date(year, month, day) => s"Copyright $year"                  // OK
   *    case _                      => "No copyright"
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

/** A [[Regex]] that finds the first match when used in a pattern match.
 *
 *  @see [[Regex#unanchored]]
 */
trait UnanchoredRegex extends Regex {
  override protected def runMatcher(m: Matcher) = m.find()
  override def unanchored = this
}

/** This object defines inner classes that describe
 *  regex matches and helper objects.
 */
object Regex {

  /** This class provides methods to access
   *  the details of a match.
   */
  trait MatchData {

    /** Basically, wraps a platform Matcher. */
    protected def matcher: Matcher

    /** The source from which the match originated */
    val source: CharSequence

    /** The names of the groups, or an empty sequence if none defined */
    val groupNames: Seq[String]

    /** The number of capturing groups in the pattern.
     *  (For a given successful match, some of those groups may not have matched any input.)
     */
    def groupCount: Int

    /** The index of the first matched character, or -1 if nothing was matched */
    def start: Int

    /** The index of the first matched character in group `i`,
     *  or -1 if nothing was matched for that group.
     */
    def start(i: Int): Int

    /** The index following the last matched character, or -1 if nothing was matched. */
    def end: Int

    /** The index following the last matched character in group `i`,
     *  or -1 if nothing was matched for that group.
     */
    def end(i: Int): Int

    /** The matched string, or `null` if nothing was matched. */
    def matched: String =
      if (start >= 0) source.subSequence(start, end).toString
      else null

    /** The matched string in group `i`,
     *  or `null` if nothing was matched.
     */
    def group(i: Int): String =
      if (start(i) >= 0) source.subSequence(start(i), end(i)).toString
      else null

    /** All capturing groups, i.e., not including group(0). */
    def subgroups: List[String] = (1 to groupCount).toList map group

    /** The char sequence before first character of match,
     *  or `null` if nothing was matched.
     */
    def before: CharSequence =
      if (start >= 0) source.subSequence(0, start)
      else null

    /** The char sequence before first character of match in group `i`,
     *  or `null` if nothing was matched for that group.
     */
    def before(i: Int): CharSequence =
      if (start(i) >= 0) source.subSequence(0, start(i))
      else null

    /** Returns char sequence after last character of match,
     *  or `null` if nothing was matched.
     */
    def after: CharSequence =
      if (end >= 0) source.subSequence(end, source.length)
      else null

    /** The char sequence after last character of match in group `i`,
     *  or `null` if nothing was matched for that group.
     */
    def after(i: Int): CharSequence =
      if (end(i) >= 0) source.subSequence(end(i), source.length)
      else null

    private lazy val nameToIndex: Map[String, Int] = Map[String, Int]() ++ ("" :: groupNames.toList).zipWithIndex

    /** Returns the group with the given name.
     *
     *  Uses explicit group names when supplied; otherwise,
     *  queries the underlying implementation for inline named groups.
     *  Not all platforms support inline group names.
     *
     *  @param id The group name
     *  @return   The requested group
     *  @throws   IllegalArgumentException if the requested group name is not defined
     */
    def group(id: String): String = (
      if (groupNames.isEmpty)
        matcher group id
      else
        nameToIndex.get(id) match {
          case Some(index) => group(index)
          case None        => matcher group id
        }
    )

    /** The matched string; equivalent to `matched.toString`. */
    override def toString = matched
  }

  /** Provides information about a successful match. */
  class Match(val source: CharSequence,
              protected[matching] val matcher: Matcher,
              val groupNames: Seq[String]) extends MatchData {

    /** The index of the first matched character. */
    val start = matcher.start

    /** The index following the last matched character. */
    val end = matcher.end

    /** The number of subgroups. */
    def groupCount = matcher.groupCount

    private lazy val starts: Array[Int] =
      ((0 to groupCount) map matcher.start).toArray
    private lazy val ends: Array[Int] =
      ((0 to groupCount) map matcher.end).toArray

    /** The index of the first matched character in group `i`. */
    def start(i: Int) = starts(i)

    /** The index following the last matched character in group `i`. */
    def end(i: Int) = ends(i)

    /** The match itself with matcher-dependent lazy vals forced,
     *  so that match is valid even once matcher is advanced.
     */
    def force: this.type = { starts; ends; this }
  }

  /** An extractor object for Matches, yielding the matched string.
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

  /** An extractor object that yields the groups in the match. Using this extractor
   *  rather than the original `Regex` ensures that the match is not recomputed.
   *
   *  {{{
   *  import scala.util.matching.Regex.Groups
   *
   *  val date = """(\d\d\d\d)-(\d\d)-(\d\d)""".r
   *  val text = "The doc spree happened on 2011-07-15."
   *  val day = date replaceAllIn(text, _ match { case Groups(_, month, day) => s"$month/$day" })
   *  }}}
   */
  object Groups {
    def unapplySeq(m: Match): Option[Seq[String]] = if (m.groupCount > 0) Some(1 to m.groupCount map m.group) else None
  }

  /** A class to step through a sequence of regex matches.
   *
   *  This is an iterator that returns the matched strings.
   *
   *  Queries about match data pertain to the current state of the underlying
   *  matcher, which is advanced by calling `hasNext` or `next`.
   *
   *  When matches are exhausted, queries about match data will throw
   *  [[java.lang.IllegalStateException]].
   *
   *  @see [[java.util.regex.Matcher]]
   */
  class MatchIterator(val source: CharSequence, val regex: Regex, val groupNames: Seq[String])
  extends AbstractIterator[String] with Iterator[String] with MatchData { self =>

    protected[Regex] val matcher = regex.pattern.matcher(source)

    // 0 = not yet matched, 1 = matched, 2 = advanced to match, 3 = no more matches
    private[this] var nextSeen = 0

    /** Return true if `next` will find a match.
     *  As a side effect, advance the underlying matcher if necessary;
     *  queries about the current match data pertain to the underlying matcher.
     */
    def hasNext: Boolean = {
      nextSeen match {
        case 0 => nextSeen = if (matcher.find()) 1 else 3
        case 1 => ()
        case 2 => nextSeen = 0 ; hasNext
        case 3 => ()
      }
      nextSeen == 1      // otherwise, 3
    }

    /** The next matched substring of `source`.
     *  As a side effect, advance the underlying matcher if necessary.
     */
    def next(): String = {
      nextSeen match {
        case 0 => if (!hasNext) throw new NoSuchElementException ; next()
        case 1 => nextSeen = 2
        case 2 => nextSeen = 0 ; next()
        case 3 => throw new NoSuchElementException
      }
      matcher.group
    }

    /** Report emptiness. */
    override def toString = super[AbstractIterator].toString

    // ensure we're at a match
    private[this] def ensure(): Unit = nextSeen match {
      case 0 => if (!hasNext) throw new IllegalStateException
      case 1 => ()
      case 2 => ()
      case 3 => throw new IllegalStateException
    }

    /** The index of the first matched character. */
    def start: Int = { ensure() ; matcher.start }

    /** The index of the first matched character in group `i`. */
    def start(i: Int): Int = { ensure() ; matcher.start(i) }

    /** The index of the last matched character. */
    def end: Int = { ensure() ; matcher.end }

    /** The index following the last matched character in group `i`. */
    def end(i: Int): Int = { ensure() ; matcher.end(i) }

    /** The number of subgroups. */
    def groupCount = { ensure() ; matcher.groupCount }

    /** Convert to an iterator that yields MatchData elements instead of Strings. */
    def matchData: Iterator[Match] = new AbstractIterator[Match] {
      def hasNext = self.hasNext
      def next = { self.next(); new Match(source, matcher, groupNames).force }
    }

    /** Convert to an iterator that yields MatchData elements instead of Strings and has replacement support. */
    private[matching] def replacementData = new AbstractIterator[Match] with Replacement {
      def matcher = self.matcher
      def hasNext = self.hasNext
      def next = { self.next(); new Match(source, matcher, groupNames).force }
    }
  }

  /**
   * A trait able to build a string with replacements assuming it has a matcher.
   * Meant to be mixed in with iterators.
   */
  private[matching] trait Replacement {
    protected def matcher: Matcher

    private val sb = new java.lang.StringBuffer

    def replaced = {
      val newsb = new java.lang.StringBuffer(sb)
      matcher.appendTail(newsb)
      newsb.toString
    }

    def replace(rs: String) = matcher.appendReplacement(sb, rs)
  }

  /** Quotes strings to be used literally in regex patterns.
   *
   *  All regex metacharacters in the input match themselves literally in the output.
   *
   *  @example {{{List("US$", "CAN$").map(Regex.quote).mkString("|").r}}}
   */
  def quote(text: String): String = Pattern quote text

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
