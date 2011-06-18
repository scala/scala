/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.util.matching

import java.util.regex.{ Pattern, Matcher }

/** This class provides methods for creating and using regular expressions.
 *  It is based on the regular expressions of the JDK since 1.4.
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
 *  @param regex      A string representing a regular expression
 *  @param groupNames A mapping from names to indices in capture groups
 */
@SerialVersionUID(-2094783597747625537L)
class Regex(regex: String, groupNames: String*) extends Serializable {

  import Regex._

  /** The compiled pattern */
  val pattern = Pattern.compile(regex)

  /** Tries to match target (whole match) and returns the matches.
   *
   *  @param target The string to match
   *  @return       The matches
   */
  def unapplySeq(target: Any): Option[List[String]] = target match {
    case s: java.lang.CharSequence =>
      val m = pattern.matcher(s)
      if (m.matches) Some((1 to m.groupCount).toList map m.group)
      else None
    case Match(s) =>
      unapplySeq(s)
    case _ =>
      None
  }

  /** Return all matches of this regexp in given character sequence as an iterator
   */
  def findAllIn(source: java.lang.CharSequence) = new Regex.MatchIterator(source, this, groupNames)

  /** Return optionally first matching string of this regexp in given character sequence,
   *  None if it does not exist.
   */
  def findFirstIn(source: java.lang.CharSequence): Option[String] = {
    val m = pattern.matcher(source)
    if (m.find) Some(m.group) else None
  }

  /** Return optionally first match of this regexp in given character sequence,
   *  None if it does not exist.
   */
  def findFirstMatchIn(source: java.lang.CharSequence): Option[Match] = {
    val m = pattern.matcher(source)
    if (m.find) Some(new Match(source, m, groupNames)) else None
  }

  /** Return optionally match of this regexp at the beginning of the
   *  given character sequence, or None if regexp matches no prefix
   *  of the character sequence.
   */
  def findPrefixOf(source: java.lang.CharSequence): Option[String] = {
    val m = pattern.matcher(source)
    if (m.lookingAt) Some(m.group) else None
  }

  /** Return optionally match of this regexp at the beginning of the
   *  given character sequence, or None if regexp matches no prefix
   *  of the character sequence.
   */
  def findPrefixMatchOf(source: java.lang.CharSequence): Option[Match] = {
    val m = pattern.matcher(source)
    if (m.lookingAt) Some(new Match(source, m, groupNames)) else None
  }

  /** Replaces all matches by a string.
   *
   *  @param target      The string to match
   *  @param replacement The string that will replace each match
   *  @return            The resulting string
   */
  def replaceAllIn(target: java.lang.CharSequence, replacement: String): String = {
    val m = pattern.matcher(target)
    m.replaceAll(replacement)
  }

  /**
   * Replaces all matches using a replacer function.
   *
   * @param target      The string to match.
   * @param replacer    The function which maps a match to another string.
   * @return            The target string after replacements.
   */
  def replaceAllIn(target: java.lang.CharSequence, replacer: Match => String): String = {
    val it = new Regex.MatchIterator(target, this, groupNames).replacementData
    it foreach (md => it replace replacer(md))
    it.replaced
  }

  def replaceSomeIn(target: java.lang.CharSequence, replacer: Match => Option[String]): String = {
    val it = new Regex.MatchIterator(target, this, groupNames).replacementData
    for (matchdata <- it ; replacement <- replacer(matchdata))
      it replace replacement

    it.replaced
  }

  /** Replaces the first match by a string.
   *
   *  @param target      The string to match
   *  @param replacement The string that will replace the match
   *  @return            The resulting string
   */
  def replaceFirstIn(target: java.lang.CharSequence, replacement: String): String = {
    val m = pattern.matcher(target)
    m.replaceFirst(replacement)
  }

  /** Splits the provided character sequence around matches of this regexp.
   *
   *  @param toSplit The character sequence to split
   *  @return        The array of strings computed by splitting the
   *                 input around matches of this regexp
   */
  def split(toSplit: java.lang.CharSequence): Array[String] =
    pattern.split(toSplit)

  /** The string defining the regular expression */
  override def toString = regex
}

/** This object defines inner classes that describe
 *  regex matches. The class hierarchy is as follows:
 *
 *  {{{
 *            MatchData
 *            /      \
 *   MatchIterator  Match
 *  }}}
 */
object Regex {

  /** This class provides methods to access
   *  the details of a match.
   */
  trait MatchData {

    /** The source from where the match originated */
    val source: java.lang.CharSequence

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
    def before: java.lang.CharSequence =
      if (start >= 0) source.subSequence(0, start)
      else null

    /** The char sequence before first character of match in group `i`,
     *  or `null` if nothing was matched for that group  */
    def before(i: Int): java.lang.CharSequence =
      if (start(i) >= 0) source.subSequence(0, start(i))
      else null

    /** Returns char sequence after last character of match,
     *  or `null` if nothing was matched */
    def after: java.lang.CharSequence =
      if (end >= 0) source.subSequence(end, source.length)
      else null

    /** The char sequence after last character of match in group `i`,
     *  or `null` if nothing was matched for that group  */
    def after(i: Int): java.lang.CharSequence =
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

  /** A case class for a successful match.
   */
  class Match(val source: java.lang.CharSequence,
              matcher: Matcher,
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

  /** An extractor object for Matches, yielding the matched string */
  object Match {
    def unapply(m: Match): Some[String] = Some(m.matched)
  }

  /** An extractor object that yields groups in the match. */
  object Groups {
    def unapplySeq(m: Match): Option[Seq[String]] = if (m.groupCount > 0) Some(1 to m.groupCount map m.group) else None
  }

  /** A class to step through a sequence of regex matches
   */
  class MatchIterator(val source: java.lang.CharSequence, val regex: Regex, val groupNames: Seq[String])
  extends Iterator[String] with MatchData { self =>

    protected val matcher = regex.pattern.matcher(source)
    private var nextSeen = false

    /** Is there another match? */
    def hasNext: Boolean = {
      if (!nextSeen) nextSeen = matcher.find()
      nextSeen
    }

    /** The next matched substring of `source` */
    def next: String = {
      if (!hasNext) throw new NoSuchElementException
      nextSeen = false
      matcher.group
    }

    override def toString = super[Iterator].toString

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
    def matchData = new Iterator[Match] {
      def hasNext = self.hasNext
      def next = { self.next; new Match(source, matcher, groupNames).force }
    }

    /** Convert to an iterator that yields MatchData elements instead of Strings and has replacement support */
    private[matching] def replacementData = new Iterator[Match] with Replacement {
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
}
