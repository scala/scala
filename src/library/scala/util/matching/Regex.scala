/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.matching

import java.util.regex.{Pattern, Matcher}

/** This class provides methods for creating and using regular expressions.
 *  It is based on the regular expressions of the JDK since 1.4.
 *
 *  @author  Thibaud Hottelier
 *  @author  Philipp Haller
 *  @author  Martin Odersky
 *  @version 1.1, 29/01/2008
 *
 *  @param regex      A string representing a regular expression
 *  @param groupNames A mapping from names to indices in capture groups
 */
class Regex(regex: String, groupNames: String*) {

  import Regex._

  /** The compiled pattern */
  val pattern = Pattern.compile(regex)

  /** Tries to match target (whole match) and returns
   *  the matches.
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
  def findAllIn(source: CharSequence) = new Regex.MatchIterator(source, this, groupNames)

  /** Return optionally first matching string of this regexp in given character sequence,
   *  None if it does not exist.
   */
  def findFirstIn(source: CharSequence): Option[String] = {
    val m = pattern.matcher(source)
    if (m.find) Some(m.group) else None
  }

  /** Return optionally first match of this regexp in given character sequence,
   *  None if it does not exist.
   */
  def findFirstMatchIn(source: CharSequence): Option[Match] = {
    val m = pattern.matcher(source)
    if (m.find) Some(new Match(source, m, groupNames)) else None
  }

  /** Return optionally match of this regexp at the beginning of the
   *  given character sequence, or None if regexp matches no prefix
   *  of the character sequence.
   */
  def findPrefixOf(source: CharSequence): Option[String] = {
    val m = pattern.matcher(source)
    if (m.lookingAt) Some(m.group) else None
  }

  /** Return optionally match of this regexp at the beginning of the
   *  given character sequence, or None if regexp matches no prefix
   *  of the character sequence.
   */
  def findPrefixMatchOf(source: CharSequence): Option[Match] = {
    val m = pattern.matcher(source)
    if (m.lookingAt) Some(new Match(source, m, groupNames)) else None
  }

  /** Replaces all matches by a string.
   *
   *  @param target      The string to match
   *  @param replacement The string that will replace each match
   *  @return            The resulting string
   */
  def replaceAllIn(target: CharSequence, replacement: String): String = {
    val m = pattern.matcher(target)
    m.replaceAll(replacement)
  }

  /** Replaces the first match by a string.
   *
   *  @param target      The string to match
   *  @param replacement The string that will replace the match
   *  @return            The resulting string
   */
  def replaceFirstIn(target: CharSequence, replacement: String): String = {
    val m = pattern.matcher(target)
    m.replaceFirst(replacement)
  }

  /** The string defining the regular expression */
  override def toString = regex
}

/** This object defines inner classes that describe
 *  regex matches. The class hirrachy is as follows.
 *
 *            MatchData
 *              |      \
 *      MatchIterator  Match
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

    /** The index of the first matched character */
    def start: Int

    /** The index of the first matched character in group <code>i</code> */
    def start(i: Int): Int

    /** The index of the last matched character */
    def end: Int

    /** The number of subgroups */
    def groupCount: Int

    /** The index following the last matched character in group <code>i</code> */
    def end(i: Int): Int

    /** The matched string */
    def matched: String = source.subSequence(start, end).toString

    /** The matched string in group <code>i</code> */
    def group(i: Int): String = source.subSequence(start(i), end(i)).toString

    /** All matched subgroups, i.e. not including group(0) */
    def subgroups: List[String] = (1 to groupCount).toList map group

    /** The char sequence before first character of match */
    def before: CharSequence = source.subSequence(0, start)

    /** The char sequence before first character of match in group <code>i</code> */
    def before(i: Int): CharSequence = source.subSequence(0, start(i))

    /** Returns char sequence after last character of match */
    def after: CharSequence = source.subSequence(end)

    /** The char sequence after last character of match in group <code>i</code> */
    def after(i: Int): CharSequence = source.subSequence(end(i))

    private lazy val nameToIndex: Map[String, Int] = Map() ++ ("" :: groupNames.toList).zipWithIndex

    /** Returns the group with given name
     *
     *  @param id The group name
     *  @return   The requested group
     *  @throws   <code>NoSuchElementException</code> if the requested
     *            group name is not defined
     */
    def group(id: String): String = nameToIndex.get(id) match {
      case None => throw new NoSuchElementException("group name "+id+" not defined")
      case Some(index) => group(index)
    }

    /** The matched string; equivalent to <code>matched.toString</code> */
    override def toString = matched

  }

  /** A case class for a succesful match.
   */
  class Match(val source: CharSequence,
              matcher: Matcher,
              val groupNames: Seq[String]) extends MatchData {

    /** The index of the first matched character */
    val start = matcher.start

    /** The index following the last matched character */
    val end = matcher.end

    /** The number of subgroups */
    def groupCount = matcher.groupCount

    private lazy val starts: Array[Int] =
      ((1 to groupCount) map matcher.start).toArray
    private lazy val ends: Array[Int] =
      ((1 to groupCount) map matcher.end).toArray

    /** The index of the first matched character in group <code>i</code> */
    def start(i: Int) = starts(i)

    /** The index following the last matched character in group <code>i</code> */
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

  /** A class to step through a sequence of regex matches
   */
  class MatchIterator(val source: CharSequence, val regex: Regex, val groupNames: Seq[String])
  extends Iterator[String] with MatchData { self =>

    private val matcher = regex.pattern.matcher(source)
    private var nextSeen = false

    /** Is there another match? */
    def hasNext: Boolean = {
      if (!nextSeen) nextSeen = matcher.find()
      nextSeen
    }

    /** The next matched substring of `source' */
    def next: String = {
      if (!hasNext) throw new NoSuchElementException
      nextSeen = false
      matcher.group
    }

    override def toString = super[Iterator].toString

    /** The index of the first matched character */
    def start: Int = matcher.start

    /** The index of the first matched character in group <code>i</code> */
    def start(i: Int): Int = matcher.start(i)

    /** The index of the last matched character */
    def end: Int = matcher.end

    /** The index following the last matched character in group <code>i</code> */
    def end(i: Int): Int = matcher.end(i)

    /** The number of subgroups */
    def groupCount = matcher.groupCount

    /** Convert to an iterator that yields MatchData elements instead of Strings */
    def matchData = new Iterator[Match] {
      def hasNext = self.hasNext
      def next = { self.next; new Match(source, matcher, groupNames).force }
    }
  }
}



