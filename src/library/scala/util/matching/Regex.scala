package scala.util.matching

import java.util.regex.{Pattern, Matcher}

/** This class provides methods for creating and using regular expressions.
 *  It is based on the regular expressions of the JDK since 1.4.
 *
 *  @author  Thibaud Hottelier
 *  @author  Philipp Haller
 *  @version 1.1, 29/01/2008
 *
 *  @param regex      A string representing a regular expression
 *  @param groupNames A mapping from names to indices in capture groups
 */
class Regex(regex: String, private var groupNames: Map[String, Int]) {
  private def buildMap(res: Map[String, Int], groupNames: Seq[String], i: Int): Map[String, Int] =
    if (i > groupNames.size)
      res
    else {
      val p = (groupNames(i - 1), i)
      buildMap(res + p, groupNames, i + 1)
    }

  /** Create a <code>Regex</code> from a string.
   *
   *  @param s The regular expression
   *  @return  A new <code>Regex</code> instance
   */
  def this(s: String) = this(s, null: Map[String, Int])

  /** Create a <code>Regex</code> from a string and
   *  a sequence of group names.
   *
   *  @param s      The regular expression
   *  @param groups The list of group names in the same order
   *                as the capture groups in the regular expression
   *  @return       A new <code>Regex</code> instance
   */
  def this(s: String, groups: String*) = {
    this(s)
    groupNames = buildMap(Map[String, Int](), groups, 1)
  }

  /* Store the compiled pattern at instantiation time */
  private val pattern = Pattern.compile(regex)

  /* Builds a MatchData[String] from a Matcher */
  private def buildSeq(l: List[String], m: Matcher, i: Int): MatchData[String] =
    if (i == -1)
      new MatchData(l, groupNames)
    else
      buildSeq(m.group(i) :: l, m, i - 1)

  /* Builds a List[MatchData[String]] from a Matcher */
  private def doMatchAll(res: List[MatchData[String]], m: Matcher): List[MatchData[String]] =
    if (m.find())
      doMatchAll(res ::: List(buildSeq(Nil, m, m.groupCount())), m)
    else
      res

  /* Builds a List[String] from a Matcher */
  private def unapplySeq0(target: String): Option[List[String]] = {
    val m = pattern.matcher(target)
    if (m.matches())
      Some(buildSeq(Nil, m, m.groupCount()).getGroups.tail)
    else
      None
  }

  /** Tries to match target (whole match) and returns
   *  the matches.
   *
   *  @param target The string to match
   *  @return       The matches
   */
  def unapplySeq(target: Any): Option[List[String]] =
    if (target.isInstanceOf[String])
      unapplySeq0(target.asInstanceOf[String])
    else if (target.isInstanceOf[MatchData[_]])
      unapplySeq0(target.asInstanceOf[MatchData[String]]())
    else
      None

  /** Creates a <code>MatchData[String]</code> from a string.
   *  This is used in for-comprehensions to iterate over matches.
   *
   *  @param s The string to match
   *  @return  The MatchData[String] instance
   */
  def ~~(s: String) = matchAll(s)

  /** Returns all matches of target string.
   *
   *  @param target The string to match
   *  @return       All matches in a list of <code>MatchData[String]</code>
   */
  def matchAll(target: String): List[MatchData[String]] = {
    val m = pattern.matcher(target)
    doMatchAll(Nil, m)
  }

  /** Returns the first match of target string.
   *
   *  @param target The string to match
   *  @return       The first match as <code>MatchData[String]</code>
   */
  def matchFirst(target: String): Option[MatchData[String]] = {
    val m = pattern.matcher(target)
    if (!m.find())
      None
    else
      Some(buildSeq(Nil, m, m.groupCount()))
  }

  /** Replaces all matches by a string.
   *
   *  @param target      The string to match
   *  @param replacement The string that will replace each match
   *  @return            The resulting string
   */
  def replaceAll(target: String, replacement: String): String = {
    val m = pattern.matcher(target)
    m.replaceAll(replacement)
  }

  /** Replaces the first match by a string.
   *
   *  @param target      The string to match
   *  @param replacement The string that will replace the match
   *  @return            The resulting string
   */
  def replaceFirst(target: String, replacement: String): String = {
    val m = pattern.matcher(target)
    m.replaceFirst(replacement)
  }

  /** Returns true if the whole string matches.
   *
   *  @param target The string to match
   *  @return       <code>true</code> iff the whole string matches
   */
  def matchesAll(target: String): Boolean = {
    val m = pattern.matcher(target)
    m.find()
  }

  /** Returns <code>true</code> iff the string or one of its substrings match.
   *
   *  @param target The string to match
   *  @return       <code>true</code> iff the string matches
   */
  def matches(target: String): Boolean = {
    val m = pattern.matcher(target)
    m.matches()
  }
}

/** Provides implicit conversions for regular expressions.
 */
object Regex {
  /** Promotes a string to <code>MatchableString</code> so that <code>=~</code> can be used */
  implicit def stringToMatchableString(s: String) = new MatchableString(s)

  /** Allows treating an <code>Option</code> as a Boolean. */
  implicit def optionToBoolean(o: Option[MatchData[String]]) = o.isDefined

  /** Promotes a string to a <code>Regex</code>. */
  implicit def stringToRegex(s: String) = new Regex(s)
}
