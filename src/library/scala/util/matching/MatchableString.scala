/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:$


package scala.util.matching

/** This class provides an implicit conversion that allows
 *  using the matching operator <code>=~</code> on strings.
 *
 *  @author  Thibaud Hottelier
 *  @version 1.0, 15/11/2007
 */
class MatchableString(s: String) {
  /** Returns the first match of this string with the
   *  provided regular expression.
   *
   *  @param regex the regular expression
   *  @return      the first match
   */
  def =~ (regex: Regex): Option[MatchData[String]] = regex.matchFirst(s)
}
