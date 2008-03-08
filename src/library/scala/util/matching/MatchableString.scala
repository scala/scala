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
