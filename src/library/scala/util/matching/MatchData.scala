/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.matching

/** This class provides methods to access
 *  the contents of capture groups.
 *
 *  @author  Thibaud Hottelier
 *  @version 1.1, 29/01/2008
 */
class MatchData[A](groups: List[A], groupNames: Map[String, Int]) {
  def this(groups: List[A]) = this(groups, null)

  /** Returns the i-th group
   *  @param id The group number
   *  @return   The i-th group
   */
  def apply(id: Int): A = groups(id)

  /** Returns the whole match
   */
  def apply(): A = groups(0)

  /** Returns the requested group
   *
   *  @param id The group name
   *  @return   The requested group
   *  @throws   <code>NoSuchElementException</code> if the requested
   *            group name is not defined
   */
  def apply(id: String): A =
    if (groupNames == null)
      throw new NoSuchElementException("no group names defined")
    else {
      val index = groupNames.get(id)
      if (index == None)
        throw new NoSuchElementException("group name "+id+" not defined")
      else
        apply(index.get)
    }

  /** Returns the list of groups
   *
   *  @return The groups
   */
  def getGroups: List[A] = groups

  /** Returns the list of group names
   *
   *  @return The names
   */
  def getNames = groupNames

  override def toString(): String = groups(0).toString
}
