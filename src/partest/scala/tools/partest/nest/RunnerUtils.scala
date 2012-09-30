/* NEST (New Scala Test)
 * Copyright 2007-2012 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

object RunnerUtils {
  def splitArgs(str: String) = str split "\\s" filterNot (_ == "") toList

  def searchPath(option: String, as: List[String]): Option[String] = as match {
    case `option` :: r :: _ => Some(r)
    case _ :: rest          => searchPath(option, rest)
    case Nil                => None
  }

  def searchAndRemovePath(option: String, as: List[String]) = (as indexOf option) match {
    case -1   => (None, as)
    case idx  => (Some(as(idx + 1)), (as take idx) ::: (as drop (idx + 2)))
  }

  def searchAndRemoveOption(option: String, as: List[String]) = (as indexOf option) match {
    case -1   => (false, as)
    case idx  => (true, (as take idx) ::: (as drop (idx + 1)))
  }
}
