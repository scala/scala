/* NEST (New Scala Test)
 * Copyright 2007-2009 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

trait RunnerUtils {

  def searchPath(option: String, as: List[String]): Option[String] = {
    val Option = option
    as match {
      case Option :: r :: rs => Some(r)
      case other :: rest => searchPath(option, rest)
      case List() => None
    }
  }

  def searchAndRemovePath(option: String, as: List[String]): (Option[String], List[String]) = {
    val Option = option
    def search(before: List[String], after: List[String]): (Option[String], List[String]) = after match {
      case Option :: r :: rs => (Some(r), before ::: rs)
      case other :: rest => search(before ::: List(other), rest)
      case List() => (None, before)
    }
    search(List(), as)
  }

  def searchAndRemoveOption(option: String, as: List[String]): (Boolean, List[String]) = {
    val Option = option
    def search(before: List[String], after: List[String]): (Boolean, List[String]) = after match {
      case Option :: rest => (true, before ::: rest)
      case other :: rest => search(before ::: List(other), rest)
      case List() => (false, before)
    }
    search(List(), as)
  }

}
