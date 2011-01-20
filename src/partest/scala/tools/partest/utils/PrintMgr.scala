/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.partest
package utils

/**
 * @author Thomas Hofer
 */
object PrintMgr {

  val NONE = 0
  val SOME = 1
  val MANY = 2

  var outline = ""
  var success = ""
  var failure = ""
  var warning = ""
  var default = ""

  def initialization(number: Int) = number match {
    case MANY =>
      outline = Console.BOLD + Console.BLACK
      success = Console.BOLD + Console.GREEN
      failure = Console.BOLD + Console.RED
      warning = Console.BOLD + Console.YELLOW
      default = Console.RESET
    case SOME =>
      outline = Console.BOLD + Console.BLACK
      success = Console.RESET
      failure = Console.BOLD + Console.BLACK
      warning = Console.BOLD + Console.BLACK
      default = Console.RESET
    case _ =>
  }

  def printOutline(msg: String) = print(outline + msg + default)

  def printSuccess(msg: String) = print(success  + msg + default)

  def printFailure(msg: String) = print(failure  + msg + default)

  def printWarning(msg: String) = print(warning  + msg + default)
}
