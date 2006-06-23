/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

/**
 * @author Philipp Haller
 */
object Debug {
  var lev = 2

  def level = lev
  def level_= (lev: int) = {
    //Console.println("Setting debug level to " + lev)
    this.lev = lev
  }

  def info(s: String) =
    if (lev > 2) System.out.println("Info: " + s)
  def warning(s: String) =
    if (lev > 1) System.err.println("Warning: " + s)
  def error(s: String) =
    if (lev > 0) System.err.println("Error: " + s)
}

class Debug(tag: String) {
  var lev = 2

  def level = lev
  def level_= (lev: int) = {
    //Console.println("Setting debug level (" + tag + ") to " + lev)
    this.lev = lev
  }

  def info(s: String) =
    if (lev > 2) System.out.println(tag + " (info): " + s)
  def warning(s: String) =
    if (lev > 1) System.err.println(tag + " (warn): " + s)
  def error(s: String) =
    if (lev > 0) System.err.println(tag + " (erro): " + s)
}
