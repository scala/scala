/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter
package session

/** An implementation-agnostic history interface which makes no
 *  reference to the jline classes.  Very sparse right now.
 */
trait History {
  def asStrings: List[String]
  def index: Int
  def size: Int
  def grep(s: String): List[String]
}
object NoHistory extends History {
  def asStrings       = Nil
  def grep(s: String) = Nil
  def index           = 0
  def size            = 0
}

object History {
  def empty: History = NoHistory
}
