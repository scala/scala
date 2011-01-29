/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

/** An implementation-agnostic history interface which makes no
 *  reference to the jline classes.
 */
trait History {
  def asStrings: List[String]
  def index: Int
  def size: Int
  def grep(s: String): List[String]
  def flush(): Unit
}
object NoHistory extends History {
  def asStrings       = Nil
  def grep(s: String) = Nil
  def index           = 0
  def size            = 0
  def flush()         = ()
}

object History {
  def empty: History = NoHistory
}
