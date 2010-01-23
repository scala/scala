/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import java.io.File
import jline.{ ConsoleReader, History => JHistory }
import scala.collection.JavaConversions.asBuffer

/** Primarily, a wrapper for JLine's History.
 */
class History(val jhistory: JHistory) {
  def asJavaList = jhistory.getHistoryList
  def asList: List[String] = asBuffer(asJavaList).toList
  def index = jhistory.getCurrentIndex

  def grep(s: String) = asList filter (_ contains s)
}

object History {
  val ScalaHistoryFile = ".scala_history"
  def homeDir = System.getProperty("user.home")

  def apply(reader: ConsoleReader): History =
    if (reader == null) apply()
    else new History(reader.getHistory)

  def apply(): History = new History(
    try new JHistory(new File(homeDir, ScalaHistoryFile))
    // do not store history if error
    catch { case _: Exception => new JHistory() }
  )
}
