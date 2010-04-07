/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import java.io.File
import jline.{ ConsoleReader, History => JHistory }
import scala.collection.JavaConversions.asBuffer
import Properties.userHome

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

  def apply(reader: ConsoleReader): History =
    if (reader == null) apply()
    else new History(reader.getHistory)

  def apply(): History = new History(
    try new JHistory(new File(userHome, ScalaHistoryFile))
    // do not store history if error
    catch { case _: Exception => new JHistory() }
  )
}
