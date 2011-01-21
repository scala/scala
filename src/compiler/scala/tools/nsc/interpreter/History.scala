/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import java.io.File
import scala.tools.jline.console.history._
import scala.tools.jline.console.history.{ FileHistory, PersistentHistory, History => JHistory }
import scala.tools.jline.console.history.History.{ Entry => JEntry }
import scala.tools.jline.console.ConsoleReader
import scala.collection.JavaConverters._
import Properties.userHome

/** Primarily, a wrapper for JLine's History.
 */
class History(val jhistory: JHistory) {
  def asJavaList = jhistory.entries()
  def asStrings = asList map (_.value.toString)
  def asList: List[JEntry] = asJavaList.asScala.toList
  def index = jhistory.index()
  def size = jhistory.size()

  def grep(s: String) = asStrings filter (_ contains s)
  def flush() = jhistory match {
    case x: PersistentHistory => x.flush()
    case _                    => ()
  }
}

object History {
  val ScalaHistoryFile = ".scala_history"

  def apply(): History = new History(
    try new FileHistory(new File(userHome, ScalaHistoryFile)) {
      // flush after every add to avoid installing a shutdown hook.
      // (The shutdown hook approach also loses history when they aren't run.)
      override def add(item: CharSequence): Unit = {
        super.add(item)
        flush()
      }
    }
    catch {
      case x: Exception =>
        Console.println("Error creating file history: memory history only. " + x)
        new MemoryHistory()
    }
  )
}
