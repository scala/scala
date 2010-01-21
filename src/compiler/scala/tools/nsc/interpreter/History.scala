/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import scala.collection.JavaConversions.asBuffer

/** Primarily, a wrapper for JLine's History.
 */
class History(jhistory: jline.History) {
  def asJavaList = jhistory.getHistoryList
  def asList: List[String] = asBuffer(asJavaList).toList
  def index = jhistory.getCurrentIndex
}