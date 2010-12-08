/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Debug.scala 17412 2009-03-31 10:08:25Z michelou $

package scala.remoting

/**
 *  @author Stephane Micheloud
 *  @version 1.0
 */
object Debug extends runtime.remoting.Debug {
  private val f = new java.text.SimpleDateFormat("HH:mm:ss")
  private val c = new java.util.GregorianCalendar

  def getTime: String = f format c.getTime

  def getLocation(obj: AnyRef): String = {
    val s = obj.getClass().getClassLoader().toString()
    s substring s.indexOf('[')
  }
}
