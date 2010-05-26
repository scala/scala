/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

import javax.swing._

/**
 * Extend this class for most simple UI applications. Clients need to implement the
 * <code>top</code> method. Framework initialization is done by this class.
 *
 * In order to conform to Swing's threading policy, never implement top or any additional
 * member that created Swing components as a value unless component creation happens on
 * the EDT (see Swing.onEDT and Swing.onEDTWait). Lazy values are okay for the same reason
 * if they are initialized on the EDT always.
 */
@deprecated("Use SimpleSwingApplication instead") abstract class SimpleGUIApplication extends GUIApplication {

  /**
   * A GUI application's version of the main method. Called by the default
   * main method implementation provided by this class.
   * Implement to return the top-level frame of this application.
   */
  def top: Frame

  /**
   * Calls top, packs the frame, and displays it.
   */
  def main(args: Array[String]) = run {
    val t = top
    t.pack()
    t.visible = true
  }

  def resourceFromClassloader(path: String): java.net.URL =
    this.getClass.getResource(path)

  def resourceFromUserDirectory(path: String): java.io.File =
    new java.io.File(util.Properties.userDir, path)
}
