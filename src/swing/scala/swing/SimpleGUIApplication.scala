package scala.swing

import javax.swing._

/**
 * Extend this class for most simple UI applications. Clients need to implement the
 * <code>top</code> method. Framework intialization is done by this class.
 *
 * In order to conform to Swing's threading policy, never implement top or any additional
 * member that created Swing components as a value unless component creation happens on
 * the EDT (see Swing.onEDT and Swing.onEDTWait). Lazy values are okay for the same reason
 * if they are intialized on the EDT always.
 */
abstract class SimpleGUIApplication extends GUIApplication {

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
    new java.io.File(System.getProperty("user.dir"), path)
}
