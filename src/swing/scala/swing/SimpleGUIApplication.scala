package scala.swing

import javax.swing._

/**
 * Extend this class for most simple UI applications. Clients need to implement the
 * <code>top</code> method. Framework intialization is done by this class.
 */
abstract class SimpleGUIApplication extends GUIApplication {
  def top: Frame

  def main(args: Array[String]) = {
    SwingUtilities.invokeLater {
      new Runnable { def run() { init(); top.pack(); top.visible = true } }
    }
  }

  def resourceFromClassloader(path: String): java.net.URL =
    this.getClass.getResource(path)

  def resourceFromUserDirectory(path: String): java.io.File =
    new java.io.File(System.getProperty("user.dir"), path)
}
