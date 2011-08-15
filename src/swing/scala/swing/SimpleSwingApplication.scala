package scala.swing

/**
 * Extend this class for most simple UI applications. Clients need to
 * implement the `top` method. Framework initialization is done by this class.
 *
 * In order to conform to Swing's threading policy, never implement top or any
 * additional member that created Swing components as a value unless component
 * creation happens on the EDT (see `Swing.onEDT` and `Swing.onEDTWait`).
 * Lazy values are okay for the same reason if they are initialized on the EDT
 * always.
 */
abstract class SimpleSwingApplication extends SwingApplication {

  /**
   * A GUI application's version of the main method. Called by the default
   * main method implementation provided by this class.
   * Implement to return the top-level frame of this application.
   */
  def top: Frame

  /**
   * Calls `top`, packs the frame, and displays it.
   */
  override def startup(args: Array[String]) {
    val t = top
    if (t.size == new Dimension(0,0)) t.pack()
    t.visible = true
  }

  def resourceFromClassloader(path: String): java.net.URL =
    this.getClass.getResource(path)

  def resourceFromUserDirectory(path: String): java.io.File =
    new java.io.File(util.Properties.userDir, path)
}
