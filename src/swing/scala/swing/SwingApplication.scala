package scala.swing

/** Convenience class with utility methods for GUI applications. */
abstract class SwingApplication extends Reactor {

  /** Initializes the application and runs the given program. */
  def main(args: Array[String]) = Swing.onEDT { startup(args) }

  /** Called before the GUI is created. Override to customize. */
  def startup(args: Array[String])

  /** Finalizes the application by calling `shutdown` and exits.*/
  def quit() { shutdown(); sys.exit(0) }

  /** Called before the application is exited. Override to customize. */
  def shutdown() {}
}
