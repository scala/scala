package scala.reflect
package macros

trait Infrastructure {
  self: Context =>

  /** Exposes current compilation run.
   */
  val currentRun: Run

  /** Exposes current classpath.
   */
  val currentClassPath: List[java.net.URL]

}
