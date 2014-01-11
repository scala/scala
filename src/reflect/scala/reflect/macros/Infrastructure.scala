package scala
package reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  A slice of [[scala.reflect.macros.blackbox.Context the Scala macros context]] that
 *  provides facilities to communicate with the compiler's infrastructure.
 */
trait Infrastructure {
  self: blackbox.Context =>

  /** Exposes macro-specific settings as a list of strings.
   *  These settings are passed to the compiler via the "-Xmacro-settings:setting1,setting2...,settingN" command-line option.
   */
  def settings: List[String]

  /** Exposes current compiler settings as a list of options.
   *  Use `scalac -help`, `scalac -X` and `scalac -Y` to learn about currently supported options.
   */
  def compilerSettings: List[String]

  /** Exposes current classpath. */
  def classPath: List[java.net.URL]
}
