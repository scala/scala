package scala.reflect
package macros

trait Infrastructure {
  self: Context =>

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