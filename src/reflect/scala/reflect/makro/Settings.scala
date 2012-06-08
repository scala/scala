package scala.reflect
package makro

trait Settings {
  self: Context =>

  /** Exposes macro-specific settings as a list of strings.
   *  These settings are passed to the compiler via the "-Xmacro-settings:setting1,setting2...,settingN" command-line option.
   */
  def settings: List[String]

  /** Exposes current compiler settings as a list of options.
   *  Use `scalac -help`, `scalac -X` and `scalac -Y` to learn about currently supported options.
   */
  // [Eugene] ugly? yes, but I don't really fancy copy/pasting all our settings here and keep it synchronized at all times
  // why all settings? because macros need to be in full control of the stuff going on
  // maybe later we can implement a gettable/settable list of important settings, but for now let's leave it like that
  def compilerSettings: List[String]

  /** Updates current compiler settings with an option string.
   *  Use `scalac -help`, `scalac -X` and `scalac -Y` to learn about currently supported options.
   *  todo. http://groups.google.com/group/scala-internals/browse_thread/thread/07c18cff41f59203
   */
  def setCompilerSettings(options: String): this.type

  /** Updates current compiler settings with a list of options.
   *  Use `scalac -help`, `scalac -X` and `scalac -Y` to learn about currently supported options.
   */
  def setCompilerSettings(options: List[String]): this.type

  /** Temporary sets compiler settings to a given option string and executes a given closure.
   *  Use `scalac -help`, `scalac -X` and `scalac -Y` to learn about currently supported options.
   */
  def withCompilerSettings[T](options: String)(op: => T): T

  /** Temporary sets compiler settings to a given list of options and executes a given closure.
   *  Use `scalac -help`, `scalac -X` and `scalac -Y` to learn about currently supported options.
   */
  def withCompilerSettings[T](options: List[String])(op: => T): T
}