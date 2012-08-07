package scala.reflect.macros
package runtime

trait Settings {
  self: Context =>

  def settings: List[String] = {
    val us = universe.settings
    import us._
    userSetSettings collectFirst { case x: MultiStringSetting if x.name == XmacroSettings.name => x.value } getOrElse Nil
  }

  def compilerSettings: List[String] = universe.settings.recreateArgs

  def setCompilerSettings(options: String): this.type =
    // todo. is not going to work with quoted arguments with embedded whitespaces
    setCompilerSettings(options.split(" ").toList)

  def setCompilerSettings(options: List[String]): this.type = {
    val settings = new tools.nsc.Settings(_ => ())
    // [Eugene] what settings should we exclude?
    settings.copyInto(universe.settings)
    this
  }

  def withCompilerSettings[T](options: String)(op: => T): T =
    // todo. is not going to work with quoted arguments with embedded whitespaces
    withCompilerSettings(options.split(" ").toList)(op)

  def withCompilerSettings[T](options: List[String])(op: => T): T = {
    val old = options
    setCompilerSettings(options)
    try op
    finally setCompilerSettings(old)
  }
}
