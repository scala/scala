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
    // SI-5925: doesn't work with arguments that contains whitespaces
    setCompilerSettings(options.split(" ").toList)

  def setCompilerSettings(options: List[String]): this.type = {
    val settings = new scala.tools.nsc.Settings(_ => ())
    settings.copyInto(universe.settings)
    this
  }

  def withCompilerSettings[T](options: String)(op: => T): T =
    // SI-5925: doesn't work with arguments that contains whitespaces
    withCompilerSettings(options.split(" ").toList)(op)

  def withCompilerSettings[T](options: List[String])(op: => T): T = {
    val old = options
    setCompilerSettings(options)
    try op
    finally setCompilerSettings(old)
  }
}
