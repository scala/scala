package scala.reflect.macros
package contexts

trait Infrastructure {
  self: Context =>

  def settings: List[String] = {
    val us = universe.settings
    import us._
    userSetSettings collectFirst { case x: MultiStringSetting if x.name == XmacroSettings.name => x.value } getOrElse Nil
  }

  def compilerSettings: List[String] = universe.settings.recreateArgs

  def classPath: List[java.net.URL] = global.classPath.asURLs.toList
}
