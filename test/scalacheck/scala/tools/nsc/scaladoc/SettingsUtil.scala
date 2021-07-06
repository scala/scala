package scala.tools.nsc.scaladoc

import java.net.{URLClassLoader, URLDecoder}
import java.nio.file.{Files, Path, Paths}

import scala.tools.nsc.Settings
import scala.tools.nsc.scaladoc.HtmlFactoryTest.RESOURCES

object SettingsUtil {
  /* If the context CL is the application (system) CL, use "java.class.path";
   * otherwise call the hook to set the parent CL to use, assume we're running under SBT.
   */
  def configureClassAndSourcePath(settings: Settings): Settings = {
    if (Thread.currentThread.getContextClassLoader == ClassLoader.getSystemClassLoader)
      settings.usejavacp.value = true
    else
      settings.embeddedDefaults[HtmlFactoryTest.type]

    settings
  }
  val checkoutRoot: Path = {
    // Don't assume the working dir is the root of the git checkout to make this work
    // by default in IntelliJ.
    val parents = Iterator.iterate(Paths.get(".").toAbsolutePath)(_.getParent).takeWhile(_ ne null).toList
    val temp = parents.find(x => Files.exists(x.resolve(RESOURCES)))
    val checkoutRoot = temp.getOrElse(Paths.get("."))
    checkoutRoot.toAbsolutePath
  }
}
