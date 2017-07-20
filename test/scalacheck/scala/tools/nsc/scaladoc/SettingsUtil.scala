package scala.tools.nsc.scaladoc

import java.net.{URLClassLoader, URLDecoder}
import java.nio.file.{Files, Path, Paths}

import scala.tools.nsc.Settings
import scala.tools.nsc.scaladoc.HtmlFactoryTest.RESOURCES

object SettingsUtil {
  def configureClassAndSourcePath(settings: Settings): Settings = {
    val ourClassLoader = HtmlFactoryTest.getClass.getClassLoader
    Thread.currentThread.getContextClassLoader match {
      case loader: URLClassLoader =>
        val paths = loader.getURLs.map(u => URLDecoder.decode(u.getPath))
        settings.classpath.value = paths mkString java.io.File.pathSeparator
      case loader =>
        settings.embeddedDefaults(ourClassLoader) // Running in SBT without forking, we have to ask the SBT classloader for the classpath
    }

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
