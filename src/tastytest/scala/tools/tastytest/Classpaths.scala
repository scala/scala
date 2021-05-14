package scala.tools.tastytest

import scala.util.Properties
import java.io.File.pathSeparatorChar

object Classpaths {

  private def classpathProp(name: String) =
    Properties.propOrNone(name).map(_.split(pathSeparatorChar).filter(_.nonEmpty).toList).getOrElse(Nil)

  def dottyCompiler: List[String] = classpathProp("tastytest.classpaths.dottyCompiler")

  def scalaReflect: List[String] = classpathProp("tastytest.classpaths.scalaReflect")

  def dottyLibrary: List[String] = classpathProp("tastytest.classpaths.dottyLibrary")

}
