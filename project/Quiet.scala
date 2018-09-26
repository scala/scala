package scala.build

import sbt._
import Keys._
import java.util.function.Supplier

object Quiet {
  // Workaround sbt issue described:
  //
  //   https://github.com/scala/scala-dev/issues/100
  def silenceScalaBinaryVersionWarning = scalaModuleInfo ~= (x => x.map(_.withCheckExplicit(false)))
}
