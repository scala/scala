import sbt._
import Keys._

object ScriptedTestPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger = allRequirements

  override val projectSettings = Seq(
    scalaVersion := sys.props("plugin.scalaVersion"),
  )
}
