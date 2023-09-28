import sbt._
import Keys._

object ScriptedTestPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger = allRequirements

  object autoImport {
    val setup = taskKey[StateTransform]("setup scripted test")
    val cacheId = AttributeKey[String]("cacheId")
  }
  import autoImport._

  override val projectSettings = Seq(
    scalaVersion := sys.props("plugin.scalaVersion"),
    setup := {
      IO.copyFile(Path(sys.props("scripted.common")).asFile, baseDirectory.value / "common.sbt")
      val id = java.util.UUID.randomUUID().toString
      StateTransform(_.put(cacheId, id))
    },
    // https://github.com/sbt/sbt/issues/7432
    Compile / compileAnalysisFilename := (Compile / compileAnalysisFilename).value.dropRight(4) + "-" + state.value.get(cacheId).get + ".zip"
  )
}
