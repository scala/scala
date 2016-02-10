// It would be nice to use sbt-mima-plugin here, but the plugin is missing
// at least two features we need:
// * ability to run MiMa twice, swapping `curr` and `prev`, to detect
//   both forwards and backwards incompatibilities
// * ability to pass a filter file
// So we invoke the MiMa CLI directly; it's also what the Ant build did.

import sbt._
import sbt.Keys._

object MiMa {

  lazy val mimaOldVersion =
    settingKey[String]("Scala version number to run MiMa against")

  lazy val mima =
    taskKey[Unit]("run Migration Manager to detect binary incompatibilities")

  lazy val settings =
    Seq(
      mimaOldVersion := s"${scalaBinaryVersion.value}.0",
      mima := {
        val log = streams.value.log
        def runOnce(prev: java.io.File, curr: java.io.File, isForward: Boolean): Unit = {
          val direction = if (isForward) "forward" else "backward"
          log.info(s"Checking $direction binary compatibility")
          runMima(
            prev = if (isForward) curr else prev,
            curr = if (isForward) prev else curr,
            // TODO: it would be nicer if each subproject had its own whitelist, but for now
            // for compatibility with how Ant did things, there's just one at the root.
            // once Ant is gone we'd be free to split it up.
            filter = baseDirectory.value / ".." / ".." / s"bincompat-$direction.whitelist.conf",
            log)
        }
        val artifact =
          getPreviousArtifact(
            "org.scala-lang" % s"${name.value}" % mimaOldVersion.value,
            ivySbt.value, streams.value)
        for (isForward <- Seq(false, true))
          runOnce(artifact, (packageBin in Compile).value, isForward)
      }
    )

  def runMima(prev: java.io.File, curr: java.io.File, filter: java.io.File, log: Logger): Unit = {
    val args = Array(
      "--prev", prev.getAbsolutePath,
      "--curr", curr.getAbsolutePath,
      "--filters", filter.getAbsolutePath,
      "--generate-filters"
    )
    val exitCode = TrapExit(com.typesafe.tools.mima.cli.Main.main(args), log)
    if (exitCode != 0)
      throw new RuntimeException(s"MiMa failed with exit code $exitCode")
  }

  // cribbed from https://github.com/typesafehub/migration-manager/blob/master/sbtplugin/src/main/scala/com/typesafe/tools/mima/plugin/SbtMima.scala
  def getPreviousArtifact(m: ModuleID, ivy: IvySbt, s: TaskStreams): File = {
    val moduleSettings = InlineConfiguration(
      "dummy" % "test" % "version",
      ModuleInfo("dummy-test-project-for-resolving"),
      dependencies = Seq(m))
    val module = new ivy.Module(moduleSettings)
    val report = IvyActions.update(
      module,
      new UpdateConfiguration(
        retrieve = None,
        missingOk = false,
        logging = UpdateLogging.DownloadOnly),
      s.log)
    val optFile = (for {
      config <- report.configurations
      module <- config.modules
      (artifact, file) <- module.artifacts
      // TODO - Hardcode this?
      if artifact.name == m.name
    } yield file).headOption
    optFile getOrElse sys.error("Could not resolve previous artifact: " + m)
  }

}
