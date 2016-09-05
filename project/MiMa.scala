// It would be nice to use sbt-mima-plugin here, but the plugin is missing
// at least two features we need:
// * ability to run MiMa twice, swapping `curr` and `prev`, to detect
//   both forwards and backwards incompatibilities (possibly fixed as of
//   https://github.com/typesafehub/migration-manager/commit/2844ffa48b6d2255aa64bd687703aec21dadd55e)
// * ability to pass a filter file (https://github.com/typesafehub/migration-manager/issues/102)
// So we invoke the MiMa CLI directly.

import sbt._
import sbt.Keys._
import BuildSettings.autoImport._

object MiMa {
  lazy val mima =
    taskKey[Unit]("run Migration Manager to detect binary incompatibilities")

  lazy val settings =
    Seq(
      mima := {
        val log = streams.value.log
        mimaReferenceVersion.value.fold {
          log.info(s"No reference version defined - skipping binary compatibility checks")
        } { refVersion =>
          def runOnce(prev: java.io.File, curr: java.io.File, isForward: Boolean): Unit = {
            val direction = if (isForward) "forward" else "backward"
            log.info(s"Checking $direction binary compatibility")
            log.debug(s"prev = $prev, curr = $curr")
            runMima(
              prev = if (isForward) curr else prev,
              curr = if (isForward) prev else curr,
              // TODO: it would be nicer if each subproject had its own whitelist, but for now
              // there's just one at the root. with the Ant build gone, we would be free now to split it.
              filter = (baseDirectory in ThisBuild).value / s"bincompat-$direction.whitelist.conf",
              log)
          }
          val artifact =
            getPreviousArtifact(
              "org.scala-lang" % s"${name.value}" % refVersion,
              ivySbt.value, streams.value)
          for (isForward <- Seq(false, true))
            runOnce(artifact, (packageBin in Compile).value, isForward)
        }
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
    val report = Deprecated.Inner.ivyUpdate(ivy)(module, s)
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

// use the SI-7934 workaround to silence a deprecation warning on an sbt API
// we have no choice but to call.  on the lack of any suitable alternative,
// see https://gitter.im/sbt/sbt-dev?at=5616e2681b0e279854bd74a4 :
// "it's my intention to eventually come up with a public API" says Eugene Y
object Deprecated {
  @deprecated("", "") class Inner {
    def ivyUpdate(ivy: IvySbt)(module: ivy.Module, s: TaskStreams) =
      IvyActions.update(
        module,
        new UpdateConfiguration(
          retrieve = None,
          missingOk = false,
          logging = UpdateLogging.DownloadOnly),
        s.log)
  }
  object Inner extends Inner
}
