import sbt._
import Keys._
import complete.DefaultParsers._

/** Custom commands for use by the Jenkins scripts. This keeps the surface area and call syntax small. */
object ScriptCommands {
  def all = Seq(setupPublishCore)

  /** Set up the environment for `validate/publish-core`. The argument is the Artifactory snapshot repository URL. */
  def setupPublishCore = Command.single("setupPublishCore") { case (state, url) =>
      Project.extract(state).append(Seq(
        VersionUtil.baseVersionSuffix in Global := "SHA-SNAPSHOT",
        // Append build.timestamp to Artifactory URL to get consistent build numbers (see https://github.com/sbt/sbt/issues/2088):
        publishTo in Global := Some("scala-pr" at url.replaceAll("/$", "") + ";build.timestamp=" + System.currentTimeMillis),
        publishArtifact in (Compile, packageDoc) in ThisBuild := false,
        scalacOptions in Compile in ThisBuild += "-optimise"
      ), state)
    }
}
