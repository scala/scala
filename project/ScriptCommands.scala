import sbt._
import Keys._
import BuildSettings.autoImport._

/** Custom commands for use by the Jenkins scripts. This keeps the surface area and call syntax small. */
object ScriptCommands {
  def all = Seq(setupPublishCore, setupValidateTest)

  /** Set up the environment for `validate/publish-core`. The argument is the Artifactory snapshot repository URL. */
  def setupPublishCore = Command.single("setupPublishCore") { case (state, url) =>
      Project.extract(state).append(Seq(
        baseVersionSuffix in Global := "SHA-SNAPSHOT",
        // Append build.timestamp to Artifactory URL to get consistent build numbers (see https://github.com/sbt/sbt/issues/2088):
        publishTo in Global := Some("scala-pr" at url.replaceAll("/$", "") + ";build.timestamp=" + System.currentTimeMillis),
        publishArtifact in (Compile, packageDoc) in ThisBuild := false,
        scalacOptions in Compile in ThisBuild += "-optimise",
        logLevel in ThisBuild := Level.Info,
        logLevel in update in ThisBuild := Level.Warn
      ), state)
    }

  /** Set up the environment for `validate/test`. The argument is the Artifactory snapshot repository URL. */
  def setupValidateTest = Command.single("setupValidateTest") { case (state, url) =>
    //TODO When ant is gone, pass starr version as an argument to this command instead of using version.properties
    Project.extract(state).append(Seq(
      resolvers in Global += "scala-pr" at url,
      scalacOptions in Compile in ThisBuild += "-optimise",
      logLevel in ThisBuild := Level.Info,
      logLevel in update in ThisBuild := Level.Warn
    ), state)
  }
}
