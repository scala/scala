import sbt._
import Keys._
import BuildSettings.autoImport._

/** Custom commands for use by the Jenkins scripts. This keeps the surface area and call syntax small. */
object ScriptCommands {
  def all = Seq(
    setupPublishCore,
    setupValidateTest,
    setupBootstrapStarr, setupBootstrapLocker, setupBootstrapQuick, setupBootstrapPublish,
    testAll
  )

  /** Set up the environment for `validate/publish-core`. The argument is the Artifactory snapshot repository URL. */
  def setupPublishCore = setup("setupPublishCore") { case Seq(url) =>
    Seq(
      baseVersionSuffix in Global := "SHA-SNAPSHOT"
    ) ++ publishTarget(url) ++ noDocs ++ enableOptimizer
  }

  /** Set up the environment for `validate/test`. The argument is the Artifactory snapshot repository URL. */
  def setupValidateTest = setup("setupValidateTest") { case Seq(url) =>
    //TODO When ant is gone, pass starr version as an argument to this command instead of using version.properties
    Seq(
      resolvers in Global += "scala-pr" at url,
      testOptions in IntegrationTest in LocalProject("test") ++= Seq(Tests.Argument("--show-log"), Tests.Argument("--show-diff"))
    ) ++ enableOptimizer
  }

  /** Set up the environment for building STARR in `validate/bootstrap`. The arguments are:
    * - Repository URL for publishing
    * - Version number to publish */
  def setupBootstrapStarr = setup("setupBootstrapStarr") { case Seq(url, ver) =>
    Seq(
      baseVersion in Global := ver,
      baseVersionSuffix in Global := "SPLIT"
    ) ++ publishTarget(url) ++ noDocs ++ enableOptimizer
  }

  /** Set up the environment for building locker in `validate/bootstrap`. The arguments are:
    * - Repository URL for publishing locker and resolving STARR
    * - Version number to publish */
  def setupBootstrapLocker = setup("setupBootstrapLocker") { case Seq(url, ver) =>
    Seq(
      baseVersion in Global := ver,
      baseVersionSuffix in Global := "SPLIT",
      resolvers in Global += "scala-pr" at url
    ) ++ publishTarget(url) ++ noDocs ++ enableOptimizer
  }

  /** Set up the environment for building quick in `validate/bootstrap`. The arguments are:
    * - Repository URL for publishing
    * - Version number to publish */
  def setupBootstrapQuick = setup("setupBootstrapQuick") { case Seq(url, ver) =>
    Seq(
      baseVersion in Global := ver,
      baseVersionSuffix in Global := "SPLIT",
      resolvers in Global += "scala-pr" at url,
      testOptions in IntegrationTest in LocalProject("test") ++= Seq(Tests.Argument("--show-log"), Tests.Argument("--show-diff"))
    ) ++ publishTarget(url) ++ enableOptimizer
  }

  /** Set up the environment for publishing in `validate/bootstrap`. The arguments are:
    * - Temporary bootstrap repository URL for resolving modules
    * - Version number to publish
    * All artifacts are published to Sonatype. GPG signing has to be configured from the
    * shell script after `setupBootstrapPublish` because we don't pull the GPG plugin in
    * by default, so we cannot reference its keys statically. */
  def setupBootstrapPublish = setup("setupBootstrapPublish") { case Seq(url, ver) =>
    // Define a copy of the setting key here in case the plugin is not part of the build
    val pgpPassphrase = SettingKey[Option[Array[Char]]]("pgp-passphrase", "The passphrase associated with the secret used to sign artifacts.", KeyRanks.BSetting)
    Seq(
      baseVersion in Global := ver,
      baseVersionSuffix in Global := "SPLIT",
      resolvers in Global += "scala-pr" at url,
      publishTo in Global := Some("sonatype-releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"),
      credentials in Global += Credentials(Path.userHome / ".credentials-sonatype"),
      pgpPassphrase in Global := Some(Array.empty)
    ) ++ enableOptimizer
  }

  private[this] def setup(name: String)(f: Seq[String] => Seq[Setting[_]]) =
    Command.args(name, name) { case (state, seq) => Project.extract(state).append(f(seq) ++ resetLogLevels, state) }

  private[this] val resetLogLevels = Seq(
    logLevel in ThisBuild := Level.Info,
    logLevel in update in ThisBuild := Level.Warn
  )

  private[this] val enableOptimizer = Seq(
    scalacOptions in Compile in ThisBuild += "-opt:l:classpath"
  )

  private[this] val noDocs = Seq(
    publishArtifact in (Compile, packageDoc) in ThisBuild := false
  )

  private[this] def publishTarget(url: String) = Seq(
    // Append build.timestamp to Artifactory URL to get consistent build numbers (see https://github.com/sbt/sbt/issues/2088):
    publishTo in Global := Some("scala-pr-publish" at url.replaceAll("/$", "") + ";build.timestamp=" + System.currentTimeMillis)
  )

  def testAll = Command.command("testAll") { state =>
    val cmds = Seq(
      "test",
      "partest run pos neg jvm",
      "partest res scalap specialized scalacheck",
      "partest instrumented presentation",
      "partest --srcpath scaladoc",
      "osgiTestFelix/test",
      "osgiTestEclipse/test",
      "library/mima",
      "reflect/mima",
      "doc"
    )
    state.log.info(cmds.mkString("Running all tests: \"", "\", \"", "\""))
    cmds ::: state
  }
}
