import sbt._
import Keys._
import BuildSettings.autoImport._

/** Custom commands for use by the Jenkins scripts. This keeps the surface area and call syntax small. */
object ScriptCommands {
  def all = Seq(
    setupPublishCore,
    setupValidateTest,
    setupBootstrapStarr, setupBootstrapLocker, setupBootstrapQuick, setupBootstrapPublish
  )

  /** Set up the environment for `validate/publish-core`.
    * The optional argument is the Artifactory snapshot repository URL. */
  def setupPublishCore = setup("setupPublishCore") { args =>
    Seq(
      baseVersionSuffix in Global := "SHA-SNAPSHOT"
    ) ++ (args match {
      case Seq(url) => publishTarget(url)
      case Nil => Nil
    }) ++ noDocs ++ enableOptimizer
  }

  /** Set up the environment for `validate/test`.
    * The optional argument is the Artifactory snapshot repository URL. */
  def setupValidateTest = setup("setupValidateTest") { args =>
    Seq(
      testOptions in IntegrationTest in LocalProject("test") ++= Seq(Tests.Argument("--show-log"), Tests.Argument("--show-diff"))
    ) ++ (args match {
      case Seq(url) => Seq(resolvers in Global += "scala-pr" at url)
      case Nil => Nil
    }) ++ enableOptimizer
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
    * All artifacts are published to Sonatype. */
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

  private[this] def publishTarget(url: String) = {
    // Append build.timestamp to Artifactory URL to get consistent build numbers (see https://github.com/sbt/sbt/issues/2088):
    val url2 = if(url.startsWith("file:")) url else url.replaceAll("/$", "") + ";build.timestamp=" + System.currentTimeMillis
    Seq(publishTo in Global := Some("scala-pr-publish" at url2))
  }

  /** Like `Def.sequential` but accumulate all results */
  def sequence[B](tasks: List[Def.Initialize[Task[B]]]): Def.Initialize[Task[List[B]]] = tasks match {
    case Nil => Def.task { Nil }
    case x :: xs => Def.taskDyn {
      val v = x.value
      sequence(xs).apply((t: Task[List[B]]) => t.map(l => v :: l))
    }
  }
}
