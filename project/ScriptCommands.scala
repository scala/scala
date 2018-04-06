package scala.build

import java.nio.file.Paths

import sbt._
import Keys._

import BuildSettings.autoImport._

/** Custom commands for use by the Jenkins scripts. This keeps the surface area and call syntax small. */
object ScriptCommands {
  def env(key: String) = Option(System.getenv(key)).getOrElse("")

  def all = Seq(
    setupPublishCore,
    setupValidateTest,
    setupBootstrapStarr, setupBootstrapLocker, setupBootstrapQuick, setupBootstrapPublish,
    enableOptimizerCommand
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
  def setupBootstrapStarr = setup("setupBootstrapStarr") { case Seq(fileOrUrl, ver) =>
    val url = fileToUrl(fileOrUrl)
    Seq(
      baseVersion in Global := ver,
      baseVersionSuffix in Global := "SPLIT"
    ) ++ publishTarget(url) ++ noDocs ++ enableOptimizer
  }

  /** Set up the environment for building locker in `validate/bootstrap`. The arguments are:
    * - Repository file or URL for publishing locker and resolving STARR
    * - Version number to publish */
  def setupBootstrapLocker = setup("setupBootstrapLocker") { case Seq(fileOrUrl, ver) =>
    val url = fileToUrl(fileOrUrl)
    Seq(
      baseVersion in Global := ver,
      baseVersionSuffix in Global := "SPLIT",
      resolvers in Global += "scala-pr" at url
    ) ++ publishTarget(url) ++ noDocs ++ enableOptimizer
  }

  /** Set up the environment for building quick in `validate/bootstrap`. The arguments are:
    * - Repository URL for publishing
    * - Version number to publish
    * - Optional: Repository for resolving (same as repository for publishing if not specified)
    * Note that the artifacts produced here are consumed by scala-dist, so the docs have to be built.
    */
  def setupBootstrapQuick = setup("setupBootstrapQuick") { case Seq(targetFileOrUrl, ver, resolverFileOrUrl) =>
    val targetUrl = fileToUrl(targetFileOrUrl)
    val resolverUrl = fileToUrl(resolverFileOrUrl)
    Seq(
      baseVersion in Global := ver,
      baseVersionSuffix in Global := "SPLIT",
      resolvers in Global += "scala-pr" at resolverUrl,
      testOptions in IntegrationTest in LocalProject("test") ++= Seq(Tests.Argument("--show-log"), Tests.Argument("--show-diff"))
    ) ++ publishTarget(targetUrl) ++ enableOptimizer
  }

  /** Set up the environment for publishing in `validate/bootstrap`. The arguments are:
    * - Temporary bootstrap repository URL for resolving modules
    * - Version number to publish
    * All artifacts are published to Sonatype. */
  def setupBootstrapPublish = setup("setupBootstrapPublish") { case Seq(fileOrUrl, ver) =>
    val url = fileToUrl(fileOrUrl)
    Seq(
      baseVersion in Global := ver,
      baseVersionSuffix in Global := "SPLIT",
      resolvers in Global += "scala-pr" at url,
      publishTo in Global := Some("sonatype-releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"),
      credentials in Global += Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", env("SONA_USER"), env("SONA_PASS"))
      // pgpSigningKey and pgpPassphrase are set externally by travis / the bootstrap script, as the sbt-pgp plugin is not enabled by default
    ) ++ enableOptimizer
  }

  def enableOptimizerCommand = setup("enableOptimizer")(_ => enableOptimizer)

  private[this] def setup(name: String)(f: Seq[String] => Seq[Setting[_]]) = Command.args(name, name) { case (state, seq) =>
    // `Project.extract(state).append(f(seq), state)` would be simpler, but it
    // takes the project's initial state and discards all changes that were made in the sbt console.
    val session = Project.session(state)
    val extracted = Project.extract(state)
    val settings = f(seq)
    val appendSettings = Load.transformSettings(Load.projectScope(extracted.currentRef), extracted.currentRef.build, extracted.rootProject, settings)
    val newStructure = Load.reapply(session.mergeSettings ++ appendSettings, extracted.structure)(extracted.showKey)
    Project.setProject(session, newStructure, state)
  }

  private[this] val enableOptimizer = Seq(
    scalacOptions in Compile in ThisBuild ++= Seq("-opt:l:inline", "-opt-inline-from:scala/**")
  )

  private[this] val noDocs = Seq(
    publishArtifact in (Compile, packageDoc) in ThisBuild := false
  )

  private[this] def publishTarget(url: String) = {
    // Append build.timestamp to Artifactory URL to get consistent build numbers (see https://github.com/sbt/sbt/issues/2088):
    val url2 = if(url.startsWith("file:")) url else url.replaceAll("/$", "") + ";build.timestamp=" + System.currentTimeMillis

    Seq(
      publishTo in Global := Some("scala-pr-publish" at url2),
      credentials in Global += Credentials("Artifactory Realm", "scala-ci.typesafe.com", "scala-ci", env("PRIVATE_REPO_PASS"))
    )
  }

  // If fileOrUrl is already a file:, http: or https: URL, return it, otherwise treat it as a local file and return a URL for it
  private[this] def fileToUrl(fileOrUrl: String): String =
    if(fileOrUrl.startsWith("file:") || fileOrUrl.startsWith("http:") || fileOrUrl.startsWith("https:")) fileOrUrl
    else Paths.get(fileOrUrl).toUri.toString

  /** Like `Def.sequential` but accumulate all results */
  def sequence[B](tasks: List[Def.Initialize[Task[B]]]): Def.Initialize[Task[List[B]]] = tasks match {
    case Nil => Def.task { Nil }
    case x :: xs => Def.taskDyn {
      val v = x.value
      sequence(xs).apply((t: Task[List[B]]) => t.map(l => v :: l))
    }
  }
}
