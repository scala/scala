import sbt._
import Keys._
import java.util.Properties
import java.io.{File, FileInputStream}
import scala.collection.JavaConverters._
import BuildSettings.autoImport._

object VersionUtil {
  lazy val copyrightString = settingKey[String]("Copyright string.")
  lazy val versionProperties = settingKey[Versions]("Version properties.")
  lazy val generateVersionPropertiesFile = taskKey[File]("Generating version properties file.")
  lazy val generateBuildCharacterPropertiesFile = taskKey[File]("Generating buildcharacter.properties file.")

  lazy val globalVersionSettings = Seq[Setting[_]](
    // Set the version properties globally (they are the same for all projects)
    versionProperties in Global := versionPropertiesImpl.value,
    version in Global := versionProperties.value.mavenVersion
  )

  lazy val generatePropertiesFileSettings = Seq[Setting[_]](
    copyrightString := "Copyright 2002-2017, LAMP/EPFL and Lightbend, Inc.",
    resourceGenerators in Compile += generateVersionPropertiesFile.map(file => Seq(file)).taskValue,
    generateVersionPropertiesFile := generateVersionPropertiesFileImpl.value
  )

  lazy val generateBuildCharacterFileSettings = Seq[Setting[_]](
    generateBuildCharacterPropertiesFile := generateBuildCharacterPropertiesFileImpl.value
  )

  case class Versions(canonicalVersion: String, mavenVersion: String, osgiVersion: String, commitSha: String, commitDate: String, isRelease: Boolean) {
    val githubTree =
      if(isRelease) "v" + mavenVersion
      else if(commitSha != "unknown") commitSha
      else "master"

    override def toString = s"Canonical: $canonicalVersion, Maven: $mavenVersion, OSGi: $osgiVersion, github: $githubTree"

    def toMap: Map[String, String] = Map(
      "version.number" -> canonicalVersion,
      "maven.version.number" -> mavenVersion,
      "osgi.version.number" -> osgiVersion
    )
  }

  /** Compute the canonical, Maven and OSGi version number from `baseVersion` and `baseVersionSuffix`.
    * Examples of the generated versions:
    *
    * ("2.11.8", "SNAPSHOT"    ) -> ("2.11.8-20151215-133023-7559aed", "2.11.8-bin-SNAPSHOT",         "2.11.8.v20151215-133023-7559aed")
    * ("2.11.8", "SHA-SNAPSHOT") -> ("2.11.8-20151215-133023-7559aed", "2.11.8-bin-7559aed-SNAPSHOT", "2.11.8.v20151215-133023-7559aed")
    * ("2.11.8", "SHA"         ) -> ("2.11.8-7559aed",                 "2.11.8-bin-7559aed",          "2.11.8.v20151215-133023-7559aed")
    * ("2.11.0", "SHA"         ) -> ("2.11.0-7559aed",                 "2.11.0-pre-7559aed",          "2.11.0.v20151215-133023-7559aed")
    * ("2.11.8", ""            ) -> ("2.11.8",                         "2.11.8",                      "2.11.8.v20151215-133023-VFINAL-7559aed")
    * ("2.11.8", "M3"          ) -> ("2.11.8-M3",                      "2.11.8-M3",                   "2.11.8.v20151215-133023-M3-7559aed")
    * ("2.11.8", "RC4"         ) -> ("2.11.8-RC4",                     "2.11.8-RC4",                  "2.11.8.v20151215-133023-RC4-7559aed")
    * ("2.11.8-RC4", "SPLIT"   ) -> ("2.11.8-RC4",                     "2.11.8-RC4",                  "2.11.8.v20151215-133023-RC4-7559aed")
    *
    * A `baseVersionSuffix` of "SNAPSHOT" is the default, which is used for local snapshot builds. The PR validation
    * job uses "SHA-SNAPSHOT". A proper version number for an integration build can be computed with "SHA". An empty
    * suffix is used for releases. All other suffix values are treated as RC / milestone builds. The special suffix
    * value "SPLIT" is used to split the real suffix off from `baseVersion` instead and then apply the usual logic. */
  private lazy val versionPropertiesImpl: Def.Initialize[Versions] = Def.setting {

    val (base, suffix) = {
      val (b, s) = (baseVersion.value, baseVersionSuffix.value)
      if(s == "SPLIT") {
        val split = """([\w+\.]+)(-[\w+\.]+)??""".r
        val split(b2, sOrNull) = b
        (b2, Option(sOrNull).map(_.drop(1)).getOrElse(""))
      } else (b, s)
    }

    def executeTool(tool: String) = {
      val cmd =
        if (System.getProperty("os.name").toLowerCase.contains("windows"))
          s"cmd.exe /c tools\\$tool.bat -p"
        else s"tools/$tool"
      Process(cmd).lines.head
    }

    val date = executeTool("get-scala-commit-date")
    val sha = executeTool("get-scala-commit-sha").substring(0, 7) // The script produces 10 digits at the moment

    val Patch = """\d+\.\d+\.(\d+)""".r
    def cross = base match {
      case Patch(p) if p.toInt > 0 => "bin"
      case _ => "pre"
    }

    val (canonicalV, mavenV, osgiV, release) = suffix match {
      case "SNAPSHOT"     => (s"$base-$date-$sha", s"$base-$cross-SNAPSHOT",      s"$base.v$date-$sha",         false)
      case "SHA-SNAPSHOT" => (s"$base-$date-$sha", s"$base-$cross-$sha-SNAPSHOT", s"$base.v$date-$sha",         false)
      case "SHA"          => (s"$base-$sha",       s"$base-$cross-$sha",          s"$base.v$date-$sha",         false)
      case ""             => (s"$base",            s"$base",                      s"$base.v$date-VFINAL-$sha",  true)
      case suffix         => (s"$base-$suffix",    s"$base-$suffix",              s"$base.v$date-$suffix-$sha", true)
    }

    Versions(canonicalV, mavenV, osgiV, sha, date, release)
  }

  private lazy val generateVersionPropertiesFileImpl: Def.Initialize[Task[File]] = Def.task {
    writeProps(versionProperties.value.toMap + ("copyright.string" -> copyrightString.value),
      (resourceManaged in Compile).value / s"${thisProject.value.id}.properties")
  }

  private lazy val generateBuildCharacterPropertiesFileImpl: Def.Initialize[Task[File]] = Def.task {
    writeProps(versionProperties.value.toMap, (baseDirectory in ThisBuild).value / "buildcharacter.properties")
  }

  private def writeProps(m: Map[String, String], propFile: File): File = {
    val props = new Properties
    m.foreach { case (k, v) => props.put(k, v) }
    // unfortunately, this will write properties in arbitrary order
    // this makes it harder to test for stability of generated artifacts
    // consider using https://github.com/etiennestuder/java-ordered-properties
    // instead of java.util.Properties
    IO.write(props, null, propFile)
    propFile
  }

  /** The global versions.properties data */
  lazy val versionProps: Map[String, String] = {
    val props = new Properties()
    val in = new FileInputStream(file("versions.properties"))
    try props.load(in)
    finally in.close()
    props.asScala.toMap.map {
      case (k, v) => (k, sys.props.getOrElse(k, v)) // allow system properties to override versions.properties
    }
  }

  /** Get a subproject version number from `versionProps` */
  def versionNumber(name: String): String =
    versionProps(s"$name.version.number")

  /** Build a dependency to a Scala module with the given group and artifact ID */
  def scalaDep(group: String, artifact: String, versionProp: String = null, scope: String = null, compatibility: String = "binary") = {
    val vp = if(versionProp eq null) artifact else versionProp
    val m = group % (artifact + "_" + versionProps(s"scala.$compatibility.version")) % versionNumber(vp)
    val m2 = if(scope eq null) m else m % scope
    // exclusion of the scala-library transitive dependency avoids eviction warnings during `update`:
    m2.exclude("org.scala-lang", "*")
  }

  private def bootstrapOrganization(path: String) =
    "org.scala-lang.scala-sha-bootstrap." + path.replace('/', '.')

  /** Build a dependency to a JAR file in the bootstrap repository */
  def bootstrapDep(baseDir: File, path: String, libName: String): ModuleID = {
    val sha = IO.read(baseDir / path / s"$libName.jar.desired.sha1").split(' ')(0)
    bootstrapOrganization(path) % libName % sha from
      s"https://dl.bintray.com/typesafe/scala-sha-bootstrap/org/scala-lang/bootstrap/$sha/$path/$libName.jar"
  }

  /** Copy a boostrap dependency JAR that is on the classpath to a file */
  def copyBootstrapJar(cp: Seq[Attributed[File]], baseDir: File, path: String, libName: String): Unit = {
    val org = bootstrapOrganization(path)
    val resolved = cp.find { a =>
      val mod = a.get(moduleID.key)
      mod.map(_.organization) == Some(org) && mod.map(_.name) == Some(libName)
    }.map(_.data).get
    IO.copyFile(resolved, baseDir / path / s"$libName.jar")
  }
}
