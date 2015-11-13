import sbt._
import Keys._
import java.util.Properties
import java.io.FileInputStream
import scala.collection.JavaConverters._

object VersionUtil {
  lazy val copyrightString = settingKey[String]("Copyright string.")
  lazy val versionProperties = settingKey[Versions]("Version properties.")
  lazy val generateVersionPropertiesFile = taskKey[File]("Generating version properties file.")

  lazy val versionPropertiesSettings = Seq[Setting[_]](
    versionProperties := versionPropertiesImpl.value
  )

  lazy val generatePropertiesFileSettings = Seq[Setting[_]](
    copyrightString := "Copyright 2002-2015, LAMP/EPFL",
    resourceGenerators in Compile += generateVersionPropertiesFile.map(file => Seq(file)).taskValue,
    versionProperties := versionPropertiesImpl.value,
    generateVersionPropertiesFile := generateVersionPropertiesFileImpl.value
  )

  case class Versions(canonicalVersion: String, mavenVersion: String, osgiVersion: String, commitSha: String, commitDate: String, isRelease: Boolean) {
    val githubTree =
      if(isRelease) "v" + mavenVersion
      else if(commitSha != "unknown") commitSha
      else "master"

    override def toString = s"Canonical: $canonicalVersion, Maven: $mavenVersion, OSGi: $osgiVersion, github: $githubTree"

    def toProperties: Properties = {
      val props = new Properties
      props.put("version.number", canonicalVersion)
      props.put("maven.version.number", mavenVersion)
      props.put("osgi.version.number", osgiVersion)
      props
    }
  }

  lazy val versionPropertiesImpl: Def.Initialize[Versions] = Def.setting {
    /** Regexp that splits version number split into two parts: version and suffix.
      * Examples of how the split is performed:
      *
      *  "2.11.5": ("2.11.5", null)
      *  "2.11.5-acda7a": ("2.11.5", "-acda7a")
      *  "2.11.5-SNAPSHOT": ("2.11.5", "-SNAPSHOT") */
    val versionSplitted = """([\w+\.]+)(-[\w+\.]+)??""".r

    val versionSplitted(ver, suffixOrNull) = version.value

    val osgiSuffix = suffixOrNull match {
      case null => "-VFINAL"
      case "-SNAPSHOT" => ""
      case suffixStr => suffixStr
    }

    def executeTool(tool: String) = {
      val cmd =
        if (System.getProperty("os.name").toLowerCase.contains("windows"))
          s"cmd.exe /c tools\\$tool.bat -p"
        else s"tools/$tool"
      Process(cmd).lines.head
    }

    val commitDate = executeTool("get-scala-commit-date")
    val commitSha = executeTool("get-scala-commit-sha")

    Versions(
      canonicalVersion = s"$ver-$commitDate-$commitSha",
      mavenVersion = s"${version.value}",
      osgiVersion = s"$ver.v$commitDate$osgiSuffix-$commitSha",
      commitSha = commitSha,
      commitDate = commitDate,
      isRelease = !osgiSuffix.isEmpty
    )
  }

  lazy val generateVersionPropertiesFileImpl: Def.Initialize[Task[File]] = Def.task {
    val props = versionProperties.value.toProperties
    val propFile = (resourceManaged in Compile).value / s"${thisProject.value.id}.properties"
    props.put("copyright.string", copyrightString.value)

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
    props.asScala.toMap
  }

  /** Get a subproject version number from `versionProps` */
  def versionNumber(name: String): String =
    versionProps(s"$name.version.number")
}
