import sbt._
import Keys._
import java.util.Properties
import scala.util.control.Exception.catching
import java.lang.{NumberFormatException => NFE}
import java.io.FileInputStream
import com.jsuereth.git.GitRunner
import com.jsuereth.git.GitKeys.gitRunner

case class VersionInfo(canonical: String,
                       maven: String,
                       osgi: String)

/** this file is responsible for setting up Scala versioning schemes and updating all the necessary bits. */
object Versions {
  val buildNumberFile = SettingKey[File]("scala-build-number-file")
  // TODO - Make this a setting?
  val buildNumberProps = SettingKey[BaseBuildNumber]("scala-build-number-props")
  val buildRelease = SettingKey[Boolean]("scala-build-release", "This is set to true if we're building a release.")
  val mavenSuffix = SettingKey[String]("scala-maven-suffix", "This is set to whatever maven suffix is required.")

  val gitSha = TaskKey[String]("scala-git-sha", "The sha of the current git commit.")
  val gitDate = TaskKey[String]("scala-git-date", "The date of the current git commit.")

  val mavenVersion = SettingKey[String]("scala-maven-version", "The maven version number.")
  val osgiVersion = TaskKey[String]("scala-osgi-version", "The OSGi version number.")
  val canonicalVersion = TaskKey[String]("scala-canonical-version", "The canonical version number.")

  val scalaVersions = TaskKey[VersionInfo]("scala-version-info", "The scala versions used for this build.")
  

  
  def settings: Seq[Setting[_]] = Seq(
    buildNumberFile <<= baseDirectory apply (_ / "build.number"),
    buildNumberProps <<= buildNumberFile apply loadBuildNumberProps,
    buildRelease := Option(System.getProperty("build.release")) map (!_.isEmpty) getOrElse false,
    mavenSuffix <<= buildRelease apply pickMavenSuffix,
    mavenVersion <<= (buildNumberProps, mavenSuffix) apply makeMavenVersion,
    gitSha <<= (gitRunner, baseDirectory, streams) map getGitSha,
    gitDate <<= (gitRunner, baseDirectory, streams) map getGitDate,
    osgiVersion <<= (buildNumberProps, gitDate, gitSha) map makeOsgiVersion,
    canonicalVersion <<= (buildRelease, mavenVersion, buildNumberProps, gitDate, gitSha) map makeCanonicalVersion,
    scalaVersions <<= (canonicalVersion, mavenVersion, osgiVersion) map VersionInfo.apply
  )


  /** This generates a  properties file, if it does not already exist, with the maximum lastmodified timestamp
    * of any source file. */
  def generateVersionPropertiesFile(name: String)(dir: File, versions: VersionInfo, skip: Boolean, s: TaskStreams): Seq[File] = {
    // TODO - We can probably clean this up by moving caching bits elsewhere perhaps....
    val target = dir / name        
    // TODO - Regenerate on triggers, like recompilation or something...
    def hasSameVersion: Boolean = {
      val props = new java.util.Properties
      val in = new java.io.FileInputStream(target)
      try props.load(in) finally in.close()
      versions.canonical == (props getProperty "version.number")
    }
    if (!target.exists || !(skip || hasSameVersion)) {
      makeVersionPropertiesFile(target, versions)
    }
    target :: Nil
  }
  
  // This creates the *.properties file used to determine the current version of scala at runtime.  TODO - move these somewhere utility like.
  def makeVersionPropertiesFile(f: File, versions: VersionInfo): Unit =
    IO.write(f, "version.number = "+versions.canonical+"\n"+
                "osgi.number = "+versions.osgi+"\n"+
                "maven.number = "+versions.maven+"\n"+
                "copyright.string = Copyright 2002-2011, LAMP/EPFL")

  def makeCanonicalVersion(isRelease: Boolean, mvnVersion: String, base: BaseBuildNumber, gitDate: String, gitSha: String): String =
    if(isRelease) mvnVersion
    else {
      val suffix = if(base.bnum > 0) "-%d".format(base.bnum) else ""
      "%s.%s.%s%s-%s-%s" format (base.major, base.minor, base.patch, suffix, gitDate, gitSha)
    }

  def makeMavenVersion(base: BaseBuildNumber, suffix: String): String = {
    val firstSuffix = if(base.bnum > 0) "-%d".format(base.bnum) else ""
    "%d.%d.%d%s%s" format (base.major, base.minor, base.patch, firstSuffix, suffix)
  }

  def makeOsgiVersion(base: BaseBuildNumber, gitDate: String, gitSha: String): String = {
    val suffix = if(base.bnum > 0) "-%d".format(base.bnum) else ""
    "%s.%s.%s.v%s%s-%s" format (base.major, base.minor, base.patch, gitDate, suffix, gitSha)
  }

  /** Determines what the maven sufffix should be for this build. */
  def pickMavenSuffix(isRelease: Boolean): String = {
    def default = if(isRelease) "" else "-SNAPSHOT"
    Option(System.getProperty("maven.version.suffix")) getOrElse default
  }

  /** Loads the build.number properties file into SBT. */
  def loadBuildNumberProps(file: File): BaseBuildNumber = {
    val fin = new FileInputStream(file)
    try {
      val props = new Properties()
      props.load(fin)
      def getProp(name: String): Int = 
        (for {
          v <- Option(props.getProperty(name))
          v2 <- catching(classOf[NFE]) opt v.toInt
        } yield v2) getOrElse sys.error("Could not convert %s to integer!" format (name))

      BaseBuildNumber(
        major=getProp("version.major"), 
        minor=getProp("version.minor"),
        patch=getProp("version.patch"),
        bnum =getProp("version.bnum")
      )
    } finally fin.close()
  }


  def getGitDate(git: GitRunner, baseDirectory: File, s: TaskStreams): String = {
    val lines = getGitLines("log","-1","--format=\"%ci\"")(git,baseDirectory, s)
    val line = if(lines.isEmpty) sys.error("Could not retreive git commit sha!") else lines.head
    // Lines *always* start with " for some reason...
    line drop 1 split "\\s+" match {
      case Array(date, time, _*) =>  "%s-%s" format (date.replaceAll("\\-", ""), time.replaceAll(":",""))
      case _                     => sys.error("Could not parse git date: " + line)
    }
  }

  def getGitSha(git: GitRunner, baseDirectory: File, s: TaskStreams): String = {
    val lines = getGitLines("log","-1","--format=\"%H\"", "HEAD")(git,baseDirectory, s)
    val line = if(lines.isEmpty) sys.error("Could not retreive git commit sha!") else lines.head
    val noquote = if(line startsWith "\"") line drop 1 else line
    val nog = if(noquote startsWith "g") noquote drop 1 else noquote
    nog take 10
  }

  def getGitLines(args: String*)(git: GitRunner, baseDirectory: File, s: TaskStreams): Seq[String] =
     git(args: _*)(baseDirectory, s.log) split "[\r\n]+"
}


case class BaseBuildNumber(major: Int, minor: Int, patch: Int, bnum: Int) {
  override def toString = "BaseBuildNumber(%d.%d.%d-%d)" format (major, minor, patch, bnum)
}
