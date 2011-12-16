import sbt._
import Keys._
import _root_.com.jsuereth.git.GitRunner

object Release {

  // TODO - move more of the dist project over here...


  lazy val pushStarr = Command.command("push-starr") { (state: State) =>
      def f(s: Setting[_]): Setting[_] = s.key.key match {
        case version.key => // TODO - use full version
          s.asInstanceOf[Setting[String]].mapInit( (_,_) => timeFormat format (new java.util.Date))
        case organization.key =>
          s.asInstanceOf[Setting[String]].mapInit( (_,_) => "org.scala-lang.bootstrapp")
        // TODO - Switch publish repo to be typesafe starr repo.
        case publishTo.key =>
          s.asInstanceOf[Setting[Option[Resolver]]].mapInit((_,_) => Some("Starr Repo" at "http://typesafe.artifactoryonline.com/typesafe/starr-releases/"))
        case _ => s
      }
      val extracted = Project.extract(state)
      import extracted._
      // Swap version on projects
      val transformed = session.mergeSettings map ( s => f(s) )
      val newStructure = Load.reapply(transformed, structure)
      val newState = Project.setProject(session, newStructure, state)
      // TODO - Run tasks.  Specifically, push scala-compiler + scala-library.  *Then* bump the STARR version locally.
      // The final course of this command should be:
      // publish-local
      // Project.evaluateTask(publishLocal, newState)
      // bump STARR version setting
      // TODO - Define Task
      // Rebuild quick + test to ensure it works
      // Project.evaluateTask(test, newState)
      // push STARR remotely
      Project.evaluateTask(publish, newState)
      // Revert to previous project state.
      Project.setProject(session, structure, state)
   }

  // TODO - Autocomplete
  /*lazy val setStarrHome = Command.single("set-starr-home") { (state: State, homeDir: String) =>
    def f(s: Setting[_]): Setting[_] = 
      if(s.key.key == scalaInstance.key) {
        s.asInstanceOf[Setting[ScalaInstance]] mapInit { (key, value) =>
           if(value.version == "starr")  
             scalaInstance <<= appConfiguration map { app =>
               val launcher = app.provider.scalaProvider.launcher
               ScalaInstance("starr", new File(homeDir), launcher)
             }
           else value
        }
      } else s
    val extracted = Project.extract(state)
    import extracted._
    val transformed = session.mergeSettings map f
    val newStructure = Load.reapply(transformed, structure)
    Project.setProject(session, newStructure, state)
  }*/

  lazy val timeFormat = {
    val formatter = new java.text.SimpleDateFormat("yyyyMMdd'T'HHmmss")
    formatter.setTimeZone(java.util.TimeZone.getTimeZone("GMT"))
    formatter
  }

  /** This generates a  properties file, if it does not already exist, with the maximum lastmodified timestamp
    * of any source file. */
  def generatePropertiesFile(name: String)(baseDirectory: File, version: String, dir: File, git: GitRunner, s: TaskStreams): Seq[File] = {
    // TODO - We can probably clean this up by moving caching bits elsewhere perhaps....
    val target = dir / name        
    // TODO - Regenerate on triggers, like recompilation or something...
    val fullVersion = makeFullVersionString(baseDirectory, version, git, s)
    def hasSameVersion: Boolean = {
      val props = new java.util.Properties
      val in = new java.io.FileInputStream(target)
      try props.load(in) finally in.close()
      def withoutDate(s: String): String = s.reverse.dropWhile (_ != '.').reverse
      withoutDate(fullVersion) == withoutDate(props getProperty "version.number")
    }
    if (!target.exists || !hasSameVersion) {
      makePropertiesFile(target, fullVersion)
    }
    target :: Nil
  }
  
  // This creates the *.properties file used to determine the current version of scala at runtime.  TODO - move these somewhere utility like.
  def makePropertiesFile(f: File, version: String): Unit =
    IO.write(f, "version.number = "+version+"\ncopyright.string = Copyright 2002-2011, LAMP/EPFL")

  def makeFullVersionString(baseDirectory: File, baseVersion: String, git: GitRunner, s: TaskStreams) = baseVersion+"."+getGitRevision(baseDirectory, git, currentDay, s)

  // TODO - do we want this in the build number?
  def currentDay = (new java.text.SimpleDateFormat("yyyyMMdd'T'HHmmss")) format (new java.util.Date)



  def getGitRevision(baseDirectory: File, git: GitRunner, date: String, s: TaskStreams) = {

    val mergeBase = {
      // TODO - Cache this value.
      // git("merge-base","v2.8.2","v2.9.1","master")(baseDirectory, s.log)
      "df13e31bbb"
    }
    // current commit sha
    val sha =
     git("rev-list", "-n", "1", "HEAD")(baseDirectory, s.log)
    
    val commits =
     git("--no-pager", "log", "--pretty=oneline", mergeBase +"..HEAD")(baseDirectory, s.log) split "[\r\n]+" size
    
    "rdev-%d-%s-g%s" format (commits, date, sha.substring(0,7))
  }
  
}
