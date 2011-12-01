import sbt._
import Keys._

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
  def generatePropertiesFile(name: String)(baseDirectory: File, version: String, dir: File): Seq[File] = {
    val target = dir / name
    // TODO - Regenerate on triggers, like recompilation or something...
    // TODO - also think about pulling git last-commit for this...
    if (!target.exists) {
      val ts = getLastModified(baseDirectory)
      val formatter = new java.text.SimpleDateFormat("yyyyMMdd'T'HHmmss")
      formatter.setTimeZone(java.util.TimeZone.getTimeZone("GMT"))
      val fullVersion = version + "." + formatter.format(new java.util.Date(ts))
      makePropertiesFile(target, fullVersion)
    }
    target :: Nil
  }
  
  // This creates the *.properties file used to determine the current version of scala at runtime.  TODO - move these somewhere utility like.
  def makePropertiesFile(f: File, version: String): Unit =
    IO.write(f, "version.number = "+version+"\ncopyright.string = Copyright 2002-2011, LAMP/EPFL")

  def makeFullVersionString(baseDirectory: File, baseVersion: String) = baseVersion+"."+getLastModified(baseDirectory)

  // TODO - Something that doesn't take so long...
  def allSourceFiles(baseDirectory: File) = (baseDirectory / "src") ** ("*.scala" | "*.java" )

  def getLastModified(baseDirectory: File) =
    allSourceFiles(baseDirectory).get.map(_.lastModified).max
  
}
