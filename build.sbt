/*
 * The new, sbt-based build definition for Scala.
 *
 * What you see below is very much work-in-progress. Basics like compiling and packaging jars
 * (into right location) work. Everything else is missing:
 *    building docs, placing shell scripts in right locations (so you can run compiler easily),
 *    running partest test, compiling and running JUnit test, and many, many other things.
 *
 * You'll notice that this build definition is much more complicated than your typical sbt build.
 * The main reason is that we are not benefiting from sbt's conventions when it comes project
 * layout. For that reason we have to configure a lot more explicitly. I've tried explain in
 * comments the less obvious settings.
 *
 * This nicely leads me to explaning goal and non-goals of this build definition. Goals are:
 *
 *   - to be easy to tweak it in case a bug or small inconsistency is found
 *   - to mimic Ant's behavior as closely as possible
 *   - to be super explicit about any departure from standard sbt settings
 *   - to achieve functional parity with Ant build as quickly as possible
 *   - to be readable and not necessarily succint
 *   - to provide the nicest development experience for people hacking on Scala
 *
 * Non-goals are:
 *
 *   - to have the shortest sbt build definition possible; we'll beat Ant definition
 *     easily and that will thrill us already
 *   - to remove irregularities from our build process right away
 *   - to modularize the Scala compiler or library further
 *
 * It boils down to simple rules:
 *
 *   - project laytout is set in stone for now
 *   - if you need to work on convincing sbt to follow non-standard layout then
 *     explain everything you did in comments
 *   - constantly check where Ant build produces class files, artifacts, what kind of other
 *     files generates and port all of that to here
 *
 * Note on bootstrapping:
 *
 *   Let's start with reminder what bootstrapping means in our context. It's an answer
 *   to this question: which version of Scala are using to compile Scala? The fact that
 *   the question sounds circular suggests trickiness. Indeed, bootstrapping Scala
 *   compiler is a tricky process.
 *
 *   Ant build used to have involved system of bootstrapping Scala. It would consist of
 *   three layers: starr, locker and quick. The sbt build for Scala ditches layering
 *   and strives to be as standard sbt project as possible. This means that we are simply
 *   building Scala with latest stable release of Scala.
 *   See this discussion for more details behind this decision:
 *     https://groups.google.com/d/topic/scala-internals/gp5JsM1E0Fo/discussion
 */

lazy val commonSettings = Seq[Setting[_]](
  organization := "org.scala-lang",
  version := "2.11.6-SNAPSHOT",
  scalaVersion := "2.11.5",
  // we don't cross build Scala itself
  crossPaths := false,
  // do not add Scala library jar as a dependency automatically
  autoScalaLibrary := false,
  // we always assume that Java classes are standalone and do not have any dependency
  // on Scala classes
  compileOrder := CompileOrder.JavaThenScala,
  // we don't want any unmanaged jars; as a reminder: unmanaged jar is a jar stored
  // directly on the file system and it's not resolved through Ivy
  // Ant's build stored unmanaged jars in `lib/` directory
  unmanagedJars in Compile := Seq.empty,
  sourceDirectory in Compile := baseDirectory.value,
  sourceDirectories in Compile := Seq(sourceDirectory.value),
  scalaSource in Compile := (sourceDirectory in Compile).value,
  javaSource in Compile := (sourceDirectory in Compile).value,
  target := (baseDirectory in ThisBuild).value / "target" / name.value,
  classDirectory in Compile := buildDirectory.value / "quick/classes" / name.value,
  artifactPath in packageBin in Compile := {
    // two lines below are copied over from sbt's sources:
    // https://github.com/sbt/sbt/blob/0.13/main/src/main/scala/sbt/Defaults.scala#L628
    //val resolvedScalaVersion = ScalaVersion((scalaVersion in artifactName).value, (scalaBinaryVersion in artifactName).value)
    //val resolvedArtifactName = artifactName.value(resolvedScalaVersion, projectID.value, artifact.value)
    // if you would like to get a jar with version number embedded in it (as normally sbt does)
    // uncomment the other definition of the `resolvedArtifactName`
    val resolvedArtifact = artifact.value
    val resolvedArtifactName = s"${resolvedArtifact.name}.${resolvedArtifact.extension}"
    buildDirectory.value / "pack/lib" / resolvedArtifactName
  },
  // given that classDirectory is overriden to be _outside_ of target directory, we have
  // to make sure its being cleaned properly
  cleanFiles += (classDirectory in Compile).value,
  copyrightString := "Copyright 2002-2013, LAMP/EPFL",
  resourceGenerators in Compile += generateVersionPropertiesFile.map(file => Seq(file)).taskValue,
  generateVersionPropertiesFile := generateVersionPropertiesFileImpl.value
)

lazy val library = configureAsSubproject(project).
  settings(
    scalacOptions ++= Seq[String]("-sourcepath", (scalaSource in Compile).value.toString)
  ) dependsOn (forkjoin)

lazy val reflect = configureAsSubproject(project).
  dependsOn(library)

lazy val compiler = configureAsSubproject(project).
  settings(libraryDependencies += "org.apache.ant" % "ant" % "1.9.4").
  dependsOn(library, reflect, asm)

lazy val interactive = configureAsSubproject(project).
  dependsOn(compiler)

lazy val repl = configureAsSubproject(project).
  // TODO: in Ant build def, this version is defined in versions.properties
  // figure out whether we also want to externalize jline's version
  settings(libraryDependencies += "jline" % "jline" % "2.12").
  dependsOn(compiler)

lazy val scaladoc = configureAsSubproject(project).
  settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml" % "1.0.3",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
      "org.scala-lang.modules" %% "scala-partest" % "1.0.5"
    )
  ).
  dependsOn(compiler)

lazy val scalap = configureAsSubproject(project).
  dependsOn(compiler)

// deprecated Scala Actors project
// TODO: it packages into actors.jar but it should be scala-actors.jar
lazy val actors = configureAsSubproject(project).
  dependsOn(library)

lazy val forkjoin = configureAsSubproject(project)

lazy val asm = configureAsSubproject(project)

lazy val root = (project in file(".")).
  aggregate(library, forkjoin, reflect, compiler, asm, interactive, repl,
    scaladoc, scalap, actors).
  // make the root project an aggragate-only
  // we disable sbt's built-in Ivy plugin in the root project
  // so it doesn't produce any artifact including not building
  // an empty jar
  disablePlugins(plugins.IvyPlugin)

/**
 * Configures passed project as a subproject (e.g. compiler or repl)
 * with common settings attached to it.
 *
 * Typical usage is:
 *
 *   lazy val mySubproject = configureAsSubproject(project)
 *
 * We pass `project` as an argument which is in fact a macro call. This macro determines
 * project.id based on the name of the lazy val on the left-hand side.
 */
def configureAsSubproject(project: Project): Project = {
  val base = file(".") / "src" / project.id
  (project in base).settings(commonSettings: _*)
}

lazy val buildDirectory = settingKey[File]("The directory where all build products go. By default ./build")
lazy val copyrightString = settingKey[String]("Copyright string.")
lazy val generateVersionPropertiesFile = taskKey[File]("Generating version properties file.")

lazy val generateVersionPropertiesFileImpl: Def.Initialize[Task[File]] = Def.task {
  val propFile = (resourceManaged in Compile).value / s"${name.value}.properties"
  val props = new java.util.Properties

  /**
   * Regexp that splits version number split into two parts: version and suffix.
   * Examples of how the split is performed:
   *
   *  "2.11.5": ("2.11.5", null)
   *  "2.11.5-acda7a": ("2.11.5", "-acda7a")
   *  "2.11.5-SNAPSHOT": ("2.11.5", "-SNAPSHOT")
   *
   */
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

  props.put("version.number", s"${version.value}-$commitDate-$commitSha")
  props.put("maven.version.number", s"${version.value}")
  props.put("osgi.version.number", s"$ver.v$commitDate$osgiSuffix-$commitSha")
  props.put("copyright.string", copyrightString.value)

  // unfortunately, this will write properties in arbitrary order
  // this makes it harder to test for stability of generated artifacts
  // consider using https://github.com/etiennestuder/java-ordered-properties
  // instead of java.util.Properties
  IO.write(props, null, propFile)

  propFile
}

buildDirectory in ThisBuild := (baseDirectory in ThisBuild).value / "build-sbt"
