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
  // all project will have baseDirectory set to root folder; we shouldn't include
  // any source from it in any project
  sourcesInBase := false,
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
  // set baseDirectory to the root folder in all projects
  baseDirectory := (baseDirectory in ThisBuild).value,
  sourceDirectory in Compile := baseDirectory.value / "src" / name.value,
  sourceDirectories in Compile := Seq(sourceDirectory.value),
  scalaSource in Compile := (sourceDirectory in Compile).value,
  javaSource in Compile := (sourceDirectory in Compile).value,
  target := baseDirectory.value / "target" / name.value,
  classDirectory in Compile := baseDirectory.value / "build/quick/classes" / name.value,
  artifactPath in packageBin in Compile := {
    // two lines below are copied over from sbt's sources:
    // https://github.com/sbt/sbt/blob/0.13/main/src/main/scala/sbt/Defaults.scala#L628
    //val resolvedScalaVersion = ScalaVersion((scalaVersion in artifactName).value, (scalaBinaryVersion in artifactName).value)
    //val resolvedArtifactName = artifactName.value(resolvedScalaVersion, projectID.value, artifact.value)
    // if you would like to get a jar with version number embedded in it (as normally sbt does)
    // uncomment the other definition of the `resolvedArtifactName`
    val resolvedArtifact = artifact.value
    val resolvedArtifactName = s"${resolvedArtifact.name}.${resolvedArtifact.extension}"
    baseDirectory.value / "build/pack/lib" / resolvedArtifactName
  },
  // given that classDirectory is overriden to be _outside_ of target directory, we have
  // to make sure its being cleaned properly
  cleanFiles += (classDirectory in Compile).value
)

lazy val library = project.
  settings(commonSettings: _*).
  settings(
    scalacOptions ++= Seq[String]("-sourcepath", (scalaSource in Compile).value.toString)
  ) dependsOn (forkjoin)

lazy val reflect = project.
  settings(commonSettings: _*).
  dependsOn(library)

lazy val compiler = project.
  settings(commonSettings: _*).
  settings(libraryDependencies += "org.apache.ant" % "ant" % "1.9.4").
  dependsOn(library, reflect, asm)

lazy val interactive = project.
  settings(commonSettings: _*).
  dependsOn(compiler)

lazy val repl = project.
  settings(commonSettings: _*).
  // TODO: in Ant build def, this version is defined in versions.properties
  // figure out whether we also want to externalize jline's version
  settings(libraryDependencies += "jline" % "jline" % "2.12").
  dependsOn(compiler)

lazy val scaladoc = project.
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml" % "1.0.3",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
      "org.scala-lang.modules" %% "scala-partest" % "1.0.5"
    )
  ).
  dependsOn(compiler)

lazy val scalap = project.
  settings(commonSettings: _*).
  dependsOn(compiler)

// deprecated Scala Actors project
// TODO: it packages into actors.jar but it should be scala-actors.jar
lazy val actors = project.
  settings(commonSettings: _*).
  dependsOn(library)

lazy val forkjoin = project.
  settings(commonSettings: _*)

lazy val asm = project.
  settings(commonSettings: _*)

lazy val root = (project in file(".")).
  aggregate(library, forkjoin, reflect, compiler, asm, interactive, repl,
    scaladoc, scalap, actors).
  // make the root project an aggragate-only
  // we disable sbt's built-in Ivy plugin in the root project
  // so it doesn't produce any artifact including not building
  // an empty jar
  disablePlugins(plugins.IvyPlugin)
