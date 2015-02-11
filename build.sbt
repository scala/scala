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
    val resolvedScalaVersion = ScalaVersion((scalaVersion in artifactName).value, (scalaBinaryVersion in artifactName).value)
    val resolvedArtifactName = artifactName.value(resolvedScalaVersion, projectID.value, artifact.value)
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

lazy val scala = (project in file(".")).
  aggregate(library, forkjoin, reflect, compiler, asm, interactive, repl,
    scaladoc, scalap).
  // make the root project an aggragate-only
  // we disable sbt's built-in Ivy plugin in the root project
  // so it doesn't produce any artifact including not building
  // an empty jar
  disablePlugins(plugins.IvyPlugin)
