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
 * This nicely leads me to explaining goal and non-goals of this build definition. Goals are:
 *
 *   - to be easy to tweak it in case a bug or small inconsistency is found
 *   - to mimic Ant's behavior as closely as possible
 *   - to be super explicit about any departure from standard sbt settings
 *   - to achieve functional parity with Ant build as quickly as possible
 *   - to be readable and not necessarily succinct
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
 *   - project layout is set in stone for now
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

val bootstrapScalaVersion = "2.11.5"

def withoutScalaLang(moduleId: ModuleID): ModuleID = moduleId exclude("org.scala-lang", "*")

// exclusion of the scala-library transitive dependency avoids eviction warnings during `update`.
val scalaParserCombinatorsDep = withoutScalaLang("org.scala-lang.modules" %% "scala-parser-combinators" % versionNumber("scala-parser-combinators"))
val scalaXmlDep = withoutScalaLang("org.scala-lang.modules" %% "scala-xml" % versionNumber("scala-xml"))
val partestDep = withoutScalaLang("org.scala-lang.modules" %% "scala-partest" % versionNumber("partest"))
val partestInterfaceDep = withoutScalaLang("org.scala-lang.modules" %% "scala-partest-interface" % "0.5.0")
val junitDep = "junit" % "junit" % "4.11"
val junitIntefaceDep = "com.novocode" % "junit-interface" % "0.11" % "test"
val asmDep = "org.scala-lang.modules" % "scala-asm" % versionProps("scala-asm.version")
val jlineDep = "jline" % "jline" % versionProps("jline.version")
val antDep = "org.apache.ant" % "ant" % "1.9.4"
val scalacheckDep = withoutScalaLang("org.scalacheck" %% "scalacheck" % "1.11.4")

lazy val commonSettings = clearSourceAndResourceDirectories ++ Seq[Setting[_]](
  organization := "org.scala-lang",
  version := "2.11.6-SNAPSHOT",
  scalaVersion := bootstrapScalaVersion,
  // we don't cross build Scala itself
  crossPaths := false,
  // do not add Scala library jar as a dependency automatically
  autoScalaLibrary := false,
  // we also do not add scala instance automatically because it introduces
  // a circular instance, see: https://github.com/sbt/sbt/issues/1872
  managedScalaInstance := false,
  // this is a way to workaround issue described in https://github.com/sbt/sbt/issues/1872
  // check it out for more details
  scalaInstance := ScalaInstance(scalaVersion.value, appConfiguration.value.provider.scalaProvider.launcher getScala scalaVersion.value),
  // we always assume that Java classes are standalone and do not have any dependency
  // on Scala classes
  compileOrder := CompileOrder.JavaThenScala,
  javacOptions in Compile ++= Seq("-g", "-source", "1.5", "-target", "1.6"),
  // we don't want any unmanaged jars; as a reminder: unmanaged jar is a jar stored
  // directly on the file system and it's not resolved through Ivy
  // Ant's build stored unmanaged jars in `lib/` directory
  unmanagedJars in Compile := Seq.empty,
  sourceDirectory in Compile := baseDirectory.value,
  unmanagedSourceDirectories in Compile := List(baseDirectory.value),
  scalaSource in Compile := (sourceDirectory in Compile).value,
  javaSource in Compile := (sourceDirectory in Compile).value,
  // resources are stored along source files in our current layout
  resourceDirectory in Compile := (sourceDirectory in Compile).value,
  // each subproject has to ask specifically for files they want to include
  includeFilter in unmanagedResources in Compile := NothingFilter,
  target := (baseDirectory in ThisBuild).value / "target" / thisProject.value.id,
  target in Compile in doc := buildDirectory.value / "scaladoc" / thisProject.value.id,
  classDirectory in Compile := buildDirectory.value / "quick/classes" / thisProject.value.id,
  // given that classDirectory is overriden to be _outside_ of target directory, we have
  // to make sure its being cleaned properly
  cleanFiles += (classDirectory in Compile).value,
  fork in run := true
)

// disable various tasks that are not needed for projects that are used
// only for compiling code and not publishing it as a standalone artifact
// we disable those tasks by overriding them and returning bogus files when
// needed. This is a bit sketchy but I haven't found any better way.
val disableDocsAndPublishingTasks = Seq[Setting[_]](
  doc := file("!!! NO DOCS !!!"),
  publishLocal := {},
  publish := {},
  packageBin in Compile := file("!!! NO PACKAGING !!!")
)

lazy val setJarLocation: Setting[_] = 
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
  }
lazy val scalaSubprojectSettings: Seq[Setting[_]] = commonSettings :+ setJarLocation

lazy val generatePropertiesFileSettings = Seq[Setting[_]](
  copyrightString := "Copyright 2002-2013, LAMP/EPFL",
  resourceGenerators in Compile += generateVersionPropertiesFile.map(file => Seq(file)).taskValue,
  generateVersionPropertiesFile := generateVersionPropertiesFileImpl.value
)

val libIncludes: FileFilter = "*.tmpl" | "*.xml" | "*.js" | "*.css" | "rootdoc.txt"

lazy val library = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings: _*)
  .settings(
    name := "scala-library",
    scalacOptions in Compile ++= Seq[String]("-sourcepath", (scalaSource in Compile).value.toString),
    // Workaround for a bug in `scaladoc` that it seems to not respect the `-sourcepath` option
    // as a result of this bug, the compiler cannot even initialize Definitions without
    // binaries of the library on the classpath. Specifically, we get this error:
    // (library/compile:doc) scala.reflect.internal.FatalError: package class scala does not have a member Int
    // Ant build does the same thing always: it puts binaries for documented classes on the classpath
    // sbt never does this by default (which seems like a good default)
    dependencyClasspath in Compile in doc += (classDirectory in Compile).value,
    scalacOptions in Compile in doc ++= {
      val libraryAuxDir = (baseDirectory in ThisBuild).value / "src/library-aux"
      Seq("-doc-no-compile", libraryAuxDir.toString)
    },
    includeFilter in unmanagedResources in Compile := libIncludes)
  .dependsOn (forkjoin)

lazy val reflect = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings: _*)
  .settings(name := "scala-reflect")
  .dependsOn(library)

val compilerIncludes: FileFilter =
  "*.tmpl" | "*.xml" | "*.js" | "*.css" | "*.html" | "*.properties" | "*.swf" |
  "*.png" | "*.gif" | "*.gif" | "*.txt"

lazy val compiler = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings: _*)
  .settings(
    name := "scala-compiler",
    libraryDependencies ++= Seq(antDep, asmDep),
    // this a way to make sure that classes from interactive and scaladoc projects
    // end up in compiler jar (that's what Ant build does)
    // we need to use LocalProject references (with strings) to deal with mutual recursion
    mappings in Compile in packageBin :=
      (mappings in Compile in packageBin).value ++
      dependencyClasses(
        (externalDependencyClasspath in Compile).value,
        modules = Set(asmDep),
        keep = "*.class" || "scala-asm.properties",
        streams.value.cacheDirectory) ++
      (mappings in Compile in packageBin in LocalProject("interactive")).value ++
      (mappings in Compile in packageBin in LocalProject("scaladoc")).value ++
      (mappings in Compile in packageBin in LocalProject("repl")).value,
    includeFilter in unmanagedResources in Compile := compilerIncludes)
  .dependsOn(library, reflect)

lazy val interactive = configureAsSubproject(project)
  .settings(disableDocsAndPublishingTasks: _*)
  .dependsOn(compiler)

// TODO: SI-9339 embed shaded copy of jline & its interface (see #4563)
lazy val repl = configureAsSubproject(project)
  .settings(
    libraryDependencies += jlineDep,
    connectInput in run := true,
    outputStrategy in run := Some(StdoutOutput),
    run <<= (run in Compile).partialInput(" -usejavacp") // Automatically add this so that `repl/run` works without additional arguments.
  )
  .settings(disableDocsAndPublishingTasks: _*)
  .dependsOn(compiler, interactive)

lazy val scaladoc = configureAsSubproject(project)
  .settings(
    libraryDependencies ++= Seq(scalaXmlDep, scalaParserCombinatorsDep, partestDep)
  )
  .settings(disableDocsAndPublishingTasks: _*)
  .dependsOn(compiler)

lazy val scalap = configureAsSubproject(project).
  dependsOn(compiler)

// deprecated Scala Actors project
// TODO: it packages into actors.jar but it should be scala-actors.jar
lazy val actors = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings: _*)
  .settings(name := "scala-actors")
  .dependsOn(library)

lazy val forkjoin = configureAsForkOfJavaProject(project)

lazy val partestExtras = configureAsSubproject(Project("partest-extras", file(".") / "src" / "partest-extras"))
  .dependsOn(repl)
  .settings(clearSourceAndResourceDirectories: _*)
  .settings(
    libraryDependencies += partestDep,
    unmanagedSourceDirectories in Compile := List(baseDirectory.value)
  )

lazy val junit = project.in(file("test") / "junit")
  .dependsOn(library, reflect, compiler, partestExtras, scaladoc)
  .settings(clearSourceAndResourceDirectories: _*)
  .settings(commonSettings: _*)
  .settings(
    fork in Test := true,
    libraryDependencies ++= Seq(junitDep, junitIntefaceDep),
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v"),
    unmanagedSourceDirectories in Test := List(baseDirectory.value)
  )

lazy val partestJavaAgent = (project in file(".") / "src" / "partest-javaagent").
  settings(commonSettings: _*).
  settings(
    libraryDependencies += asmDep,
    doc := file("!!! NO DOCS !!!"),
    publishLocal := {},
    publish := {},
    // Setting name to "scala-partest-javaagent" so that the jar file gets that name, which the Runner relies on
    name := "scala-partest-javaagent",
    // writing jar file to $buildDirectory/pack/lib because that's where it's expected to be found
    setJarLocation,
    // add required manifest entry - previously included from file
    packageOptions in (Compile, packageBin) +=
      Package.ManifestAttributes( "Premain-Class" -> "scala.tools.partest.javaagent.ProfilingAgent" ),
    // we need to build this to a JAR
    exportJars := true
  )

lazy val test = project.
  dependsOn(compiler, interactive, actors, repl, scalap, partestExtras, partestJavaAgent, scaladoc).
  configs(IntegrationTest).
  settings(disableDocsAndPublishingTasks: _*).
  settings(commonSettings: _*).
  settings(Defaults.itSettings: _*).
  settings(
    libraryDependencies ++= Seq(asmDep, partestDep, scalaXmlDep, partestInterfaceDep, scalacheckDep),
    unmanagedBase in Test := baseDirectory.value / "files" / "lib",
    unmanagedJars in Test <+= (unmanagedBase) (j => Attributed.blank(j)) map(identity),
    // no main sources
    sources in Compile := Seq.empty,
    // test sources are compiled in partest run, not here
    sources in IntegrationTest := Seq.empty,
    fork in IntegrationTest := true,
    javaOptions in IntegrationTest += "-Xmx1G",
    testFrameworks += new TestFramework("scala.tools.partest.Framework"),
    testOptions in IntegrationTest += Tests.Setup( () => root.base.getAbsolutePath + "/pull-binary-libs.sh" ! ),
    definedTests in IntegrationTest += (
      new sbt.TestDefinition(
        "partest",
        // marker fingerprint since there are no test classes
        // to be discovered by sbt:
        new sbt.testing.AnnotatedFingerprint {
          def isModule = true
          def annotationName = "partest"
        }, true, Array())
     )
  )

lazy val root = (project in file(".")).
  aggregate(library, forkjoin, reflect, compiler, interactive, repl,
    scaladoc, scalap, actors, partestExtras, junit).settings(
    sources in Compile := Seq.empty,
    onLoadMessage := """|*** Welcome to the sbt build definition for Scala! ***
      |This build definition has an EXPERIMENTAL status. If you are not
      |interested in testing or working on the build itself, please use
      |the Ant build definition for now. Check README.md for more information.""".stripMargin
  )

lazy val dist = (project in file("dist")).settings(
  mkBin := mkBinImpl.value
)

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
  (project in base).settings(scalaSubprojectSettings: _*)
}

/**
 * Configuration for subprojects that are forks of some Java projects
 * we depend on. At the moment there's just forkjoin.
 *
 * We do not publish artifacts for those projects but we package their
 * binaries in a jar of other project (compiler or library).
 *
 * For that reason we disable docs generation, packaging and publishing.
 */
def configureAsForkOfJavaProject(project: Project): Project = {
  val base = file(".") / "src" / project.id
  (project in base).
    settings(commonSettings: _*).
    settings(disableDocsAndPublishingTasks: _*).
    settings(
      sourceDirectory in Compile := baseDirectory.value,
      javaSource in Compile := (sourceDirectory in Compile).value,
      sources in Compile in doc := Seq.empty,
      classDirectory in Compile := buildDirectory.value / "libs/classes" / thisProject.value.id
    )
}

lazy val buildDirectory = settingKey[File]("The directory where all build products go. By default ./build")
lazy val copyrightString = settingKey[String]("Copyright string.")
lazy val generateVersionPropertiesFile = taskKey[File]("Generating version properties file.")
lazy val mkBin = taskKey[Seq[File]]("Generate shell script (bash or Windows batch).")

lazy val generateVersionPropertiesFileImpl: Def.Initialize[Task[File]] = Def.task {
  val propFile = (resourceManaged in Compile).value / s"${thisProject.value.id}.properties"
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

/**
 * Extract selected dependencies to the `cacheDirectory` and return a mapping for the content.
 * Heavily inspired by sbt-assembly (https://github.com/sbt/sbt-assembly/blob/0.13.0/src/main/scala/sbtassembly/Assembly.scala#L157)
 */
def dependencyClasses(dependencies: Classpath, modules: Set[ModuleID], keep: FileFilter, cacheDirectory: File): Seq[(File, String)] = {
  val dependencyFiles: Seq[File] = dependencies.map(_.data).toSeq
  val toInclude = dependencyFiles.filter(f => {
    val p = f.getCanonicalPath
    modules.exists(m => {
      // works for both .m2 (org/scala-lang/modules/scala-asm/5.0.3-scala-3/scala-asm-5.0.3-scala-3.jar)
      // and .ivy2 (org.scala-lang.modules/scala-asm/5.0.3-scala-3/bundles/scala-asm.jar)
      val nameParts = m.organization.split('.').toSet + m.name + m.revision
      nameParts.forall(p.contains)
    })
  })
  assert(toInclude.forall(sbt.classpath.ClasspathUtilities.isArchive), s"Expected JAR files as dependencies: $toInclude")

  val tempDir = cacheDirectory / "unpackedDependencies"

  def sha1name(f: File): String     = bytesToSha1String(f.getCanonicalPath.getBytes("UTF-8"))
  def sha1content(f: File): String  = bytesToSha1String(IO.readBytes(f))
  def bytesToSha1String(bytes: Array[Byte]): String = {
    val sha1 = java.security.MessageDigest.getInstance("SHA-1")
    val hash = sha1.digest(bytes)
    hash map {"%02x".format(_)} mkString
  }

  val jarDirs: Seq[File] = for (jar <- toInclude) yield {
    val jarName = jar.getName
    val hash = sha1name(jar) + "_" + sha1content(jar)
    val jarNamePath = tempDir / (hash + ".jarName")
    val dest = tempDir / hash
    if (!jarNamePath.exists || IO.read(jarNamePath) != jar.getCanonicalPath) {
      IO.delete(dest)
      dest.mkdir()
      IO.unzip(jar, dest)
      IO.write(jarNamePath, jar.getCanonicalPath, IO.utf8, append = false)
    }
    dest
  }

  jarDirs.flatMap(dir => dir ** keep --- dir pair relativeTo(dir))
}

// Defining these settings is somewhat redundant as we also redefine settings that depend on them.
// However, IntelliJ's project import works better when these are set correctly.
def clearSourceAndResourceDirectories = Seq(Compile, Test).flatMap(config => inConfig(config)(Seq(
  unmanagedSourceDirectories := Nil,
  managedSourceDirectories := Nil,
  unmanagedResourceDirectories := Nil,
  managedResourceDirectories := Nil
)))

lazy val mkBinImpl: Def.Initialize[Task[Seq[File]]] = Def.task {
  def mkScalaTool(mainCls: String, classpath: Seq[Attributed[File]]): ScalaTool =
    ScalaTool(mainClass  = mainCls,
      classpath  = classpath.toList.map(_.data.getAbsolutePath),
      properties = Map.empty,
      javaOpts   = "-Xmx256M -Xms32M",
      toolFlags  = "")
  val rootDir = (classDirectory in Compile in compiler).value
  def writeScripts(scalaTool: ScalaTool, file: String, outDir: File): Seq[File] =
    Seq(
      scalaTool.writeScript(file, "unix", rootDir, outDir),
      scalaTool.writeScript(file, "windows", rootDir, outDir)
    )
  def mkQuickBin(file: String, mainCls: String, classpath: Seq[Attributed[File]]): Seq[File] = {
    val scalaTool = mkScalaTool(mainCls, classpath)
    val outDir = buildDirectory.value / "quick/bin"
    writeScripts(scalaTool, file, outDir)
  }

  def mkPackBin(file: String, mainCls: String): Seq[File] = {
    val scalaTool = mkScalaTool(mainCls, classpath = Nil)
    val outDir = buildDirectory.value / "pack/bin"
    writeScripts(scalaTool, file, outDir)
  }

  def mkBin(file: String, mainCls: String, classpath: Seq[Attributed[File]]): Seq[File] =
    mkQuickBin(file, mainCls, classpath) ++ mkPackBin(file, mainCls)

  mkBin("scala"    , "scala.tools.nsc.MainGenericRunner", (fullClasspath in Compile in repl).value) ++
  mkBin("scalac"   , "scala.tools.nsc.Main",              (fullClasspath in Compile in compiler).value) ++
  mkBin("fsc"      , "scala.tools.nsc.CompileClient",     (fullClasspath in Compile in compiler).value) ++
  mkBin("scaladoc" , "scala.tools.nsc.ScalaDoc",          (fullClasspath in Compile in scaladoc).value) ++
  mkBin("scalap"   , "scala.tools.scalap.Main",           (fullClasspath in Compile in scalap).value)
}

buildDirectory in ThisBuild := (baseDirectory in ThisBuild).value / "build-sbt"

lazy val versionProps: Map[String, String] = {
  import java.io.FileInputStream
  import java.util.Properties
  val props = new Properties()
  val in = new FileInputStream(file("versions.properties"))
  try props.load(in)
  finally in.close()
  import scala.collection.JavaConverters._
  props.asScala.toMap
}

def versionNumber(name: String): String =
  versionProps(s"$name.version.number")
