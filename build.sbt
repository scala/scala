/*
 * The new, sbt-based build definition for Scala.
 *
 * What you see below is very much work-in-progress. The following features are implemented:
 *   - Compiling all classses for the compiler and library ("compile" in the respective subprojects)
 *   - Running JUnit tests ("test") and partest ("test/it:test")
 *   - Creating build-sbt/quick with all compiled classes and launcher scripts ("dist/mkQuick")
 *   - Creating build-sbt/pack with all JARs and launcher scripts ("dist/mkPack")
 *   - Building all scaladoc sets ("doc")
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

import VersionUtil.{versionProps, versionNumber, generatePropertiesFileSettings, versionProperties, versionPropertiesSettings}

val bootstrapScalaVersion = versionProps("starr.version")

def withoutScalaLang(moduleId: ModuleID): ModuleID = moduleId exclude("org.scala-lang", "*")

// exclusion of the scala-library transitive dependency avoids eviction warnings during `update`.
val scalaParserCombinatorsDep = withoutScalaLang("org.scala-lang.modules" %% "scala-parser-combinators" % versionNumber("scala-parser-combinators"))
val scalaSwingDep = withoutScalaLang("org.scala-lang.modules" %% "scala-swing" % versionNumber("scala-swing"))
val scalaXmlDep = withoutScalaLang("org.scala-lang.modules" %% "scala-xml" % versionNumber("scala-xml"))
val partestDep = withoutScalaLang("org.scala-lang.modules" %% "scala-partest" % versionNumber("partest"))
val partestInterfaceDep = withoutScalaLang("org.scala-lang.modules" %% "scala-partest-interface" % "0.5.0")
val junitDep = "junit" % "junit" % "4.11"
val junitIntefaceDep = "com.novocode" % "junit-interface" % "0.11" % "test"
val asmDep = "org.scala-lang.modules" % "scala-asm" % versionProps("scala-asm.version")
val jlineDep = "jline" % "jline" % versionProps("jline.version")
val antDep = "org.apache.ant" % "ant" % "1.9.4"
val scalacheckDep = withoutScalaLang("org.scalacheck" %% "scalacheck" % versionNumber("scalacheck") % "it")

lazy val commonSettings = clearSourceAndResourceDirectories ++ versionPropertiesSettings ++ Seq[Setting[_]](
  organization := "org.scala-lang",
  // The ANT build uses the file "build.number" and the property "build.release" to compute the version
  version := "2.11.8-SNAPSHOT",
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
  javacOptions in Compile ++= Seq("-g", "-source", "1.8", "-target", "1.8"),
  // we don't want any unmanaged jars; as a reminder: unmanaged jar is a jar stored
  // directly on the file system and it's not resolved through Ivy
  // Ant's build stored unmanaged jars in `lib/` directory
  unmanagedJars in Compile := Seq.empty,
  sourceDirectory in Compile := baseDirectory.value,
  unmanagedSourceDirectories in Compile := List(baseDirectory.value),
  unmanagedResourceDirectories in Compile += (baseDirectory in ThisBuild).value / "src" / thisProject.value.id,
  scalaSource in Compile := (sourceDirectory in Compile).value,
  javaSource in Compile := (sourceDirectory in Compile).value,
  // resources are stored along source files in our current layout
  resourceDirectory in Compile := (sourceDirectory in Compile).value,
  // each subproject has to ask specifically for files they want to include
  includeFilter in unmanagedResources in Compile := NothingFilter,
  target := (baseDirectory in ThisBuild).value / "target" / thisProject.value.id,
  classDirectory in Compile := buildDirectory.value / "quick/classes" / thisProject.value.id,
  target in Compile in doc := buildDirectory.value / "scaladoc" / thisProject.value.id,
  // given that classDirectory and doc target are overriden to be _outside_ of target directory, we have
  // to make sure they are being cleaned properly
  cleanFiles += (classDirectory in Compile).value,
  cleanFiles += (target in Compile in doc).value,
  fork in run := true,
  scalacOptions in Compile in doc ++= Seq(
    "-doc-footer", "epfl",
    "-diagrams",
    "-implicits",
    "-groups",
    "-doc-version", versionProperties.value.canonicalVersion,
    "-doc-title", description.value,
    "-sourcepath", (baseDirectory in ThisBuild).value.toString,
    "-doc-source-url", s"https://github.com/scala/scala/tree/${versionProperties.value.githubTree}€{FILE_PATH}.scala#L1"
  )
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
lazy val scalaSubprojectSettings: Seq[Setting[_]] = commonSettings ++ generatePropertiesFileSettings :+ setJarLocation

def filterDocSources(ff: FileFilter): Seq[Setting[_]] = Seq(
  sources in (Compile, doc) ~= (_.filter(ff.accept _)),
  // Excluded sources may still be referenced by the included sources, so we add the compiler
  // output to the scaladoc classpath to resolve them. For the `library` project this is
  // always required because otherwise the compiler cannot even initialize Definitions without
  // binaries of the library on the classpath. Specifically, we get this error:
  // (library/compile:doc) scala.reflect.internal.FatalError: package class scala does not have a member Int
  // Ant build does the same thing always: it puts binaries for documented classes on the classpath
  // sbt never does this by default (which seems like a good default)
  dependencyClasspath in (Compile, doc) += (classDirectory in Compile).value,
  doc in Compile <<= doc in Compile dependsOn (compile in Compile)
)

def regexFileFilter(s: String): FileFilter = new FileFilter {
  val pat = s.r.pattern
  def accept(f: File) = pat.matcher(f.getAbsolutePath.replace('\\', '/')).matches()
}

lazy val library = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings: _*)
  .settings(
    name := "scala-library",
    description := "Scala Standard Library",
    compileOrder := CompileOrder.Mixed, // needed for JFunction classes in scala.runtime.java8
    scalacOptions in Compile ++= Seq[String]("-sourcepath", (scalaSource in Compile).value.toString),
    scalacOptions in Compile in doc ++= {
      val libraryAuxDir = (baseDirectory in ThisBuild).value / "src/library-aux"
      Seq(
        "-doc-no-compile", libraryAuxDir.toString,
        "-skip-packages", "scala.concurrent.impl",
        "-doc-root-content", (sourceDirectory in Compile).value + "/rootdoc.txt"
      )
    },
    includeFilter in unmanagedResources in Compile := "*.tmpl" | "*.xml" | "*.js" | "*.css" | "rootdoc.txt"
  )
  .settings(filterDocSources("*.scala" -- (regexFileFilter(".*/runtime/.*\\$\\.scala") ||
                                           regexFileFilter(".*/runtime/ScalaRunTime\\.scala") ||
                                           regexFileFilter(".*/runtime/StringAdd\\.scala"))): _*)

lazy val reflect = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings: _*)
  .settings(
    name := "scala-reflect",
    description := "Scala Reflection Library",
    scalacOptions in Compile in doc ++= Seq(
      "-skip-packages", "scala.reflect.macros.internal:scala.reflect.internal:scala.reflect.io"
    )
  )
  .dependsOn(library)

lazy val compiler = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings: _*)
  .settings(
    name := "scala-compiler",
    description := "Scala Compiler",
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
      (mappings in Compile in packageBin in LocalProject("repl")).value ++
      (mappings in Compile in packageBin in LocalProject("repl-jline")).value.filter(_._2 != "repl-jline.properties") ++
      (mappings in Compile in packageBin in LocalProject("repl-jline-embedded")).value,
    includeFilter in unmanagedResources in Compile :=
      "*.tmpl" | "*.xml" | "*.js" | "*.css" | "*.html" | "*.properties" | "*.swf" |
      "*.png" | "*.gif" | "*.gif" | "*.txt",
    scalacOptions in Compile in doc ++= Seq(
      "-doc-root-content", (sourceDirectory in Compile).value + "/rootdoc.txt"
    ),
    // Generate the ScriptEngineFactory service definition. The ant build does this when building
    // the JAR but sbt has no support for it and it is easier to do as a resource generator:
    generateServiceProviderResources("javax.script.ScriptEngineFactory" -> "scala.tools.nsc.interpreter.IMain$Factory"),
    managedResourceDirectories in Compile := Seq((resourceManaged in Compile).value)
  )
  .dependsOn(library, reflect)

lazy val interactive = configureAsSubproject(project)
  .settings(disableDocsAndPublishingTasks: _*)
  .settings(
    name := "scala-compiler-interactive",
    description := "Scala Interactive Compiler"
  )
  .dependsOn(compiler)

lazy val repl = configureAsSubproject(project)
  .settings(disableDocsAndPublishingTasks: _*)
  .settings(
    connectInput in run := true,
    outputStrategy in run := Some(StdoutOutput),
    run <<= (run in Compile).partialInput(" -usejavacp") // Automatically add this so that `repl/run` works without additional arguments.
  )
  .dependsOn(compiler, interactive)

lazy val replJline = configureAsSubproject(Project("repl-jline", file(".") / "src" / "repl-jline"))
  .settings(
    libraryDependencies += jlineDep,
    name := "scala-repl-jline",
    doc := file("!!! NO DOCS !!!")
  )
  .dependsOn(repl)

lazy val replJlineEmbedded = Project("repl-jline-embedded", file(".") / "target" / "repl-jline-embedded-src-dummy")
  .settings(scalaSubprojectSettings: _*)
  .settings(
    name := "scala-repl-jline-embedded",
    // There is nothing to compile for this project. Instead we use the compile task to create
    // shaded versions of repl-jline and jline.jar. dist/mkBin puts all of quick/repl,
    // quick/repl-jline and quick/repl-jline-shaded on the classpath for quick/bin scripts.
    // This is different from the ant build where all parts are combined into quick/repl, but
    // it is cleaner because it avoids circular dependencies.
    compile in Compile <<= (compile in Compile).dependsOn(Def.task {
      import java.util.jar._
      import collection.JavaConverters._
      val inputs: Iterator[JarJar.Entry] = {
        val repljlineClasses = (products in Compile in replJline).value.flatMap(base => Path.allSubpaths(base).map(x => (base, x._1)))
        val jlineJAR = (dependencyClasspath in Compile).value.find(_.get(moduleID.key) == Some(jlineDep)).get.data
        val jarFile = new JarFile(jlineJAR)
        val jarEntries = jarFile.entries.asScala.filterNot(_.isDirectory).map(entry => JarJar.JarEntryInput(jarFile, entry))
        def compiledClasses = repljlineClasses.iterator.map { case (base, file) => JarJar.FileInput(base, file) }
        (jarEntries ++ compiledClasses).filter(x =>
          x.name.endsWith(".class") || x.name.endsWith(".properties") || x.name.startsWith("META-INF/native") || x.name.startsWith("META-INF/maven")
        )
      }
      //println(inputs.map(_.name).mkString("\n"))
      import JarJar.JarJarConfig._
      val config: Seq[JarJar.JarJarConfig] = Seq(
        Rule("org.fusesource.**", "scala.tools.fusesource_embedded.@1"),
        Rule("jline.**", "scala.tools.jline_embedded.@1"),
        Rule("scala.tools.nsc.interpreter.jline.**", "scala.tools.nsc.interpreter.jline_embedded.@1"),
        Keep("scala.tools.**")
      )
      val outdir = (classDirectory in Compile).value
      JarJar(inputs, outdir, config)
    }),
    // Exclude repl-jline-embedded.properties from JAR
    mappings in (Compile, packageBin) :=
      (mappings in (Compile, packageBin)).value.filter(_._2 != "repl-jline-embedded.properties")
  )
  .dependsOn(replJline)

lazy val scaladoc = configureAsSubproject(project)
  .settings(
    name := "scala-compiler-doc",
    description := "Scala Documentation Generator",
    libraryDependencies ++= Seq(scalaXmlDep, scalaParserCombinatorsDep, partestDep),
    includeFilter in unmanagedResources in Compile := "*.html" | "*.css" | "*.gif" | "*.png" | "*.js" | "*.txt"
  )
  .settings(disableDocsAndPublishingTasks: _*)
  .dependsOn(compiler)

lazy val scalap = configureAsSubproject(project).
  settings(
    description := "Scala Bytecode Parser",
    // Include decoder.properties
    includeFilter in unmanagedResources in Compile := "*.properties"
  )
  .dependsOn(compiler)

lazy val partestExtras = configureAsSubproject(Project("partest-extras", file(".") / "src" / "partest-extras"))
  .dependsOn(replJlineEmbedded)
  .settings(clearSourceAndResourceDirectories: _*)
  .settings(
    name := "scala-partest-extras",
    description := "Scala Compiler Testing Tool (compiler-specific extras)",
    libraryDependencies += partestDep,
    unmanagedSourceDirectories in Compile := List(baseDirectory.value),
    doc := file("!!! NO DOCS !!!")
  )

lazy val junit = project.in(file("test") / "junit")
  .dependsOn(library, reflect, compiler, partestExtras, scaladoc)
  .settings(clearSourceAndResourceDirectories: _*)
  .settings(commonSettings: _*)
  .settings(
    fork in Test := true,
    libraryDependencies ++= Seq(junitDep, junitIntefaceDep),
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v"),
    unmanagedSourceDirectories in Test := List(baseDirectory.value),
    doc := file("!!! NO DOCS !!!")
  )

lazy val partestJavaAgent = Project("partest-javaagent", file(".") / "src" / "partest-javaagent")
  .settings(commonSettings: _*)
  .settings(generatePropertiesFileSettings: _*)
  .settings(
    libraryDependencies += asmDep,
    doc := file("!!! NO DOCS !!!"),
    publishLocal := {},
    publish := {},
    // Setting name to "scala-partest-javaagent" so that the jar file gets that name, which the Runner relies on
    name := "scala-partest-javaagent",
    description := "Scala Compiler Testing Tool (compiler-specific java agent)",
    // writing jar file to $buildDirectory/pack/lib because that's where it's expected to be found
    setJarLocation,
    // add required manifest entry - previously included from file
    packageOptions in (Compile, packageBin) +=
      Package.ManifestAttributes( "Premain-Class" -> "scala.tools.partest.javaagent.ProfilingAgent" ),
    // we need to build this to a JAR
    exportJars := true
  )

lazy val test = project.
  dependsOn(compiler, interactive, replJlineEmbedded, scalap, partestExtras, partestJavaAgent, scaladoc).
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

lazy val manual = configureAsSubproject(project)
  .settings(
    libraryDependencies ++= Seq(scalaXmlDep, antDep),
    classDirectory in Compile := (target in Compile).value / "classes"
  )
  .settings(disableDocsAndPublishingTasks: _*)
  .dependsOn(library)

lazy val scalaDist = Project("scala-dist", file(".") / "target" / "scala-dist-dist-src-dummy")
  .settings(commonSettings: _*)
  .settings(
    doc := file("!!! NO DOCS !!!"),
    mappings in Compile in packageBin ++= {
      val binBaseDir = buildDirectory.value / "pack"
      val binMappings = (mkBin in dist).value.pair(relativeTo(binBaseDir), errorIfNone = false)
      // With the way the resource files are spread out over the project sources we can't just add
      // an unmanagedResourceDirectory, so we generate the mappings manually:
      val docBaseDir = (baseDirectory in ThisBuild).value
      val docMappings = (docBaseDir / "doc").*** pair relativeTo(docBaseDir)
      val resBaseDir = (baseDirectory in ThisBuild).value / "src/manual/scala/tools/docutil/resources"
      val resMappings = resBaseDir ** ("*.html" | "*.css" | "*.gif" | "*.png") pair (p => relativeTo(resBaseDir)(p).map("doc/tools/" + _))
      docMappings ++ resMappings ++ binMappings
    },
    resourceGenerators in Compile += Def.task {
      val command = "fsc, scala, scalac, scaladoc, scalap"
      val htmlOut = (resourceManaged in Compile).value / "doc/tools"
      val manOut = (resourceManaged in Compile).value / "genman"
      val fixedManOut = (resourceManaged in Compile).value / "man"
      IO.createDirectory(htmlOut)
      IO.createDirectory(manOut / "man1")
      toError(runner.value.run("scala.tools.docutil.ManMaker",
        (fullClasspath in Compile in manual).value.files,
        Seq(command, htmlOut.getAbsolutePath, manOut.getAbsolutePath),
        streams.value.log))
      (manOut ** "*.1" pair rebase(manOut, fixedManOut)).foreach { case (in, out) =>
        // Generated manpages should always use LF only. There doesn't seem to be a good reason
        // for generating them with the platform EOL first and then converting them but that's
        // what the ant build does.
        IO.write(out, IO.readBytes(in).filterNot(_ == '\r'))
      }
      (htmlOut ** "*.html").get ++ (fixedManOut ** "*.1").get
    }.taskValue,
    managedResourceDirectories in Compile := Seq((resourceManaged in Compile).value),
    packageOptions in Compile in packageBin := Seq.empty
  )

lazy val root = (project in file(".")).
  aggregate(library, reflect, compiler, interactive, repl, replJline, replJlineEmbedded,
    scaladoc, scalap, partestExtras, junit).settings(
    sources in Compile := Seq.empty,
    onLoadMessage := """|*** Welcome to the sbt build definition for Scala! ***
      |This build definition has an EXPERIMENTAL status. If you are not
      |interested in testing or working on the build itself, please use
      |the Ant build definition for now. Check README.md for more information.""".stripMargin
  )

// The following subprojects' binaries are required for building "pack":
lazy val distDependencies = Seq(replJline, replJlineEmbedded, compiler, library, partestExtras, partestJavaAgent, reflect, scalap, scaladoc)

lazy val dist = (project in file("dist"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(scalaSwingDep, jlineDep),
    mkBin := mkBinImpl.value,
    mkQuick <<= Def.task {} dependsOn ((distDependencies.map(products in Runtime in _) :+ mkBin): _*),
    mkPack <<= Def.task {} dependsOn (packageBin in Compile, mkBin),
    target := (baseDirectory in ThisBuild).value / "target" / thisProject.value.id,
    packageBin in Compile := {
      val extraDeps = Set(scalaSwingDep, scalaParserCombinatorsDep, scalaXmlDep)
      val targetDir = (buildDirectory in ThisBuild).value / "pack" / "lib"
      def uniqueModule(m: ModuleID) = (m.organization, m.name.replaceFirst("_.*", ""))
      val extraModules = extraDeps.map(uniqueModule)
      val extraJars = (externalDependencyClasspath in Compile).value.map(a => (a.get(moduleID.key), a.data)).collect {
        case (Some(m), f) if extraModules contains uniqueModule(m) => f
      }
      val jlineJAR = (dependencyClasspath in Compile).value.find(_.get(moduleID.key) == Some(jlineDep)).get.data
      val mappings = extraJars.map(f => (f, targetDir / f.getName)) :+ (jlineJAR, targetDir / "jline.jar")
      IO.copy(mappings, overwrite = true)
      targetDir
    },
    cleanFiles += (buildDirectory in ThisBuild).value / "quick",
    cleanFiles += (buildDirectory in ThisBuild).value / "pack",
    packageBin in Compile <<= (packageBin in Compile).dependsOn(distDependencies.map(packageBin in Compile in _): _*)
  )
  .dependsOn(distDependencies.map(p => p: ClasspathDep[ProjectReference]): _*)

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


lazy val buildDirectory = settingKey[File]("The directory where all build products go. By default ./build")
lazy val mkBin = taskKey[Seq[File]]("Generate shell script (bash or Windows batch).")
lazy val mkQuick = taskKey[Unit]("Generate a full build, including scripts, in build-sbt/quick")
lazy val mkPack = taskKey[Unit]("Generate a full build, including scripts, in build-sbt/pack")

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
  import java.io.IOException
  def mkScalaTool(mainCls: String, classpath: Seq[Attributed[File]]): ScalaTool =
    ScalaTool(mainClass  = mainCls,
      classpath  = classpath.toList.map(_.data.getAbsolutePath),
      properties = Map.empty,
      javaOpts   = "-Xmx256M -Xms32M",
      toolFlags  = "")
  val rootDir = (classDirectory in Compile in compiler).value
  val quickOutDir = buildDirectory.value / "quick/bin"
  val packOutDir = buildDirectory.value / "pack/bin"
  def writeScripts(scalaTool: ScalaTool, file: String, outDir: File): Seq[File] = {
    val res = Seq(
      scalaTool.writeScript(file, "unix", rootDir, outDir),
      scalaTool.writeScript(file, "windows", rootDir, outDir)
    )
    res.foreach { f =>
      //TODO 2.12: Use Files.setPosixFilePermissions() (Java 7+) instead of calling out to chmod
      if(Process(List("chmod", "ugo+rx", f.getAbsolutePath())).! > 0)
        throw new IOException("chmod failed")
    }
    res
  }

  def mkBin(file: String, mainCls: String, classpath: Seq[Attributed[File]]): Seq[File] =
    writeScripts(mkScalaTool(mainCls, classpath), file, quickOutDir) ++
    writeScripts(mkScalaTool(mainCls, Nil      ), file, packOutDir)

  streams.value.log.info(s"Creating scripts in $quickOutDir and $packOutDir")

  mkBin("scala"    , "scala.tools.nsc.MainGenericRunner", (fullClasspath in Compile in replJlineEmbedded).value) ++
  mkBin("scalac"   , "scala.tools.nsc.Main",              (fullClasspath in Compile in compiler).value) ++
  mkBin("fsc"      , "scala.tools.nsc.CompileClient",     (fullClasspath in Compile in compiler).value) ++
  mkBin("scaladoc" , "scala.tools.nsc.ScalaDoc",          (fullClasspath in Compile in scaladoc).value) ++
  mkBin("scalap"   , "scala.tools.scalap.Main",           (fullClasspath in Compile in scalap).value)
}

/** Generate service provider definition files under META-INF/services */
def generateServiceProviderResources(services: (String, String)*): Setting[_] =
  resourceGenerators in Compile += Def.task {
    services.map { case (k, v) =>
      val f = (resourceManaged in Compile).value / "META-INF/services" / k
      IO.write(f, v + "\n")
      f
    }
  }.taskValue

buildDirectory in ThisBuild := (baseDirectory in ThisBuild).value / "build-sbt"
