import sbt._
import Keys._
import partest._
import SameTest._
import ScalaBuildKeys._



object ScalaBuild extends Build with Layers {

  // Build wide settings:
  override lazy val settings = super.settings ++ Versions.settings ++ Seq(
    autoScalaLibrary := false,
    resolvers += Resolver.url(
      "Typesafe nightlies", 
      url("https://typesafe.artifactoryonline.com/typesafe/ivy-snapshots/")
    )(Resolver.ivyStylePatterns),
    resolvers ++= Seq(
      "junit interface repo" at "https://repository.jboss.org/nexus/content/repositories/scala-tools-releases",
      ScalaToolsSnapshots
    ),
    organization := "org.scala-lang",
    version <<= Versions.mavenVersion,
    pomExtra := epflPomExtra,
    commands += Command.command("fix-uri-projects") { (state: State) =>
      if(state.get(buildFixed) getOrElse false) state
      else {
        // TODO -fix up scalacheck's dependencies!
        val extracted = Project.extract(state)
        import extracted._
        def fix(s: Setting[_]): Setting[_] = s match {
          case ScopedExternalSetting(`scalacheck`, scalaInstance.key, setting)    => fullQuickScalaReference mapKey Project.mapScope(_ => s.key.scope)
          case s                                                                  => s
         }
         val transformed = session.mergeSettings map ( s => fix(s) )
         val scopes = transformed collect { case ScopedExternalSetting(`scalacheck`, _, s) => s.key.scope } toSet
         // Create some fixers so we don't download scala or rely on it.
         val fixers = for { scope <- scopes
                            setting <- Seq(autoScalaLibrary := false, crossPaths := false)
                      } yield setting mapKey Project.mapScope(_ => scope)
         val newStructure = Load.reapply(transformed ++ fixers, structure)
         Project.setProject(session, newStructure, state).put(buildFixed, true)
      }
    },
    onLoad in Global <<= (onLoad in Global) apply (_ andThen { (state: State) =>
      "fix-uri-projects" :: state
    })
  )

  // Collections of projects to run 'compile' on.
  lazy val compiledProjects = Seq(quickLib, quickComp, continuationsLibrary, actors, swing, forkjoin, fjbg)
  // Collection of projects to 'package' and 'publish' together.
  lazy val packagedBinaryProjects = Seq(scalaLibrary, scalaCompiler, swing, continuationsPlugin, jline, scalap)
  lazy val partestRunProjects = Seq(testsuite, continuationsTestsuite)
  
  private def epflPomExtra = (
    <xml:group>
      <inceptionYear>2002</inceptionYear>
      <licenses>
        <license>
          <name>BSD-like</name>
          <url>http://www.scala-lang.org/downloads/license.html</url>
        </license>
      </licenses>
      <scm>
        <connection>scm:git:git://github.com/scala/scala.git</connection>
      </scm>
      <issueManagement>
        <system>jira</system>
        <url>http://issues.scala-lang.org</url>
      </issueManagement>
    </xml:group>
  )
    
  // Settings used to make sure publishing goes smoothly.
  def publishSettings: Seq[Setting[_]] = Seq(
    ivyScala ~= ((is: Option[IvyScala]) => is.map(_.copy(checkExplicit = false))),
    pomIncludeRepository := (_ => false),
    publishMavenStyle := true,
    makePomConfiguration <<= makePomConfiguration apply (_.copy(configurations = Some(Seq(Compile, Default)))),
    pomExtra := epflPomExtra
  )

  // Settings for root project.  These are aggregate tasks against the rest of the build.
  def projectSettings: Seq[Setting[_]] = publishSettings ++ Seq(
    doc in Compile <<= (doc in documentation in Compile).identity,
    // These next two aggregate commands on several projects and return results that are to be ignored by remaining tasks.
    compile in Compile <<= compiledProjects.map(p => compile in p in Compile).join.map(_.head),
    // TODO - just clean target? i.e. target map IO.deleteRecursively
    clean <<= (compiledProjects ++ partestRunProjects).map(p => clean in p).dependOn,
    packageBin in Compile <<= packagedBinaryProjects.map(p => packageBin in p in Compile).join.map(_.head),
    // TODO - Make sure scalaLibrary has packageDoc + packageSrc from documentation attached...
    publish <<= packagedBinaryProjects.map(p => publish in p).join.map(_.head),
    publishLocal <<= packagedBinaryProjects.map(p => publishLocal in p).join.map(_.head),
    packageDoc in Compile <<= (packageDoc in documentation in Compile).identity,
    packageSrc in Compile <<= (packageSrc in documentation in Compile).identity,
    test in Test <<= (runPartest in testsuite, runPartest in continuationsTestsuite, checkSame in testsuite) map { (a,b,c) => () },
    lockerLock <<= (lockFile in lockerLib, lockFile in lockerComp, compile in Compile in lockerLib, compile in Compile in lockerComp) map { (lib, comp, _, _) =>
      Seq(lib,comp).foreach(f => IO.touch(f))
    },
    lockerUnlock <<= (lockFile in lockerLib, lockFile in lockerComp) map { (lib, comp) =>
      Seq(lib,comp).foreach(IO.delete)
    },
    genBinQuick <<= (genBinQuick in scaladist).identity,
    makeDist <<= (makeDist in scaladist).identity,
    makeExplodedDist <<= (makeExplodedDist in scaladist).identity,
    // Note: We override unmanagedSources so that ~ compile will look at all these sources, then run our aggregated compile...
    unmanagedSourceDirectories in Compile <<= baseDirectory apply (_ / "src") apply { dir =>
      Seq("library/scala","actors","compiler","fjbg","swing","continuations/library","forkjoin") map (dir / _)
    },
    // TODO - Make exported products == makeDist so we can use this when creating a *real* distribution.
    commands += Release.pushStarr
    //commands += Release.setStarrHome
  )
  // Note: Root project is determined by lowest-alphabetical project that has baseDirectory as file(".").  we use aaa_ to 'win'.
  lazy val aaa_root = Project("scala", file(".")) settings(projectSettings: _*) settings(ShaResolve.settings: _*)

  // External dependencies used for various projects
  lazy val externalDeps: Setting[_] = libraryDependencies <<= (sbtVersion)(v => 
    Seq(
      "org.apache.ant" % "ant" % "1.8.2",
      "org.scala-sbt" % "compiler-interface" % v % "provided"
    )
  )

  // These are setting overrides for most artifacts in the Scala build file.
  def settingOverrides: Seq[Setting[_]] = publishSettings ++ Seq(
    crossPaths := false,
    autoScalaLibrary := false,
    // Work around a bug where scala-library (and forkjoin) is put on classpath for analysis.
    classpathOptions := ClasspathOptions.manual,
    publishArtifact in packageDoc := false,
    publishArtifact in packageSrc := false,
    target <<= (baseDirectory, name) apply (_ / "target" / _),
    (classDirectory in Compile) <<= target(_ / "classes"),
    javacOptions ++= Seq("-target", "1.5", "-source", "1.5"),
    scalaSource in Compile <<= (baseDirectory, name) apply (_ / "src" / _),
    javaSource in Compile <<= (baseDirectory, name) apply (_ / "src" / _),
    autoScalaLibrary := false,
    unmanagedJars in Compile := Seq(),
    // Most libs in the compiler use this order to build.
    compileOrder in Compile := CompileOrder.JavaThenScala,
    lockFile <<= target(_ / "compile.lock"),
    skip in Compile <<= lockFile map (_.exists),
    lock <<= lockFile map (f => IO.touch(f)),
    unlock <<= lockFile map IO.delete
  )

  // --------------------------------------------------------------
  //  Libraries used by Scalac that change infrequently
  //  (or hopefully so).
  // --------------------------------------------------------------

  // Jline nested project.   Compile this sucker once and be done.
  lazy val jline = Project("jline", file("src/jline"))
  // Fast Java Bytecode Generator (nested in every scala-compiler.jar)
  lazy val fjbg = Project("fjbg", file(".")) settings(settingOverrides : _*)
  // Our wrapped version of msil.
  lazy val asm = Project("asm", file(".")) settings(settingOverrides : _*)
  // Forkjoin backport
  lazy val forkjoin = Project("forkjoin", file(".")) settings(settingOverrides : _*)

  // --------------------------------------------------------------
  //  The magic kingdom.
  //  Layered compilation of Scala.
  //   Stable Reference -> Locker ('Lockable' dev version) -> Quick -> Strap (Binary compatibility testing)
  // --------------------------------------------------------------

  // Need a report on this...
  // TODO - Resolve STARR from a repo..
  lazy val STARR = scalaInstance <<= (appConfiguration, ShaResolve.pullBinaryLibs in ThisBuild) map { (app, _) =>
    val launcher = app.provider.scalaProvider.launcher
    val library  = file("lib/scala-library.jar")
    val compiler = file("lib/scala-compiler.jar")
    val libJars  = (file("lib") * "*.jar").get filterNot Set(library, compiler)
    ScalaInstance("starr", library, compiler, launcher, libJars: _*)
  }

  // Locker is a lockable Scala compiler that can be built of 'current' source to perform rapid development.
  lazy val (lockerLib, lockerComp) = makeLayer("locker", STARR, autoLock = true)
  lazy val locker = Project("locker", file(".")) aggregate(lockerLib, lockerComp)

  // Quick is the general purpose project layer for the Scala compiler.
  lazy val (quickLib, quickComp) = makeLayer("quick", makeScalaReference("locker", lockerLib, lockerComp))
  lazy val quick = Project("quick", file(".")) aggregate(quickLib, quickComp)

  // Reference to quick scala instance.
  lazy val quickScalaInstance = makeScalaReference("quick", quickLib, quickComp)
  def quickScalaLibraryDependency = unmanagedClasspath in Compile <++= (exportedProducts in quickLib in Compile).identity
  def quickScalaCompilerDependency = unmanagedClasspath in Compile <++= (exportedProducts in quickComp in Compile).identity

  // Strapp is used to test binary 'sameness' between things built with locker and things built with quick.
  lazy val (strappLib, strappComp) = makeLayer("strapp", quickScalaInstance)

  // --------------------------------------------------------------
  //  Projects dependent on layered compilation (quick)
  // --------------------------------------------------------------
  def addCheaterDependency(projectName: String): Setting[_] = 
    pomPostProcess <<= (version, organization, pomPostProcess) apply { (v,o,k) => 
      val dependency: scala.xml.Node = 
        <dependency>
          <groupId>{o}</groupId>
          <artifactid>{projectName}</artifactid>
          <version>{v}</version>
        </dependency>
      def fixDependencies(node: scala.xml.Node): scala.xml.Node = node match {
         case <dependencies>{nested@_*}</dependencies> => <dependencies>{dependency}{nested}</dependencies>
         case x                                        => x
      }
      // This is a hack to get around issues where \ and \\ don't work if any of the children are `scala.xml.Group`.
      def hasDependencies(root: scala.xml.Node): Boolean =
        (root.child collectFirst {
          case n: scala.xml.Elem if n.label == "dependencies" => n
        } isEmpty)
      // TODO - Keep namespace on project...
      k andThen { 
        case n @ <project>{ nested@_*}</project> if hasDependencies(n)   =>
          <project xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://maven.apache.org/POM/4.0.0">{nested}<dependencies>{dependency}</dependencies></project>
        case <project>{ nested@_*}</project>                                       => 
          <project xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://maven.apache.org/POM/4.0.0">{ nested map fixDependencies }</project>
      }
    }

  // TODO - in sabbus, these all use locker to build...  I think tihs way is better, but let's farm this idea around.
  // TODO - Actors + swing separate jars...
  lazy val dependentProjectSettings = settingOverrides ++ Seq(quickScalaInstance, quickScalaLibraryDependency, addCheaterDependency("scala-library"))
  lazy val actors = Project("actors", file(".")) settings(dependentProjectSettings:_*) dependsOn(forkjoin % "provided")
  // TODO - Remove actors dependency from pom...
  lazy val swing = Project("swing", file(".")) settings(dependentProjectSettings:_*) dependsOn(actors % "provided")
  // This project will generate man pages (in man1 and html) for scala.    
  lazy val manmakerSettings: Seq[Setting[_]] = dependentProjectSettings :+ externalDeps
  lazy val manmaker = Project("manual", file(".")) settings(manmakerSettings:_*)

  // Things that compile against the compiler.
  lazy val compilerDependentProjectSettings = dependentProjectSettings ++ Seq(quickScalaCompilerDependency, addCheaterDependency("scala-compiler"))
  lazy val partestSettings = compilerDependentProjectSettings :+ externalDeps
  lazy val partest = Project("partest", file(".")) settings(partestSettings:_*)  dependsOn(actors,forkjoin,scalap)
  lazy val scalapSettings = compilerDependentProjectSettings ++ Seq(
    name := "scalap",
    exportJars := true
  )
  lazy val scalap = Project("scalap", file(".")) settings(scalapSettings:_*)

  // --------------------------------------------------------------
  //  Continuations plugin + library
  // --------------------------------------------------------------
  lazy val continuationsPluginSettings = compilerDependentProjectSettings ++ Seq(
    scalaSource in Compile <<= baseDirectory(_ / "src/continuations/plugin/"),
    resourceDirectory in Compile <<= baseDirectory(_ / "src/continuations/plugin/"),
    exportJars := true,
    name := "continuations"  // Note: This artifact is directly exported.

  )
  lazy val continuationsPlugin = Project("continuations-plugin", file(".")) settings(continuationsPluginSettings:_*)
  lazy val continuationsLibrarySettings = dependentProjectSettings ++ Seq(
    scalaSource in Compile <<= baseDirectory(_ / "src/continuations/library/"),
    scalacOptions in Compile <++= (exportedProducts in Compile in continuationsPlugin) map { 
     case Seq(cpDir) => Seq("-Xplugin-require:continuations", "-P:continuations:enable", "-Xplugin:"+cpDir.data.getAbsolutePath)
    }
  )
  lazy val continuationsLibrary = Project("continuations-library", file(".")) settings(continuationsLibrarySettings:_*)

  // TODO - OSGi Manifest

  // --------------------------------------------------------------
  //  Real Library Artifact
  // --------------------------------------------------------------
  val allSubpathsCopy = (dir: File) => (dir.*** --- dir) x (relativeTo(dir)|flat)
  def productTaskToMapping(products : Seq[File]) = products flatMap { p => allSubpathsCopy(p) }
  lazy val packageScalaLibBinTask = Seq(quickLib, continuationsLibrary, forkjoin, actors).map(p => products in p in Compile).join.map(_.flatten).map(productTaskToMapping)
  lazy val scalaLibArtifactSettings: Seq[Setting[_]] = inConfig(Compile)(Defaults.packageTasks(packageBin, packageScalaLibBinTask)) ++ Seq(
    name := "scala-library",
    crossPaths := false,
    exportJars := true,
    autoScalaLibrary := false,
    unmanagedJars in Compile := Seq(),
    packageDoc in Compile <<= (packageDoc in documentation in Compile).identity,
    packageSrc in Compile <<= (packageSrc in documentation in Compile).identity,
    fullClasspath in Runtime <<= (exportedProducts in Compile).identity,
    quickScalaInstance,
    target <<= (baseDirectory, name) apply (_ / "target" / _)
  )
  lazy val scalaLibrary = Project("scala-library", file(".")) settings(publishSettings:_*) settings(scalaLibArtifactSettings:_*)

  // --------------------------------------------------------------
  //  Real Compiler Artifact
  // --------------------------------------------------------------
  lazy val packageScalaBinTask = Seq(quickComp, fjbg, asm).map(p => products in p in Compile).join.map(_.flatten).map(productTaskToMapping)
  lazy val scalaBinArtifactSettings : Seq[Setting[_]] = inConfig(Compile)(Defaults.packageTasks(packageBin, packageScalaBinTask)) ++ Seq(
    name := "scala-compiler",
    crossPaths := false,
    exportJars := true,
    autoScalaLibrary := false,
    unmanagedJars in Compile := Seq(),
    fullClasspath in Runtime <<= (exportedProducts in Compile).identity,
    quickScalaInstance,
    target <<= (baseDirectory, name) apply (_ / "target" / _)
  )
  lazy val scalaCompiler = Project("scala-compiler", file(".")) settings(publishSettings:_*) settings(scalaBinArtifactSettings:_*) dependsOn(scalaLibrary)
  lazy val fullQuickScalaReference = makeScalaReference("pack", scalaLibrary, scalaCompiler)

  // --------------------------------------------------------------
  //  Testing
  // --------------------------------------------------------------
  /* lazy val scalacheckSettings: Seq[Setting[_]] = Seq(fullQuickScalaReference, crossPaths := false)*/
  lazy val scalacheck = uri("git://github.com/jsuereth/scalacheck.git#scala-build")

  lazy val testsuiteSettings: Seq[Setting[_]] = compilerDependentProjectSettings ++ partestTaskSettings ++ VerifyClassLoad.settings ++ Seq(
    unmanagedBase <<= baseDirectory / "test/files/lib",
    fullClasspath in VerifyClassLoad.checkClassLoad <<= (fullClasspath in scalaLibrary in Runtime).identity,
    autoScalaLibrary := false,
    checkSameLibrary <<= checkSameBinaryProjects(quickLib, strappLib),
    checkSameCompiler <<= checkSameBinaryProjects(quickComp, strappComp),
    checkSame <<= (checkSameLibrary, checkSameCompiler) map ((a,b) => ()),
    autoScalaLibrary := false
  )
  lazy val continuationsTestsuiteSettings: Seq[Setting[_]] = testsuiteSettings ++ Seq(
    scalacOptions in Test <++= (exportedProducts in Compile in continuationsPlugin) map { 
     case Seq(cpDir) => Seq("-Xplugin-require:continuations", "-P:continuations:enable", "-Xplugin:"+cpDir.data.getAbsolutePath)
    },
    partestDirs <<= baseDirectory apply { bd =>
      def mkFile(name: String) = bd / "test" / "files" / name
      def mkTestType(name: String) = name.drop("continuations-".length).toString
      Seq("continuations-neg", "continuations-run") map (t => mkTestType(t) -> mkFile(t)) toMap
    }
  )
  val testsuite = (
    Project("testsuite", file(".")) 
    settings (testsuiteSettings:_*)
    dependsOn (swing, scalaLibrary, scalaCompiler, fjbg, partest, scalacheck)
  )
  val continuationsTestsuite = (
    Project("continuations-testsuite", file("."))
    settings (continuationsTestsuiteSettings:_*) 
    dependsOn (partest, swing, scalaLibrary, scalaCompiler, fjbg)
  )

  // --------------------------------------------------------------
  //  Generating Documentation.
  // --------------------------------------------------------------
  
  // TODO - Migrate this into the dist project.
  // Scaladocs
  def distScalaInstance = makeScalaReference("dist", scalaLibrary, scalaCompiler)
  lazy val documentationSettings: Seq[Setting[_]] = dependentProjectSettings ++ Seq(
    // TODO - Make these work for realz.
    defaultExcludes in unmanagedSources in Compile := ((".*"  - ".") || HiddenFileFilter ||
      "reflect/Print.scala" ||
      "reflect/Symbol.scala" ||
      "reflect/Tree.scala" ||
      "reflect/Type.scala" ||
      "runtime/*$.scala" ||
      "runtime/ScalaRuntime.scala" ||
      "runtime/StringAdd.scala" ||
      "scala/swing/test/*"),
    sourceFilter in Compile := ("*.scala"),
    unmanagedSourceDirectories in Compile <<= baseDirectory apply { dir =>
      Seq(dir / "src" / "library" / "scala", dir / "src" / "actors", dir / "src" / "swing", dir / "src" / "continuations" / "library")
    },
    compile := inc.Analysis.Empty,
    // scaladocOptions in Compile <++= (baseDirectory) map (bd =>
    //   Seq("-sourcepath", (bd / "src" / "library").getAbsolutePath,
    //       "-doc-no-compile", (bd / "src" / "library-aux").getAbsolutePath,
    //       "-doc-source-url", """https://lampsvn.epfl.ch/trac/scala/browser/scala/trunk/src/€{FILE_PATH}.scala#L1""",
    //       "-doc-root-content", (bd / "compiler/scala/tools/nsc/doc/html/resource/lib/rootdoc.txt").getAbsolutePath
    //   )),
    classpathOptions in Compile := ClasspathOptions.manual
  )
  lazy val documentation = (
    Project("documentation", file("."))
    settings (documentationSettings: _*)
    dependsOn(quickLib, quickComp, actors, fjbg, forkjoin, swing, continuationsLibrary)
  )

  // --------------------------------------------------------------
  //  Packaging a distro
  // --------------------------------------------------------------

  class ScalaToolRunner(classpath: Classpath) {
    // TODO - Don't use the ant task directly...
    lazy val classLoader        = new java.net.URLClassLoader(classpath.map(_.data.toURI.toURL).toArray, null)
    lazy val mainClass          = classLoader.loadClass("scala.tools.ant.ScalaTool")
    lazy val executeMethod      = mainClass.getMethod("execute")
    lazy val setFileMethod      = mainClass.getMethod("setFile", classOf[java.io.File])
    lazy val setClassMethod     = mainClass.getMethod("setClass", classOf[String])
    lazy val setClasspathMethod = mainClass.getMethod("setClassPath", classOf[String])
    lazy val instance           = mainClass.newInstance()
    
    def setClass(cls: String): Unit    = setClassMethod.invoke(instance, cls)
    def setFile(file: File): Unit      = setFileMethod.invoke(instance, file)
    def setClasspath(cp: String): Unit = setClasspathMethod.invoke(instance, cp)
    def execute(): Unit                = executeMethod.invoke(instance)
  }

  def genBinTask(
    runner: ScopedTask[ScalaToolRunner], 
    outputDir: ScopedSetting[File], 
    classpath: ScopedTask[Classpath], 
    useClasspath: Boolean
  ): Project.Initialize[sbt.Task[Map[File,String]]] = {
    (runner, outputDir, classpath, streams) map { (runner, outDir, cp, s) =>
      IO.createDirectory(outDir)
      val classToFilename = Map(
        "scala.tools.nsc.MainGenericRunner" -> "scala",
        "scala.tools.nsc.Main"              -> "scalac",
        "scala.tools.nsc.ScalaDoc"          -> "scaladoc",
        "scala.tools.nsc.CompileClient"     -> "fsc",
        "scala.tools.scalap.Main"           -> "scalap"
      )
      if (useClasspath) { 
        val classpath = Build.data(cp).map(_.getCanonicalPath).distinct.mkString(",")
        s.log.debug("Setting classpath = " + classpath)
        runner setClasspath classpath
      }
      def genBinFiles(cls: String, dest: File) = {
        runner.setClass(cls)
        runner.setFile(dest)
        runner.execute()
        // TODO - Mark generated files as executable (755 or a+x) that is *not* JDK6 specific...
        dest.setExecutable(true)
      }
      def makeBinMappings(cls: String, binName: String): Map[File,String] = {
        val file       = outDir / binName
        val winBinName = binName + ".bat"
        genBinFiles(cls, file)
        Map( file -> ("bin/"+binName), outDir / winBinName -> ("bin/"+winBinName) )
      }
      classToFilename.flatMap((makeBinMappings _).tupled).toMap
    }
  }
  def runManmakerTask(classpath: ScopedTask[Classpath], scalaRun: ScopedTask[ScalaRun], mainClass: String, dir: String, ext: String): Project.Initialize[Task[Map[File,String]]] =
    (classpath, scalaRun, streams, target) map { (cp, runner, s, target) =>
      val binaries = Seq("fsc", "scala", "scalac", "scaladoc", "scalap")
      binaries map { bin =>
        val file = target / "man" / dir / (bin + ext)
        val classname = "scala.man1." + bin
        IO.createDirectory(file.getParentFile)
        toError(runner.run(mainClass, Build.data(cp), Seq(classname, file.getAbsolutePath), s.log))   
        file -> ("man/" + dir + "/" + bin + ext)
      } toMap
    }

  val genBinRunner = TaskKey[ScalaToolRunner]("gen-bin-runner", 
    "Creates a utility to generate script files for Scala.")  
  val genBin = TaskKey[Map[File,String]]("gen-bin",
    "Creates script files for Scala distribution.")
  val binDir = SettingKey[File]("binaries-directory",
    "Directory where binary scripts will be located.")
  val genBinQuick = TaskKey[Map[File,String]]("gen-quick-bin",
    "Creates script files for testing against current Scala build classfiles (not local dist).")
  val runManmakerMan = TaskKey[Map[File,String]]("make-man",
    "Runs the man maker project to generate man pages")
  val runManmakerHtml = TaskKey[Map[File,String]]("make-html",
    "Runs the man maker project to generate html pages")

  lazy val scalaDistSettings: Seq[Setting[_]] = Seq(
    crossPaths := false,
    target <<= (baseDirectory, name) apply (_ / "target" / _),
    scalaSource in Compile <<= (baseDirectory, name) apply (_ / "src" / _),
    autoScalaLibrary := false,
    unmanagedJars in Compile := Seq(),
    genBinRunner <<= (fullClasspath in quickComp in Runtime) map (new ScalaToolRunner(_)),
    binDir <<= target(_/"bin"),
    genBin <<= genBinTask(genBinRunner, binDir, fullClasspath in Runtime, false),
    binDir in genBinQuick <<= baseDirectory apply (_ / "target" / "bin"),
    // Configure the classpath this way to avoid having .jar files and previous layers on the classpath.
    fullClasspath in Runtime in genBinQuick <<= Seq(quickComp,quickLib,scalap,actors,swing,fjbg,jline,forkjoin).map(classDirectory in Compile in _).join.map(Attributed.blankSeq),
    fullClasspath in Runtime in genBinQuick <++= (fullClasspath in Compile in jline),
    genBinQuick <<= genBinTask(genBinRunner, binDir in genBinQuick, fullClasspath in Runtime in genBinQuick, true),
    runManmakerMan <<= runManmakerTask(fullClasspath in Runtime in manmaker, runner in manmaker, "scala.tools.docutil.EmitManPage", "man1", ".1"),
    runManmakerHtml <<= runManmakerTask(fullClasspath in Runtime in manmaker, runner in manmaker, "scala.tools.docutil.EmitHtml", "doc", ".html"),
    // TODO - We could *really* clean this up in many ways.   Let's look into making a a Seq of "direct jars" (scalaLibrary, scalaCompiler, jline, scalap)
    // a seq of "plugin jars" (continuationsPlugin) and "binaries" (genBin) and "documentation" mappings (genBin) that this can aggregate.
    // really need to figure out a better way to pull jline + jansi.
    makeDistMappings <<= (genBin, 
                          runManmakerMan,
                          runManmakerHtml,
                          packageBin in scalaLibrary in Compile, 
                          packageBin in scalaCompiler in Compile,
                          packageBin in jline in Compile,
                          packageBin in continuationsPlugin in Compile,
                          managedClasspath in jline in Compile,
                          packageBin in scalap in Compile) map {
      (binaries, man, html, lib, comp, jline, continuations, jlineDeps, scalap) =>
        val jlineDepMap: Seq[(File, String)] = jlineDeps.map(_.data).flatMap(_ x Path.flat) map { case(a,b) => a -> ("lib/"+b) }
        binaries ++ man ++ html ++ jlineDepMap ++ Seq(
          lib           -> "lib/scala-library.jar",
          comp          -> "lib/scala-compiler.jar",
          jline         -> "lib/jline.jar",
          continuations -> "misc/scala-devel/plugins/continuations.jar",
          scalap        -> "lib/scalap.jar"
        ) toMap
    },
    // Add in some more dependencies
    makeDistMappings <<= (makeDistMappings, 
                          packageBin in swing in Compile) map {
      (dist, s) =>
        dist ++ Seq(s -> "lib/scala-swing.jar")
    },
    makeDist <<= (makeDistMappings, baseDirectory, streams) map { (maps, dir, s) => 
      s.log.debug("Map = " + maps.mkString("\n")) 
      val file = dir / "target" / "scala-dist.zip"
      IO.zip(maps, file)
      s.log.info("Created " + file.getAbsolutePath)
      file
    },
    makeExplodedDist <<= (makeDistMappings, target, streams) map { (maps, dir, s) => 
      def sameFile(f: File, f2: File) = f.getCanonicalPath == f2.getCanonicalPath
      IO.createDirectory(dir)
      IO.copy(for {
       (file, name) <- maps
       val file2 = dir / name
       if !sameFile(file,file2)
      } yield (file, file2))
      // Hack to make binaries be executable.  TODO - Fix for JDK 5 and below...
      maps.values filter (_ startsWith "bin/") foreach (dir / _ setExecutable true)
      dir
    }
  )
  lazy val scaladist = (
    Project("dist", file("."))
    settings (scalaDistSettings: _*)
  )
}

/** Matcher to make updated remote project references easier. */
object ScopedExternalSetting {
  def unapply[T](s: Setting[_]): Option[(URI, AttributeKey[_], Setting[_])] =
    s.key.scope.project match {
      case Select(p @ ProjectRef(uri, _)) => Some((uri, s.key.key, s))
      case _                              => None
    }
}
