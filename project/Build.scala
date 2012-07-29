import sbt._
import Keys._
import partest._
import ScalaBuildKeys._
import Release._


object ScalaBuild extends Build with Layers with Packaging with Testing {

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
    pomExtra := epflPomExtra
  ) 

  // Collections of projects to run 'compile' on.
  lazy val compiledProjects = Seq(quickLib, quickComp, continuationsLibrary, actors, actorsMigration, swing, forkjoin, fjbg)
  // Collection of projects to 'package' and 'publish' together.
  lazy val packagedBinaryProjects = Seq(scalaLibrary, scalaCompiler, swing, actors, actorsMigration, continuationsPlugin, jline, scalap)
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

  def fixArtifactSrc(dir: File, name: String) = name match {
    case x if x startsWith "scala-" => dir / "src" / (name drop 6)
    case x                          => dir / "src" / name
  }

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
    scalaSource in Compile <<= (baseDirectory, name) apply fixArtifactSrc,
    javaSource in Compile <<= (baseDirectory, name) apply fixArtifactSrc,
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
  lazy val (lockerLib, lockerReflect, lockerComp) = makeLayer("locker", STARR, autoLock = true)
  lazy val locker = Project("locker", file(".")) aggregate(lockerLib, lockerReflect, lockerComp)

  // Quick is the general purpose project layer for the Scala compiler.
  lazy val (quickLib, quickReflect, quickComp) = makeLayer("quick", makeScalaReference("locker", lockerLib, lockerReflect, lockerComp))
  lazy val quick = Project("quick", file(".")) aggregate(quickLib, quickReflect, quickComp)

  // Reference to quick scala instance.
  lazy val quickScalaInstance = makeScalaReference("quick", quickLib, quickReflect, quickComp)
  def quickScalaLibraryDependency = unmanagedClasspath in Compile <++= (exportedProducts in quickLib in Compile).identity
  def quickScalaReflectDependency = unmanagedClasspath in Compile <++= (exportedProducts in quickReflect in Compile).identity
  def quickScalaCompilerDependency = unmanagedClasspath in Compile <++= (exportedProducts in quickComp in Compile).identity

  // Strapp is used to test binary 'sameness' between things built with locker and things built with quick.
  lazy val (strappLib, strappReflect, strappComp) = makeLayer("strapp", quickScalaInstance)

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
  lazy val dependentProjectSettings = settingOverrides ++ Seq(quickScalaInstance, quickScalaLibraryDependency, addCheaterDependency("scala-library"))
  lazy val actors = Project("scala-actors", file(".")) settings(dependentProjectSettings:_*) dependsOn(forkjoin % "provided")
  lazy val swing = Project("scala-swing", file(".")) settings(dependentProjectSettings:_*) dependsOn(actors % "provided")
  lazy val actorsMigration = Project("scala-actors-migration", file(".")) settings(dependentProjectSettings:_*) dependsOn(actors % "provided")
  // This project will generate man pages (in man1 and html) for scala.    
  lazy val manmakerSettings: Seq[Setting[_]] = dependentProjectSettings :+ externalDeps
  lazy val manmaker = Project("manual", file(".")) settings(manmakerSettings:_*)

  // Things that compile against the compiler.
  lazy val compilerDependentProjectSettings = dependentProjectSettings ++ Seq(quickScalaReflectDependency, quickScalaCompilerDependency, addCheaterDependency("scala-compiler"))

  lazy val scalacheck = Project("scalacheck", file(".")) settings(compilerDependentProjectSettings:_*) dependsOn(actors % "provided")
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
  lazy val packageScalaLibBinTask = Seq(quickLib, continuationsLibrary, forkjoin).map(p => products in p in Compile).join.map(_.flatten).map(productTaskToMapping)
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
  //  Real Reflect Artifact
  // --------------------------------------------------------------

  lazy val packageScalaReflect = Seq(quickReflect).map(p => products in p in Compile).join.map(_.flatten).map(productTaskToMapping)
  lazy val scalaReflectArtifactSettings : Seq[Setting[_]] = inConfig(Compile)(Defaults.packageTasks(packageBin, packageScalaReflect)) ++ Seq(
    name := "scala-reflect",
    crossPaths := false,
    exportJars := true,
    autoScalaLibrary := false,
    unmanagedJars in Compile := Seq(),
    fullClasspath in Runtime <<= (exportedProducts in Compile).identity,
    quickScalaInstance,
    target <<= (baseDirectory, name) apply (_ / "target" / _)
  )
  lazy val scalaReflect = Project("scala-reflect", file(".")) settings(publishSettings:_*) settings(scalaReflectArtifactSettings:_*) dependsOn(scalaLibrary)


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
  lazy val scalaCompiler = Project("scala-compiler", file(".")) settings(publishSettings:_*) settings(scalaBinArtifactSettings:_*) dependsOn(scalaReflect)
  lazy val fullQuickScalaReference = makeScalaReference("pack", scalaLibrary, scalaReflect, scalaCompiler)

  
  // --------------------------------------------------------------
  //  Generating Documentation.
  // --------------------------------------------------------------
  
  // TODO - Migrate this into the dist project.
  // Scaladocs
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
}
