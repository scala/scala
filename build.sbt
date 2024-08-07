/*
 * The new, sbt-based build definition for Scala.
 *
 * What you see below is very much work-in-progress. The following features are implemented:
 *   - Compiling all classes for the compiler and library ("compile" in the respective subprojects)
 *   - Running JUnit ("junit/test"), ScalaCheck ("scalacheck/test"), and partest ("test/IntegrationTest/test") tests
 *   - Creating build/quick with all compiled classes and launcher scripts ("dist/mkQuick")
 *   - Creating build/pack with all JARs and launcher scripts ("dist/mkPack")
 *   - Building all scaladoc sets ("doc")
 *   - Publishing (standard sbt tasks like "publish" and "publishLocal")
 *
 * You'll notice that this build definition is much more complicated than your typical sbt build.
 * The main reason is that we are not benefiting from sbt's conventions when it comes project
 * layout. For that reason we have to configure a lot more explicitly. I've tried to explain in
 * comments the less obvious settings.
 *
 * This nicely leads me to explain the goal and non-goals of this build definition. Goals are:
 *
 *   - to be easy to tweak it in case a bug or small inconsistency is found
 *   - to be super explicit about any departure from standard sbt settings
 *   - to be readable and not necessarily succinct
 *   - to provide the nicest development experience for people hacking on Scala
 *   - originally, to mimic Ant's behavior as closely as possible, so the
 *     sbt and Ant builds could be maintained in parallel. the Ant build
 *     has now been removed, so we are now free to depart from that history.
 *
 * Non-goals are:
 *
 *   - to have the shortest sbt build definition possible
 *   - to remove irregularities from our build process right away
 *     (but let's keep making gradual progress on this)
 *   - to modularize the Scala compiler or library further
 */

import scala.build._, VersionUtil._

// Non-Scala dependencies:
val junitDep          = "junit"                          % "junit"                            % "4.13.2"
val junitInterfaceDep = "com.github.sbt"                 % "junit-interface"                  % "0.13.3"                          % Test
val scalacheckDep     = "org.scalacheck"                %% "scalacheck"                       % "1.18.0"                          % Test
val jolDep            = "org.openjdk.jol"                % "jol-core"                         % "0.16"
val asmDep            = "org.scala-lang.modules"         % "scala-asm"                        % versionProps("scala-asm.version")
val jlineDep          = "org.jline"                      % "jline"                            % versionProps("jline.version")
val jnaDep            = "net.java.dev.jna"               % "jna"                              % versionProps("jna.version")
val jlineDeps         = Seq(jlineDep, jnaDep)
val testInterfaceDep  = "org.scala-sbt"                  % "test-interface"                   % "1.0"
val diffUtilsDep      = "io.github.java-diff-utils"      % "java-diff-utils"                  % "4.12"
val compilerInterfaceDep = "org.scala-sbt"               % "compiler-interface"               % "1.10.1"

val projectFolder = settingKey[String]("subfolder in src when using configureAsSubproject, else the project name")

// `set Global / fatalWarnings := true` to enable -Werror for the certain modules
// currently, many modules cannot support -Werror; ideally this setting will eventually
//   enable -Werror for all modules
val fatalWarnings = settingKey[Boolean]("whether or not warnings should be fatal in the build")

// enable fatal warnings automatically on CI
Global / fatalWarnings := insideCI.value

Global / credentials ++= {
  val file = Path.userHome / ".credentials"
  if (file.exists && !file.isDirectory) List(Credentials(file))
  else Nil
}

lazy val publishSettings : Seq[Setting[_]] = Seq(
  // Add a "default" Ivy configuration because sbt expects the Scala distribution to have one:
  ivyConfigurations += Configuration.of("Default", "default", "Default", true, Vector(Configurations.Runtime), true),
  publishMavenStyle := true
)

// Set the version number: We use the two settings `baseVersion` and `baseVersionSuffix` to compute all versions
// (canonical, Maven, OSGi). See VersionUtil.versionPropertiesImpl for details. The standard sbt `version` setting
// should not be set directly. It is the same as the Maven version and derived automatically from `baseVersion` and
// `baseVersionSuffix`.
globalVersionSettings
Global / baseVersion       := "2.13.15"
Global / baseVersionSuffix := "SNAPSHOT"
ThisBuild / organization   := "org.scala-lang"
ThisBuild / homepage       := Some(url("https://www.scala-lang.org"))
ThisBuild / startYear      := Some(2002)
ThisBuild / licenses       += (("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0")))
ThisBuild / headerLicense  := Some(HeaderLicense.Custom(
  s"""Scala (${(ThisBuild/homepage).value.get})
     |
     |Copyright EPFL and Lightbend, Inc.
     |
     |Licensed under Apache License 2.0
     |(http://www.apache.org/licenses/LICENSE-2.0).
     |
     |See the NOTICE file distributed with this work for
     |additional information regarding copyright ownership.
     |""".stripMargin
))

// Save MiMa logs
SavedLogs.settings

Global / scalaVersion      := {
  if (DottySupport.compileWithDotty)
    DottySupport.dottyVersion
  else
    versionProps("starr.version")
}

lazy val instanceSettings = Seq[Setting[_]](
  // we don't cross build Scala itself
  crossPaths := false,
  // do not add Scala library jar as a dependency automatically
  autoScalaLibrary := false,
  // Avoid circular dependencies for scalaInstance (see https://github.com/sbt/sbt/issues/1872)
  managedScalaInstance := false,
  scalaInstance := {
    val s = (bootstrap / scalaInstance).value
    // sbt claims that s.isManagedVersion is false even though s was resolved by Ivy
    // We create a managed copy to prevent sbt from putting it on the classpath where we don't want it
    if(s.isManagedVersion) s else {
      import sbt.internal.inc.ScalaInstance
      val s2 = new ScalaInstance(s.version, s.loader, s.loaderCompilerOnly, s.loaderLibraryOnly, s.libraryJars, s.compilerJars, s.allJars, Some(s.actualVersion))
      assert(s2.isManagedVersion)
      s2
    }
  },
  // sbt endeavours to align both scalaOrganization and scalaVersion
  // in the Scala artefacts, for example scala-library and scala-compiler.
  // This doesn't work in the scala/scala build because the version of scala-library and the scalaVersion of
  // scala-library are correct to be different. So disable overriding.
  scalaModuleInfo ~= (_ map (_ withOverrideScalaVersion false)),
  Quiet.silenceScalaBinaryVersionWarning
)


lazy val commonSettings = instanceSettings ++ clearSourceAndResourceDirectories ++ publishSettings ++ Seq[Setting[_]](
  // we always assume that Java classes are standalone and do not have any dependency
  // on Scala classes
  compileOrder := CompileOrder.JavaThenScala,
  projectFolder := thisProject.value.id, // overridden in configureAsSubproject
  Compile / javacOptions ++= Seq("-g", "-source", "1.8", "-target", "1.8", "-Xlint:unchecked"),
  Compile / javacOptions ++= (
    if (scala.util.Properties.isJavaAtLeast("20"))
      Seq("-Xlint:-options")  // allow `-source 1.8` and `-target 1.8`
    else
      Seq()),
  Compile / unmanagedJars := Seq.empty,  // no JARs in version control!
  Compile / sourceDirectory := baseDirectory.value,
  Compile / unmanagedSourceDirectories := List(baseDirectory.value),
  Compile / unmanagedResourceDirectories += (ThisBuild / baseDirectory).value / "src" / projectFolder.value,
  sourcesInBase := false,
  Compile / scalaSource := (Compile / sourceDirectory).value,
  // for some reason sbt 1.4 issues unused-settings warnings for this, it seems to me incorrectly
  Global / excludeLintKeys ++= Set(scalaSource),
  // each subproject has to ask specifically for files they want to include
  Compile / unmanagedResources / includeFilter := NothingFilter,
  target := (ThisBuild / target).value / projectFolder.value,
  Compile / classDirectory := buildDirectory.value / "quick/classes" / projectFolder.value,
  Compile / doc / target := buildDirectory.value / "scaladoc" / projectFolder.value,
  // given that classDirectory and doc target are overridden to be _outside_ of target directory, we have
  // to make sure they are being cleaned properly
  cleanFiles += (Compile / classDirectory).value,
  cleanFiles += (Compile / doc / target).value,
  run / fork := true,
  run / connectInput := true,
  Compile / scalacOptions ++= Seq("-feature", "-Xlint",
    //"-Xmaxerrs", "5", "-Xmaxwarns", "5", // uncomment for ease of development while breaking things
    // work around https://github.com/scala/bug/issues/11534
    "-Wconf:cat=unchecked&msg=The outer reference in this type test cannot be checked at run time.:s",
    // optimizer warnings at INFO since `-Werror` may be turned on.
    // optimizer runs in CI and release builds, though not in local development.
    "-Wconf:cat=optimizer:is",
    // we use @nowarn for methods that are deprecated in JDK > 8, but CI/release is under JDK 8
    "-Wconf:cat=unused-nowarn:s",
    //"-Wunnamed-boolean-literal-strict",
    ),
  Compile / doc / scalacOptions ++= Seq(
    "-doc-footer", "epfl",
    "-diagrams",
    "-implicits",
    "-groups",
    "-doc-version", versionProperties.value.canonicalVersion,
    "-doc-title", description.value,
    "-sourcepath", (ThisBuild / baseDirectory).value.toString,
    "-doc-source-url", s"https://github.com/scala/scala/blob/${versionProperties.value.githubTree}/€{FILE_PATH_EXT}#L€{FILE_LINE}"
  ),
  //maxErrors := 10,
  setIncOptions,
  // http://stackoverflow.com/questions/16934488
  apiMappings ++= {
    Option(System.getProperty("sun.boot.class.path")).flatMap { classPath =>
      classPath.split(java.io.File.pathSeparator).find(_.endsWith(java.io.File.separator + "rt.jar"))
    }.map { jarPath =>
      Map(
        file(jarPath) -> url("https://docs.oracle.com/javase/8/docs/api")
      )
    }.getOrElse {
      streams.value.log.warn("Failed to add bootstrap class path of Java to apiMappings")
      Map.empty[File,URL]
    }
  },
  apiURL := None, // set on a per-project basis
  autoAPIMappings := true,
  pomIncludeRepository := { _ => false },
  pomExtra := {
    <scm>
      <connection>scm:git:git://github.com/scala/scala.git</connection>
      <url>https://github.com/scala/scala</url>
    </scm>
      <issueManagement>
        <system>GitHub</system>
        <url>https://github.com/scala/bug/issues</url>
      </issueManagement>
      <developers>
        <developer>
          <id>lamp</id>
          <name>LAMP/EPFL</name>
        </developer>
        <developer>
          <id>Lightbend</id>
          <name>Lightbend, Inc.</name>
        </developer>
      </developers>
  },
  headerLicense := (ThisBuild / headerLicense).value,
  // Remove auto-generated manifest attributes
  Compile / packageBin / packageOptions := Seq.empty,
  Compile / packageSrc / packageOptions := Seq.empty,

  // Lets us CTRL-C partest without exiting SBT entirely
  Global / cancelable := true,

  // Don't log process output (e.g. of forked `compiler/runMain ...Main`), just pass it
  // directly to stdout
  run / outputStrategy := Some(StdoutOutput)
) ++ removePomDependencies ++ setForkedWorkingDirectory ++ (
  if (DottySupport.compileWithDotty)
    DottySupport.commonSettings
  else
    Seq()
)

lazy val fatalWarningsSettings = Seq(
  Compile / scalacOptions ++= {
    if (fatalWarnings.value) Seq("-Werror")
    else Nil
  },
  Compile / javacOptions ++= {
    if (fatalWarnings.value) Seq("-Werror")
    else Nil
  },
  Compile / doc / scalacOptions -= "-Werror", // there are too many doc errors to enable this right now
)

/** Extra post-processing for the published POM files. These are needed to create POMs that
  * are equivalent to the ones from the old Ant build. In the long term this should be removed and
  * POMs, scaladocs, OSGi manifests, etc. should all use the same metadata. */
def fixPom(extra: (String, scala.xml.Node)*): Setting[_] = {
  /** Find elements in an XML document by a simple XPath and replace them */
  def fixXML(n: scala.xml.Node, repl: Map[String, scala.xml.Node]): scala.xml.Node = {
    def f(n: scala.xml.Node, p: String): scala.xml.Node = n match {
      case e: scala.xml.Elem =>
        val pp = p + "/" + e.label
        repl.get(pp) match {
          case Some(xml) => xml
          case None => e.copy(child = e.child.map(ch => f(ch, pp)))
        }
      case n => n
    }
    f(n, "")
  }
  pomPostProcess := { n => fixXML(pomPostProcess.value.apply(n), Map(
    "/project/organization" ->
      <organization>
        <name>LAMP/EPFL</name>
        <url>https://lamp.epfl.ch/</url>
      </organization>,
    "/project/url" -> <url>https://www.scala-lang.org/</url>
  ) ++ extra) }
}

def ivyDependencyFilter(deps: Seq[(String, String)], scalaBinaryVersion: String) = {
  import scala.xml._
  import scala.xml.transform._
  new RuleTransformer(new RewriteRule {
    override def transform(node: Node) = node match {
      case e: Elem if e.label == "dependency" && {
        val org = e.attribute("org").getOrElse("").toString
        val name = e.attribute("name").getOrElse("").toString
        deps.exists { case (g, a) =>
          org == g && (name == a || name == (a + "_" + scalaBinaryVersion))
        }
      } => Seq.empty
      case n => n
    }
  })
}

val pomDependencyExclusions =
  settingKey[Seq[(String, String)]]("List of (groupId, artifactId) pairs to exclude from the POM and ivy.xml")
lazy val fixCsrIvy = taskKey[Unit]("Apply pomDependencyExclusions to coursier ivy")

Global / pomDependencyExclusions := Nil

/** Remove unwanted dependencies from the POM and ivy.xml. */
lazy val removePomDependencies: Seq[Setting[_]] = Seq(
  pomPostProcess := { n =>
    val n2 = pomPostProcess.value.apply(n)
    val deps = pomDependencyExclusions.value
    import scala.xml._
    import scala.xml.transform._
    new RuleTransformer(new RewriteRule {
      override def transform(node: Node) = node match {
        case e: Elem if e.label == "dependency" &&
            deps.exists { case (g, a) =>
              e.child.contains(<groupId>{g}</groupId>) &&
                (e.child.contains(<artifactId>{a}</artifactId>) || e.child.contains(<artifactId>{a + "_" + scalaBinaryVersion.value}</artifactId>))
            } => Seq.empty
        case n => n
      }
    }).transform(Seq(n2)).head
  },
  fixCsrIvy := {
    //  - coursier makes target/sbt-bridge/resolution-cache/org.scala-lang/scala2-sbt-bridge/2.13.12-bin-SNAPSHOT/resolved.xml.xml
    //  - copied to target/sbt-bridge//ivy-2.13.12-bin-SNAPSHOT.xml
    //  - copied to ~/.ivy2/local/org.scala-lang/scala2-sbt-bridge/2.13.12-bin-SNAPSHOT/ivys/ivy.xml
    import scala.jdk.CollectionConverters._
    import scala.xml._
    val currentProject = csrProject.value
    val ivyModule = org.apache.ivy.core.module.id.ModuleRevisionId.newInstance(
      currentProject.module.organization.value,
      currentProject.module.name.value,
      currentProject.version,
      currentProject.module.attributes.asJava)
    val ivyFile = ivySbt.value.withIvy(streams.value.log)(_.getResolutionCacheManager).getResolvedIvyFileInCache(ivyModule)
    val e = ivyDependencyFilter(pomDependencyExclusions.value, scalaBinaryVersion.value)
      .transform(Seq(XML.loadFile(ivyFile))).head
    XML.save(ivyFile.getAbsolutePath, e, xmlDecl = true)
  },
  publishConfiguration := Def.taskDyn {
    val pc = publishConfiguration.value
    Def.task {
      fixCsrIvy.value
      pc
    }
  }.value,
  publishLocalConfiguration := Def.taskDyn {
    val pc = publishLocalConfiguration.value
    Def.task {
      fixCsrIvy.value
      pc
    }
  }.value,
  deliverLocal := {
    // this doesn't seem to do anything currently, it probably worked before sbt used coursier
    import scala.xml._
    val f = deliverLocal.value
    val e = ivyDependencyFilter(pomDependencyExclusions.value, scalaBinaryVersion.value)
      .transform(Seq(XML.loadFile(f))).head
    XML.save(f.getAbsolutePath, e, xmlDecl = true)
    f
  }
)

val disableDocs = Seq[Setting[_]](
  Compile / doc / sources := Seq.empty,
  Compile / packageDoc / publishArtifact := false
)

lazy val setJarLocation: Setting[_] =
  Compile / packageBin / artifactPath := {
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

def filterDocSources(ff: FileFilter): Seq[Setting[_]] = Seq(
  Compile / doc / sources ~= (_.filter(ff.accept)),
  // Excluded sources may still be referenced by the included sources, so we add the compiler
  // output to the scaladoc classpath to resolve them. For the `library` project this is
  // always required because otherwise the compiler cannot even initialize Definitions without
  // binaries of the library on the classpath. Specifically, we get this error:
  // (library/compile:doc) scala.reflect.internal.FatalError: package class scala does not have a member Int
  Compile / doc / dependencyClasspath += (Compile / classDirectory).value,
  Compile / doc := (Compile / doc).dependsOn(Compile / compile).value
)

def regexFileFilter(s: String): FileFilter = new FileFilter {
  val pat = s.r.pattern
  def accept(f: File) = pat.matcher(f.getAbsolutePath.replace('\\', '/')).matches()
}

def setForkedWorkingDirectory: Seq[Setting[_]] = {
  // When we fork subprocesses, use the base directory as the working directory.
  // This enables `sbt> partest test/files/run/t1.scala` or `sbt> scalac sandbox/test.scala`
  val setting = (Compile / forkOptions) := (Compile / forkOptions).value.withWorkingDirectory((ThisBuild / baseDirectory).value)
  setting ++ inTask(run)(setting)
}

// This project provides the STARR scalaInstance for bootstrapping
lazy val bootstrap = project.in(file("target/bootstrap")).settings(bspEnabled := false)

lazy val library = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings)
  .settings(Osgi.settings)
  .settings(AutomaticModuleName.settings("scala.library"))
  .settings(fatalWarningsSettings)
  .settings(
    name := "scala-library",
    description := "Scala Standard Library",
    Compile / scalacOptions ++= Seq("-sourcepath", (Compile / scalaSource).value.toString),
    Compile / doc / scalacOptions ++= {
      val libraryAuxDir = (ThisBuild / baseDirectory).value / "src/library-aux"
      Seq(
        "-doc-no-compile", libraryAuxDir.toString,
        "-skip-packages", "scala.concurrent.impl",
        "-doc-root-content", (Compile / sourceDirectory).value + "/rootdoc.txt",
        //"-required", // placeholder for internal flag
      )
    },
    Compile / console / scalacOptions := {
      val opts = (console / scalacOptions).value
      val ix = (console / scalacOptions).value.indexOfSlice(Seq[String]("-sourcepath", (Compile / scalaSource).value.toString))
      opts.patch(ix, Nil, 2)
    },
    Compile / unmanagedResources / includeFilter := "*.tmpl" | "*.xml" | "*.js" | "*.css" | "rootdoc.txt",
    // Include *.txt files in source JAR:
    Compile / packageSrc / mappings ++= {
      val base = (Compile / unmanagedResourceDirectories).value
      (base ** "*.txt" pair Path.relativeTo(base)) ++ {
        val auxBase = (ThisBuild / baseDirectory).value / "src/library-aux"
        auxBase ** ("*.scala" || "*.java") pair Path.relativeTo(auxBase)
      }
    },
    Osgi.headers += "Import-Package" -> "sun.misc;resolution:=optional, *",
    Osgi.jarlist := true,
    fixPom(
      "/project/name" -> <name>Scala Library</name>,
      "/project/description" -> <description>Standard library for the Scala Programming Language</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    ),
    apiURL := Some(url(s"https://www.scala-lang.org/api/${versionProperties.value.mavenVersion}/")),
    MimaFilters.mimaSettings,
  )
  .settings(filterDocSources("*.scala" -- regexFileFilter(".*/scala/runtime/.*")))
  .settings(
    if (DottySupport.compileWithDotty)
      DottySupport.librarySettings
    else
      Seq()
  )

lazy val reflect = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings)
  .settings(Osgi.settings)
  .settings(AutomaticModuleName.settings("scala.reflect"))
  .settings(fatalWarningsSettings)
  .settings(
    name := "scala-reflect",
    description := "Scala Reflection Library",
    Osgi.bundleName := "Scala Reflect",
    Compile / doc / scalacOptions ++= Seq(
      "-skip-packages", "scala.reflect.macros.internal:scala.reflect.internal:scala.reflect.io"
    ),
    Osgi.headers +=
      "Import-Package" -> (raw"""scala.*;version="$${range;[==,=+);$${ver}}",""" +
                           raw"""scala.tools.nsc;resolution:=optional;version="$${range;[==,=+);$${ver}}",""" +
                           "*"),
    fixPom(
      "/project/name" -> <name>Scala Reflect</name>,
      "/project/description" -> <description>Reflection Library for the Scala Programming Language</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    ),
    apiURL := Some(url(s"https://www.scala-lang.org/api/${versionProperties.value.mavenVersion}/scala-${projectFolder.value}/")),
    MimaFilters.mimaSettings,
  )
  .dependsOn(library)

lazy val compiler = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings)
  .settings(generateBuildCharacterFileSettings)
  .settings(Osgi.settings)
  .settings(AutomaticModuleName.settings("scala.tools.nsc"))
  .settings(fatalWarningsSettings)
  .settings(
    name := "scala-compiler",
    description := "Scala Compiler",
    libraryDependencies += asmDep,
    libraryDependencies += diffUtilsDep,
    // These are only needed for the POM:
    // TODO: jline dependency is only needed for the REPL shell, which should move to its own jar
    libraryDependencies ++= jlineDeps,
    buildCharacterPropertiesFile := (Compile / resourceManaged).value / "scala-buildcharacter.properties",
    Compile / resourceGenerators += generateBuildCharacterPropertiesFile.map(file => Seq(file)).taskValue,
    // this a way to make sure that classes from interactive and scaladoc projects
    // end up in compiler jar. note that we need to use LocalProject references
    // (with strings) to deal with mutual recursion
    Compile / packageBin / products :=
      (Compile / packageBin / products).value ++
        (Compile / dependencyClasspath).value.filter(_.get(moduleID.key).map(id => (id.organization, id.name, id.revision)) match {
          case Some((diffUtilsDep.organization, diffUtilsDep.name, diffUtilsDep.revision)) => true
          case Some((asmDep.organization, asmDep.name, asmDep.revision)) => true
          case _ => false
        }).map(_.data) ++
        (LocalProject("interactive")   / Compile / packageBin / products).value ++
        (LocalProject("scaladoc")      / Compile / packageBin / products).value ++
        (LocalProject("repl")          / Compile / packageBin / products).value ++
        (LocalProject("replFrontend")  / Compile / packageBin / products).value,
    Compile / unmanagedResources / includeFilter :=
      "*.tmpl" | "*.xml" | "*.js" | "*.css" | "*.html" | "*.properties" | "*.swf" |
      "*.png" | "*.gif" | "*.gif" | "*.txt",
    // Also include the selected unmanaged resources and source files from the additional projects in the source JAR:
    Compile / packageSrc / mappings ++= {
      val base = (Compile / unmanagedResourceDirectories).value ++
        (LocalProject("interactive")   / Compile / unmanagedResourceDirectories).value ++
        (LocalProject("scaladoc")      / Compile / unmanagedResourceDirectories).value ++
        (LocalProject("repl")          / Compile / unmanagedResourceDirectories).value ++
        (LocalProject("replFrontend")  / Compile / unmanagedResourceDirectories).value
      base ** ((Compile / unmanagedResources / includeFilter).value || "*.scala" || "*.psd" || "*.ai" || "*.java") pair Path.relativeTo(base)
    },
    // Include the additional projects in the scaladoc JAR:
    Compile / doc / sources ++= {
      val base =
        (LocalProject("interactive")   / Compile / unmanagedSourceDirectories).value ++
        (LocalProject("scaladoc")      / Compile / unmanagedSourceDirectories).value ++
        (LocalProject("repl")          / Compile / unmanagedSourceDirectories).value ++
        (LocalProject("replFrontend")  / Compile / unmanagedSourceDirectories).value
      ((base ** ("*.scala" || "*.java"))
        --- (base ** "Scaladoc*ModelTest.scala") // exclude test classes that depend on partest
      ).get
    },
    Compile / scalacOptions ++= Seq(
      "-Wconf:cat=deprecation&msg=early initializers:s", // compiler heavily relies upon early initializers
    ),
    Compile / doc / scalacOptions ++= Seq(
      "-doc-root-content", (Compile / sourceDirectory).value + "/rootdoc.txt"
    ),
    Osgi.headers ++= Seq(
      "Import-Package" -> raw"""org.jline.keymap.*;resolution:=optional
                            |org.jline.reader.*;resolution:=optional
                            |org.jline.style.*;resolution:=optional
                            |org.jline.terminal;resolution:=optional
                            |org.jline.terminal.impl;resolution:=optional
                            |org.jline.terminal.impl.jna.*;resolution:=optional
                            |org.jline.terminal.spi;resolution:=optional
                            |org.jline.utils;resolution:=optional
                            |org.jline.builtins;resolution:=optional
                            |scala.*;version="$${range;[==,=+);$${ver}}"
                            |*""".stripMargin.linesIterator.mkString(","),
      "Class-Path" -> "scala-reflect.jar scala-library.jar"
    ),
    // Generate the ScriptEngineFactory service definition. The old Ant build did this when building
    // the JAR but sbt has no support for it and it is easier to do as a resource generator:
    generateServiceProviderResources("javax.script.ScriptEngineFactory" -> "scala.tools.nsc.interpreter.shell.Scripted$Factory"),
    Compile / managedResourceDirectories := Seq((Compile / resourceManaged).value),
    fixPom(
      "/project/name" -> <name>Scala Compiler</name>,
      "/project/description" -> <description>Compiler for the Scala Programming Language</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    ),
    apiURL := Some(url(s"https://www.scala-lang.org/api/${versionProperties.value.mavenVersion}/scala-${projectFolder.value}/")),
    pomDependencyExclusions += (("org.scala-lang.modules", "scala-asm"))
  )
  .dependsOn(library, reflect)

lazy val interactive = configureAsSubproject(project)
  .settings(disableDocs)
  .settings(fatalWarningsSettings)
  .settings(publish / skip := true)
  .settings(
    name := "scala-compiler-interactive",
    description := "Scala Interactive Compiler",
    Compile / scalacOptions ++= Seq("-Wconf:cat=deprecation&msg=early initializers:s"),
  )
  .dependsOn(compiler)

lazy val repl = configureAsSubproject(project)
  .settings(disableDocs)
  .settings(fatalWarningsSettings)
  .settings(publish / skip := true)
  .settings(Compile / scalacOptions ++= Seq("-Wconf:cat=deprecation&msg=early initializers:s"))
  .dependsOn(compiler, interactive)

lazy val replFrontend = configureAsSubproject(project, srcdir = Some("repl-frontend"))
  .settings(disableDocs)
  .settings(fatalWarningsSettings)
  .settings(publish / skip := true)
  .settings(
    libraryDependencies ++= jlineDeps,
    name := "scala-repl-frontend",
  )
  .settings(
    run := (Compile / run).partialInput(" -usejavacp").evaluated, // so `replFrontend/run` works
    Compile / run / javaOptions += s"-Dscala.color=${!scala.util.Properties.isWin}",
    Compile / run / javaOptions += "-Dorg.jline.terminal.output=forced-out",
  )
  .dependsOn(repl)

lazy val scaladoc = configureAsSubproject(project)
  .settings(disableDocs)
  .settings(fatalWarningsSettings)
  .settings(publish / skip := true)
  .settings(
    name := "scala-compiler-doc",
    description := "Scala Documentation Generator",
    Compile / unmanagedResources / includeFilter := "*.html" | "*.css" | "*.gif" | "*.png" | "*.js" | "*.txt" | "*.svg" | "*.eot" | "*.woff" | "*.ttf",
    libraryDependencies ++= ScaladocSettings.webjarResources,
    Compile / resourceGenerators += ScaladocSettings.extractResourcesFromWebjar,
    Compile / scalacOptions ++= Seq(
      "-Wconf:cat=deprecation&msg=early initializers:s",
    ),
  )
  .dependsOn(compiler)

// dependencies on compiler and compiler-interface are "provided" to align with scala3-sbt-bridge
lazy val sbtBridge = configureAsSubproject(project, srcdir = Some("sbt-bridge"))
  .settings(Osgi.settings)
  .settings(AutomaticModuleName.settings("scala.sbtbridge"))
  //.settings(fatalWarningsSettings)
  .settings(
    name := "scala2-sbt-bridge",
    description := "sbt compiler bridge for Scala 2",
    libraryDependencies += compilerInterfaceDep % Provided,
    Compile / scalacOptions ++= Seq(
      "-Wconf:cat=deprecation&msg=early initializers:s", // compiler heavily relies upon early initializers
    ),
    generateServiceProviderResources("xsbti.compile.CompilerInterface2" -> "scala.tools.xsbt.CompilerBridge"),
    generateServiceProviderResources("xsbti.compile.ConsoleInterface1"  -> "scala.tools.xsbt.ConsoleBridge"),
    generateServiceProviderResources("xsbti.compile.ScaladocInterface2" -> "scala.tools.xsbt.ScaladocBridge"),
    generateServiceProviderResources("xsbti.InteractiveConsoleFactory"  -> "scala.tools.xsbt.InteractiveConsoleBridgeFactory"),
    Compile / managedResourceDirectories := Seq((Compile / resourceManaged).value),
    pomDependencyExclusions ++= List((organization.value, "scala-repl-frontend"), (organization.value, "scala-compiler-doc")),
    fixPom(
      "/project/name" -> <name>Scala 2 sbt Bridge</name>,
      "/project/description" -> <description>sbt compiler bridge for Scala 2</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    ),
    headerLicense := Some(HeaderLicense.Custom(
      s"""Zinc - The incremental compiler for Scala.
         |Copyright Scala Center, Lightbend, and Mark Harrah
         |
         |Scala (${(ThisBuild/homepage).value.get})
         |Copyright EPFL and Lightbend, Inc.
         |
         |Licensed under Apache License 2.0
         |(http://www.apache.org/licenses/LICENSE-2.0).
         |
         |See the NOTICE file distributed with this work for
         |additional information regarding copyright ownership.
         |""".stripMargin)),
  )
  .dependsOn(compiler % Provided, replFrontend, scaladoc)

lazy val scalap = configureAsSubproject(project)
  .settings(fatalWarningsSettings)
  .settings(
    description := "Scala Bytecode Parser",
    // Include decoder.properties
    Compile / unmanagedResources / includeFilter := "*.properties",
    fixPom(
      "/project/name" -> <name>Scalap</name>,
      "/project/description" -> <description>bytecode analysis tool</description>,
      "/project/properties" -> scala.xml.Text("")
    ),
    headerLicense  := Some(HeaderLicense.Custom(
      s"""Scala classfile decoder (${(ThisBuild/homepage).value.get})
         |
         |Copyright EPFL and Lightbend, Inc.
         |
         |Licensed under Apache License 2.0
         |(http://www.apache.org/licenses/LICENSE-2.0).
         |
         |See the NOTICE file distributed with this work for
         |additional information regarding copyright ownership.
         |""".stripMargin)),
    Compile / headerSources ~= { xs =>
      val excluded = Set("Memoisable.scala", "Result.scala", "Rule.scala", "Rules.scala", "SeqRule.scala")
      xs filter { x => !excluded(x.getName) }
    },
    Compile / headerResources := Nil,
  )
  .dependsOn(compiler)

lazy val partest = configureAsSubproject(project)
  .dependsOn(library, reflect, compiler, replFrontend, scalap, scaladoc, testkit)
  .settings(Osgi.settings)
  .settings(AutomaticModuleName.settings("scala.partest"))
  .settings(fatalWarningsSettings)
  .settings(
    name := "scala-partest",
    description := "Scala Compiler Testing Tool",
    libraryDependencies ++= List(testInterfaceDep, diffUtilsDep, junitDep),
    Compile / javacOptions ++= Seq("-XDenableSunApiLintControl", "-Xlint") ++
      (if (fatalWarnings.value) Seq("-Werror") else Seq()),
    pomDependencyExclusions ++= List((organization.value, "scala-repl-frontend"), (organization.value, "scala-compiler-doc")),
    fixPom(
      "/project/name" -> <name>Scala Partest</name>,
      "/project/description" -> <description>Scala Compiler Testing Tool</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    )
  )

lazy val tastytest = configureAsSubproject(project)
  .dependsOn(library, reflect, compiler, scaladoc)
  .settings(disableDocs)
  .settings(fatalWarningsSettings)
  .settings(publish / skip := true)
  .settings(
    name := "scala-tastytest",
    description := "Scala TASTy Integration Testing Tool",
    libraryDependencies += diffUtilsDep,
  )

// An instrumented version of BoxesRunTime and ScalaRunTime for partest's "specialized" test category
lazy val specLib = project.in(file("test") / "instrumented")
  .dependsOn(library, reflect, compiler)
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(fatalWarningsSettings)
  .settings(
    publish / skip := true,
    bspEnabled := false,
    Compile / sourceGenerators += Def.task {
      import scala.collection.JavaConverters._
      val srcBase = (library / Compile / sourceDirectories).value.head / "scala/runtime"
      val targetBase = (Compile / sourceManaged).value / "scala/runtime"
      def patch(srcFile: String, patchFile: String): File = try {
        val p = difflib.DiffUtils.parseUnifiedDiff(IO.readLines(baseDirectory.value / patchFile).asJava)
        val r = difflib.DiffUtils.patch(IO.readLines(srcBase / srcFile).asJava, p)
        val target = targetBase / srcFile
        IO.writeLines(target, r.asScala)
        target
      } catch { case ex: Exception =>
        streams.value.log.error(s"Error patching $srcFile: $ex")
        throw ex
      }
      IO.createDirectory(targetBase)
      Seq(
        patch("BoxesRunTime.java", "boxes.patch"),
        patch("ScalaRunTime.scala", "srt.patch")
      )
    }.taskValue,
  )

// The scala version used by the benchmark suites, leave undefined to use the ambient version.")
def benchmarkScalaVersion = System.getProperty("benchmark.scala.version", "")

lazy val bench = project.in(file("test") / "benchmarks")
  .dependsOn((if (benchmarkScalaVersion == "") Seq[sbt.ClasspathDep[sbt.ProjectReference]](library, compiler) else Nil): _*)
  .settings(if (benchmarkScalaVersion == "") instanceSettings else Seq(scalaVersion := benchmarkScalaVersion, crossPaths := false))
  .settings(disableDocs)
  .settings(publish / skip := true)
  .enablePlugins(JmhPlugin)
  .settings(
    name := "test-benchmarks",
    autoScalaLibrary := false,
    crossPaths := true, // needed to enable per-scala-version source directories (https://github.com/sbt/sbt/pull/1799)
    compileOrder := CompileOrder.JavaThenScala, // to allow inlining from Java ("... is defined in a Java source (mixed compilation), no bytecode is available")
    libraryDependencies += "org.openjdk.jol" % "jol-core" % "0.10",
    libraryDependencies ++= {
      if (benchmarkScalaVersion == "") Nil
      else "org.scala-lang" % "scala-compiler" % benchmarkScalaVersion :: Nil
    },
    //scalacOptions ++= Seq("-feature", "-opt:inline:scala/**", "-Wopt"),
    scalacOptions ++= Seq("-feature", "-opt:l:inline", "-opt-inline-from:scala/**", "-opt-warnings"),
    // Skips JMH source generators during IDE import to avoid needing to compile scala-library during the import
    // should not be needed once sbt-jmh 0.4.3 is out (https://github.com/sbt/sbt-jmh/pull/207)
    Jmh / bspEnabled := false
  ).settings(inConfig(JmhPlugin.JmhKeys.Jmh)(scalabuild.JitWatchFilePlugin.jitwatchSettings))


lazy val testkit = configureAsSubproject(project)
  .dependsOn(compiler)
  .settings(Osgi.settings)
  .settings(AutomaticModuleName.settings("scala.testkit"))
  .settings(fatalWarningsSettings)
  .settings(
    name := "scala-testkit",
    description := "Scala Compiler Testkit",
    libraryDependencies ++= Seq(junitDep, asmDep),
    Compile / unmanagedSourceDirectories := List(baseDirectory.value),
    fixPom(
      "/project/name" -> <name>Scala Testkit</name>,
      "/project/description" -> <description>Scala Compiler Testing Tool</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    )
  )

// Jigsaw: reflective access between modules (`setAccessible(true)`) requires an `opens` directive.
// This is enforced by error (not just by warning) since JDK 16. In our tests we use reflective access
// from the unnamed package (the classpath) to JDK modules in testing utilities like `assertNotReachable`.
// `add-exports=jdk.jdeps/com.sun.tools.javap` is tests that use `:javap` in the REPL, see scala/bug#12378
val addOpensForTesting = "-XX:+IgnoreUnrecognizedVMOptions" +: "--add-exports=jdk.jdeps/com.sun.tools.javap=ALL-UNNAMED" +:
  Seq("java.util.concurrent.atomic", "java.lang", "java.lang.reflect", "java.net").map(p => s"--add-opens=java.base/$p=ALL-UNNAMED")

lazy val junit = project.in(file("test") / "junit")
  .dependsOn(testkit, compiler, replFrontend, scaladoc, sbtBridge)
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(fatalWarningsSettings)
  .settings(publish / skip := true)
  .settings(
    Test / fork := true,
    Test / javaOptions ++= "-Xss1M" +: addOpensForTesting,
    (Test / forkOptions) := (Test / forkOptions).value.withWorkingDirectory((ThisBuild / baseDirectory).value),
    (Test / testOnly / forkOptions) := (Test / testOnly / forkOptions).value.withWorkingDirectory((ThisBuild / baseDirectory).value),
    Compile / scalacOptions ++= Seq(
      "-Xlint:-valpattern",
      "-Wconf:msg=match may not be exhaustive:s", // if we missed a case, all that happens is the test fails
      "-Wconf:cat=lint-nullary-unit&site=.*Test:s", // normal unit test style
      "-Ypatmat-exhaust-depth", "40", // despite not caring about patmat exhaustiveness, we still get warnings for this
    ),
    Compile / javacOptions ++= Seq("-Xlint"),
    libraryDependencies ++= Seq(junitInterfaceDep, jolDep, diffUtilsDep, compilerInterfaceDep),
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s"),
    Compile / unmanagedSourceDirectories := Nil,
    Test / unmanagedSourceDirectories := List(baseDirectory.value),
    Test / headerSources := Nil,
  )

lazy val tasty = project.in(file("test") / "tasty")
  .settings(commonSettings)
  .dependsOn(tastytest)
  .settings(disableDocs)
  .settings(publish / skip := true)
  .settings(
    Test / fork := true,
    libraryDependencies ++= Seq(junitInterfaceDep, TastySupport.scala3Library),
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v"),
    Test / testOptions += Tests.Argument(
      s"-Dtastytest.src=${baseDirectory.value}",
      s"-Dtastytest.packageName=tastytest"
    ),
    Compile / unmanagedSourceDirectories := Nil,
    Test    / unmanagedSourceDirectories := List(baseDirectory.value/"test"),
  )
  .configs(TastySupport.CompilerClasspath, TastySupport.LibraryClasspath)
  .settings(
    inConfig(TastySupport.CompilerClasspath)(Defaults.configSettings),
    inConfig(TastySupport.LibraryClasspath)(Defaults.configSettings),
    libraryDependencies ++= Seq(
      TastySupport.scala3Compiler % TastySupport.CompilerClasspath,
      TastySupport.scala3Library % TastySupport.LibraryClasspath,
    ),
    javaOptions ++= {
      import java.io.File.pathSeparator
      val scalaLibrary = (library / Compile / classDirectory).value.getAbsoluteFile()
      val scalaReflect = (reflect / Compile / classDirectory).value.getAbsoluteFile()
      val dottyCompiler = (TastySupport.CompilerClasspath / managedClasspath).value.seq.map(_.data) :+ scalaLibrary
      val dottyLibrary = (TastySupport.LibraryClasspath / managedClasspath).value.seq.map(_.data) :+ scalaLibrary
      Seq(
        s"-Dtastytest.classpaths.dottyCompiler=${dottyCompiler.mkString(pathSeparator)}",
        s"-Dtastytest.classpaths.dottyLibrary=${dottyLibrary.mkString(pathSeparator)}",
        s"-Dtastytest.classpaths.scalaReflect=$scalaReflect",
      )
    },
    Compile / scalacOptions ++= Seq(
      "-Wconf:cat=lint-nullary-unit&site=.*Test:s", // normal unit test style
    ),
  )

lazy val scalacheck = project.in(file("test") / "scalacheck")
  .dependsOn(library, reflect, compiler, scaladoc)
  .settings(commonSettings)
  .settings(fatalWarningsSettings)
  .settings(disableDocs)
  .settings(publish / skip := true)
  .settings(
    // Enable forking to workaround https://github.com/sbt/sbt/issues/4009.
    Test / fork := true,
    // Instead of forking above, it should be possible to set:
    // Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat,
    Test / javaOptions ++= "-Xss1M" +: addOpensForTesting,
    Test / testOptions += Tests.Argument(
      // Full stack trace on failure:
      "-verbosity", "2"
    ),
    libraryDependencies ++= Seq(scalacheckDep, junitDep),
    Compile / unmanagedSourceDirectories := Nil,
    Test / unmanagedSourceDirectories := List(baseDirectory.value),
    Compile / scalacOptions ++= Seq(
      "-Wconf:msg=match may not be exhaustive:s", // if we missed a case, all that happens is the test fails
      "-Wconf:msg=Classes which cannot access Tree:s", // extension is irrelevant to tests
    ),
  )

lazy val osgiTestFelix = osgiTestProject(
  project.in(file(".") / "target" / "osgiTestFelix"),
  "org.apache.felix" % "org.apache.felix.framework" % "5.6.10")

lazy val osgiTestEclipse = osgiTestProject(
  project.in(file(".") / "target" / "osgiTestEclipse"),
  "org.eclipse.tycho" % "org.eclipse.osgi" % "3.13.0.v20180226-1711")

def osgiTestProject(p: Project, framework: ModuleID) = p
  .dependsOn(library, reflect, compiler)
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(
    publish / skip := true,
    bspEnabled := false,
    Test / fork := true,
    Test / parallelExecution := false,
    libraryDependencies ++= {
      val paxExamVersion = "4.11.0" // Last version which supports Java 9+
      Seq(
        junitDep,
        junitInterfaceDep,
        "org.ops4j.pax.exam" % "pax-exam-container-native" % paxExamVersion,
        "org.ops4j.pax.exam" % "pax-exam-junit4" % paxExamVersion,
        "org.ops4j.pax.exam" % "pax-exam-link-assembly" % paxExamVersion,
        "org.ops4j.pax.url" % "pax-url-aether" % "2.4.1",
        "org.ops4j.pax.swissbox" % "pax-swissbox-tracker" % "1.8.1",
        "ch.qos.logback" % "logback-core" % "1.2.8",
        "ch.qos.logback" % "logback-classic" % "1.2.8",
        "org.slf4j" % "slf4j-api" % "1.7.32",
        framework % Test
      )
    },
    Test / Keys.test := (Test / Keys.test).dependsOn(Compile / packageBin).value,
    Test / Keys.testOnly := (Test / Keys.testOnly).dependsOn(Compile / packageBin).evaluated,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-q"),
    Test / javaOptions ++= ("-Dscala.bundle.dir=" + (ThisBuild / buildDirectory).value / "osgi") +: addOpensForTesting,
    Test / Keys.test / forkOptions := (Test / Keys.test / forkOptions).value.withWorkingDirectory((ThisBuild / baseDirectory).value),
    Test / unmanagedSourceDirectories := List((ThisBuild / baseDirectory).value / "test" / "osgi" / "src"),
    Compile / unmanagedResourceDirectories := (Test / unmanagedSourceDirectories).value,
    Compile / unmanagedResources / includeFilter := "*.xml",
    Compile / packageBin := { // Put the bundle JARs required for the tests into build/osgi
      val targetDir = (ThisBuild / buildDirectory).value / "osgi"
      val mappings = ((dist / mkPack).value / "lib").listFiles.collect {
        case f if f.getName.startsWith("scala-") && f.getName.endsWith(".jar") => (f, targetDir / f.getName)
      }
      IO.copy(mappings, CopyOptions() withOverwrite true)
      targetDir
    },
    cleanFiles += (ThisBuild / buildDirectory).value / "osgi"
  )

lazy val verifyScriptedBoilerplate = taskKey[Unit]("Ensure scripted tests have the necessary boilerplate.")

// Running scripted tests locally
//  - `set ThisBuild / Compile / packageDoc / publishArtifact := false` for faster turn around time
//  - `sbtTest/scripted source-dependencies/scalac-options` to run a single test
//  - `set sbtTest/scriptedBufferLog := false` to see sbt log of test
//  - add `> set logLevel := Level.Debug` to individual `test` script for debug output
//  - uncomment `-agentlib:...` below to attach the debugger while running a test
lazy val sbtTest = project.in(file("test") / "sbt-test")
  .enablePlugins(ScriptedPlugin)
  .settings(disableDocs)
  .settings(
    scalaVersion := appConfiguration.value.provider.scalaProvider.version,
    publish / skip := true,
    bspEnabled := false,
    target := (ThisBuild / target).value / thisProject.value.id,

    sbtTestDirectory := baseDirectory.value,

    scriptedBatchExecution := true, // set to `false` to execute each test in a separate sbt instance
    scriptedParallelInstances := 2, // default is 1

    // hide sbt output of scripted tests
    scriptedBufferLog := true,

    scriptedLaunchOpts ++= Seq(
      "-Dplugin.scalaVersion=" + version.value,
      "-Dsbt.boot.directory=" + (target.value / ".sbt-scripted").getAbsolutePath, // Workaround sbt/sbt#3469
      "-Dscripted.common=" + (baseDirectory.value / "common.sbt.template").getAbsolutePath,
      // "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=*:5005",
    ),

    // Pass along ivy home and repositories settings to sbt instances run from the tests
    scriptedLaunchOpts ++= {
      val repositoryPath = (io.Path.userHome / ".sbt" / "repositories").absolutePath
      s"-Dsbt.repository.config=$repositoryPath" ::
        ivyPaths.value.ivyHome.map("-Dsbt.ivy.home=" + _.getAbsolutePath).toList
    },

    verifyScriptedBoilerplate := {
      import java.nio.file._
      val tests = (baseDirectory.value * "*").get.flatMap(f => (f * "*").get()).filter(_.isDirectory)
      for (t <- tests) {
        for (script <- (t * ("test" || "pending" || "disabled")).get().headOption) {
          val ls = Files.lines(script.toPath)
          val setup = ls.findFirst().orElseGet(() => "")
          ls.close()
          if (setup.trim != "> setup; reload")
            throw new MessageOnlyException(s"$script is missing test boilerplate; the first needs to be `> setup; reload`")
        }
        val pluginFile = "project/ScriptedTestPlugin.scala"
        if (!(t / pluginFile).exists)
          throw new MessageOnlyException(s"$t is missing the file $pluginFile; copy it from any other scripted test")
      }
    },

    scripted := scripted.dependsOn(
      verifyScriptedBoilerplate,
      library / publishLocal,
      reflect / publishLocal,
      compiler / publishLocal,
      sbtBridge / publishLocal,
    ).evaluated
  )

lazy val partestJavaAgent = configureAsSubproject(project, srcdir = Some("partest-javaagent"))
  .settings(fatalWarningsSettings)
  .settings(disableDocs)
  .settings(
    libraryDependencies += asmDep,
    publish / skip := true,
    // Setting name to "scala-partest-javaagent" so that the jar file gets that name, which the Runner relies on
    name := "scala-partest-javaagent",
    description := "Scala Compiler Testing Tool (compiler-specific java agent)",
    // add required manifest entry - previously included from file
    Compile / packageBin / packageOptions +=
      Package.ManifestAttributes( "Premain-Class" -> "scala.tools.partest.javaagent.ProfilingAgent" ),
    // we need to build this to a JAR
    exportJars := true
  )

lazy val test = project
  .dependsOn(compiler, interactive, replFrontend, scalap, partest, partestJavaAgent, scaladoc)
  .disablePlugins(plugins.JUnitXmlReportPlugin)
  .configs(IntegrationTest)
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(publish / skip := true)
  .settings(Defaults.itSettings)
  .settings(
    libraryDependencies ++= Seq(asmDep),
    // no main sources
    Compile / unmanagedSourceDirectories := Nil,
    Compile / sources := Nil,
    // test sources are compiled in partest run, not here
    IntegrationTest / unmanagedSourceDirectories := Nil,
    IntegrationTest / sources := Nil,
    IntegrationTest / fork := true,
    Compile / scalacOptions += "-Yvalidate-pos:parser,typer",
    IntegrationTest / javaOptions ++= List("-Xmx2G", "-Dpartest.exec.in.process=true", "-Dfile.encoding=UTF-8", "-Duser.language=en", "-Duser.country=US") ++ addOpensForTesting,
    IntegrationTest / javaOptions ++= { if (scala.util.Properties.isJavaAtLeast("18")) List("-Djava.security.manager=allow") else Nil },
    IntegrationTest / testOptions += Tests.Argument("-Dfile.encoding=UTF-8", "-Duser.language=en", "-Duser.country=US"),
    testFrameworks += new TestFramework("scala.tools.partest.sbt.Framework"),
    IntegrationTest / testOptions += Tests.Argument(s"-Dpartest.java_opts=-Xmx1024M -Xms64M ${addOpensForTesting.mkString(" ")}"),
    IntegrationTest / testOptions += Tests.Argument("-Dpartest.scalac_opts=" + (Compile / scalacOptions).value.mkString(" ")),
    (IntegrationTest / forkOptions) := (IntegrationTest / forkOptions).value.withWorkingDirectory((ThisBuild / baseDirectory).value),
    IntegrationTest / testOptions += {
      val cp = (Test / dependencyClasspath).value
      val baseDir = (ThisBuild / baseDirectory).value
      val instrumentedJar = (specLib / Compile / packageBin / packagedArtifact).value._2
      Tests.Setup { () =>
        // Copy instrumented.jar (from specLib)to the location where partest expects it.
        IO.copyFile(instrumentedJar, baseDir / "test/files/speclib/instrumented.jar")
      }
    },
    IntegrationTest / definedTests += new sbt.TestDefinition(
      "partest",
      // marker fingerprint since there are no test classes
      // to be discovered by sbt:
      new sbt.testing.AnnotatedFingerprint {
        def isModule = true
        def annotationName = "partest"
      }, true, Array()
    ),
    IntegrationTest / executeTests := {
      val log = streams.value.log
      val result = (IntegrationTest / executeTests).value
      val result2 = (Test / executeTests).value
      if (result.overall != TestResult.Error && result.events.isEmpty) {
        // workaround for https://github.com/sbt/sbt/issues/2722
        log.error("No test events found")
        result2.copy(overall = TestResult.Error)
      }
      else result
    },
    IntegrationTest / testListeners += new PartestTestListener(target.value)
  )

lazy val manual = configureAsSubproject(project)
  .settings(disableDocs)
  .settings(publish / skip := true)
  .settings(fatalWarningsSettings)
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-library" % scalaVersion.value,
    Compile / classDirectory := (Compile / target).value / "classes"
  )

lazy val scalaDist = Project("scalaDist", file(".") / "target" / "scala-dist-dist-src-dummy")
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(
    bspEnabled := false,
    name := "scala-dist",
    Compile / packageBin / mappings ++= {
      val binBaseDir = buildDirectory.value / "pack"
      val binMappings = (dist / mkBin).value.pair(Path.relativeTo(binBaseDir), errorIfNone = false)
      // With the way the resource files are spread out over the project sources we can't just add
      // an unmanagedResourceDirectory, so we generate the mappings manually:
      val docBaseDir = (ThisBuild / baseDirectory).value
      val docMappings = (docBaseDir / "doc").allPaths pair Path.relativeTo(docBaseDir)
      val resBaseDir = (ThisBuild / baseDirectory).value / "src/manual/scala/tools/docutil/resources"
      val resMappings = resBaseDir ** ("*.html" | "*.css" | "*.gif" | "*.png") pair (p => Path.relativeTo(resBaseDir)(p).map("doc/tools/" + _))
      docMappings ++ resMappings ++ binMappings
    },
    Compile / resourceGenerators += Def.task {
      val command = "fsc, scala, scalac, scaladoc, scalap"
      val htmlOut = (Compile / resourceManaged).value / "doc/tools"
      val manOut = (Compile / resourceManaged).value / "genman"
      val fixedManOut = (Compile / resourceManaged).value / "man"
      IO.createDirectory(htmlOut)
      IO.createDirectory(manOut / "man1")
      runner.value.run("scala.tools.docutil.ManMaker",
        (manual / Compile / fullClasspath).value.files,
        Seq(command, htmlOut.getAbsolutePath, manOut.getAbsolutePath),
        streams.value.log).failed foreach (sys error _.getMessage)
      (manOut ** "*.1" pair Path.rebase(manOut, fixedManOut)).foreach { case (in, out) =>
        // Generated manpages should always use LF only. There doesn't seem to be a good reason
        // for generating them with the platform EOL first and then converting them but that's
        // what the old Ant build did.
        IO.write(out, IO.readBytes(in).filterNot(_ == '\r'))
      }
      (htmlOut ** "*.html").get ++ (fixedManOut ** "*.1").get
    }.taskValue,
    Compile / managedResourceDirectories := Seq((Compile / resourceManaged).value),
    libraryDependencies ++= jlineDeps,
    apiURL := None,
    fixPom(
      "/project/name" -> <name>Scala Distribution Artifacts</name>,
      "/project/description" -> <description>The Artifacts Distributed with Scala</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    ),
    Compile / packageSrc / publishArtifact := false
  )
  .dependsOn(library, reflect, compiler, scalap)

def partestOnly(in: String): Def.Initialize[Task[Unit]] =
  (testP / IntegrationTest / testOnly).toTask(" -- --terse " + in)

def partestDesc(in: String): Def.Initialize[Task[(Result[Unit], String)]] =
  partestOnly(in).result map (_ -> s"partest $in")

lazy val root: Project = (project in file("."))
  .settings(disableDocs)
  .settings(generateBuildCharacterFileSettings)
  .settings(
    publish / skip := true,
    commands ++= ScriptCommands.all,
    extractBuildCharacterPropertiesFile := {
      val jar = (bootstrap / scalaInstance).value.allJars.find(_.getName contains "-compiler").get
      val bc = buildCharacterPropertiesFile.value
      val packagedName = "scala-buildcharacter.properties"
      IO.withTemporaryDirectory { tmp =>
        val extracted = IO.unzip(jar, tmp, new SimpleFilter(_ == packagedName)).headOption.getOrElse {
          throw new RuntimeException(s"No file $packagedName found in bootstrap compiler $jar")
        }
        IO.copyFile(extracted, bc)
        bc
      }
    },
    // Generate (Product|TupleN|Function|AbstractFunction)*.scala files and scaladoc stubs for all AnyVal sources.
    // They should really go into a managedSources dir instead of overwriting sources checked into git but scaladoc
    // source links (could be fixed by shipping these sources with the scaladoc bundles) and scala-js source maps
    // rely on them being on github.
    commands += Command.command("generateSources") { state =>
      val dir = ((ThisBuild / baseDirectory).value / "src" / "library" / "scala").getAbsoluteFile
      genprod.run(dir)
      GenerateAnyVals.run(dir)
      GenerateFunctionConverters.run(dir)
      state
    },
    // ../docs.scala-lang/_data/compiler-options.yml
    commands += Command.command("generateDocsData") { state =>
      val dir = (((ThisBuild / baseDirectory).value) / ".." / "docs.scala-lang" / "_data")
      val target = if (dir.exists) dir else ((ThisBuild / baseDirectory).value)
      GenerateDocsData.run(target.getAbsoluteFile)
      state
    },

    testJDeps := TestJDeps.testJDepsImpl.value,
    testJarSize := TestJarSize.testJarSizeImpl.value,

    // Wasn't sure if findRootCauses would work if I just aggregated testAll1/etc, so a little duplication..
    testAll  := runTests(unitTests ::: partests ::: remainingTests).value,
    // splitting this in two parts allows them to run in parallel on CI.
    // partest takes the longest, so "partest vs. everything else" is a roughly equal split
    testAll1 := runTests(unitTests ::: remainingTests).value,
    testAll2 := runTests(partests).value,

    setIncOptions
  )
  .aggregate(library, reflect, compiler, interactive, repl, replFrontend, sbtBridge,
    scaladoc, scalap, testkit, partest, junit, scalacheck, tasty, tastytest, scalaDist).settings(
    Compile / sources := Seq.empty,
    onLoadMessage := s"""|*** Welcome to the sbt build definition for Scala! ***
      |version=${(Global / version).value} scalaVersion=${(Global / scalaVersion).value}
      |Check README.md for more information.""".stripMargin
  )

lazy val clearSavedLogs = SavedLogs.clearSavedLogs.result.map(_ -> "clearSavedLogs")

lazy val unitTests = List(
  (     junit / Test / testOnly).toTask(" -- +v").result.map(_ -> "junit/testOnly -- +v"),
  (scalacheck / Test / Keys.test                ).result.map(_ -> "scalacheck/test"),
)

lazy val partests = List(
  partestDesc("run"),
  partestDesc("pos neg jvm"),
  partestDesc("res scalap specialized"),
  partestDesc("instrumented presentation"),
  partestDesc("--srcpath scaladoc"),
  partestDesc("--srcpath macro-annot"),
  partestDesc("--srcpath async"),
  (tasty / Test / Keys.test).result.map(_ -> "tasty/test"),
)

lazy val remainingTests = List(
  (osgiTestFelix   / Test / Keys.test).result.map(_ -> "osgiTestFelix/test"),
  (osgiTestEclipse / Test / Keys.test).result.map(_ -> "osgiTestEclipse/test"),
  (sbtTest / scripted         ).toTask("").result.map(_ -> "sbtTest/scripted"),
  (library / mimaReportBinaryIssues      ).result.map(_ -> "library/mimaReportBinaryIssues"), // doesn't aggregate..
  (reflect / mimaReportBinaryIssues      ).result.map(_ -> "reflect/mimaReportBinaryIssues"), // ..so specify both
  (testJDeps                             ).result.map(_ -> "testJDeps"),
  (testJarSize                           ).result.map(_ -> "testJarSize"),
  (bench / Compile / compile).map(_ => ()).result.map(_ -> "bench/compile"),
  Def.task(()).dependsOn( // Run these in parallel:
     library / Compile / doc,
     reflect / Compile / doc,
    compiler / Compile / doc,
      scalap / Compile / doc,
  ).result.map(_ -> "doc")
)

def runTests(tests: List[Def.Initialize[Task[(Result[Unit], String)]]]) = Def.task {
  val results = ScriptCommands.sequence[(Result[Unit], String)](clearSavedLogs :: tests).value
  val log     = streams.value.log
  val failed  = results.collect { case (Inc(i), d) => (i, d) }
  if (failed.nonEmpty) {
    def showScopedKey(k: Def.ScopedKey[_]): String =
      Vector(
        k.scope.project.toOption.map { case p: ProjectRef => p.project case p => p }.map(_ + "/"),
        k.scope.config.toOption.map(_.name + ":"),
        k.scope.task.toOption.map(_.label + "::")
      ).flatten.mkString + k.key

    val loggedThis, loggedAny = new scala.collection.mutable.HashSet[String]

    def findRootCauses(i: Incomplete, currentTask: String): Vector[(String, Option[Throwable])] = {
      val skey = i.node.collect { case t: Task[_] => t.info.attributes.get(taskDefinitionKey) }.flatten
      val sk = skey.map(showScopedKey)
      val task = sk.getOrElse(currentTask)
      val dup = sk.exists(!loggedAny.add(_))
      if (sk.exists(!loggedThis.add(_))) Vector.empty
      else i.directCause match {
        case Some(e) => Vector((task, if (dup) None else Some(e)))
        case None    => i.causes.toVector.flatMap(findRootCauses(_, task))
      }
    }

    log.error("")
    log.error(s"${failed.size} of ${results.length} test tasks failed:")
    failed.foreach { case (i, d) =>
      log.error(s"- $d")
      loggedThis.clear()
      findRootCauses(i, "<unknown task>").foreach {
        case (task, Some(ex)) => log.error(s"  - $task failed: $ex")
        case (task, None)     => log.error(s"  - ($task failed)")
      }
    }
    SavedLogs.showSavedLogsImpl(log.error(_))
    throw new MessageOnlyException("Failure due to previous errors")
  }
}

def setIncOptions = incOptions := {
  incOptions.value
    .withRecompileOnMacroDef(Some(Boolean box false).asJava) // macros in library+reflect are hard-wired to implementations with `FastTrack`.
}

// The following subprojects' binaries are required for building "pack":
lazy val distDependencies = Seq(replFrontend, compiler, library, reflect, scalap, scaladoc)

lazy val dist = (project in file("dist"))
  .settings(commonSettings)
  .settings(
    bspEnabled := false,
    libraryDependencies ++= jlineDeps,
    mkBin := mkBinImpl.value,
    mkQuick := Def.task {
      val cp = (testP / IntegrationTest / fullClasspath).value
      val propsFile = (ThisBuild / buildDirectory).value / "quick" / "partest.properties"
      val props = new java.util.Properties()
      props.setProperty("partest.classpath", cp.map(_.data.getAbsolutePath).mkString(sys.props("path.separator")))
      IO.write(props, null, propsFile)
      (ThisBuild / buildDirectory).value / "quick"
    }.dependsOn((distDependencies.map(_ / Runtime / products) :+ mkBin): _*).value,
    mkPack := Def.task { (ThisBuild / buildDirectory).value / "pack" }.dependsOn(Compile / packageBin / packagedArtifact, mkBin).value,
    target := (ThisBuild / target).value / projectFolder.value,
    Compile / packageBin := {
      val targetDir = (ThisBuild / buildDirectory).value / "pack" / "lib"
      val jlineJAR = findJar((Compile / dependencyClasspath).value, jlineDep).get.data
      val jnaJAR = findJar((Compile / dependencyClasspath).value, jnaDep).get.data
      val mappings = Seq(
        (jlineJAR, targetDir / "jline.jar"),
        (jnaJAR, targetDir / "jna.jar"),
      )
      IO.copy(mappings, CopyOptions() withOverwrite true)
      targetDir
    },
    cleanFiles += (ThisBuild / buildDirectory).value / "quick",
    cleanFiles += (ThisBuild / buildDirectory).value / "pack",
    Compile / packageBin / packagedArtifact :=
      (Compile / packageBin / packagedArtifact)
        .dependsOn(distDependencies.map(_ / Compile / packageBin / packagedArtifact): _*)
        .value
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
def configureAsSubproject(project: Project, srcdir: Option[String] = None): Project = {
  val base = file(".") / "src" / srcdir.getOrElse(project.id)
  (project in base)
    .settings(scalaSubprojectSettings)
    .settings(generatePropertiesFileSettings)
    .settings(projectFolder := srcdir.getOrElse(project.id))
}

lazy val mkBin = taskKey[Seq[File]]("Generate shell script (bash or Windows batch).")
lazy val mkQuick = taskKey[File]("Generate a full build, including scripts, in build/quick")
lazy val mkPack = taskKey[File]("Generate a full build, including scripts, in build/pack")
lazy val testAll = taskKey[Unit]("Run all test tasks sequentially")
lazy val testAll1 = taskKey[Unit]("Run 1/2 test tasks sequentially")
lazy val testAll2 = taskKey[Unit]("Run 2/2 test tasks sequentially")

val testJDeps = taskKey[Unit]("Run jdeps to check dependencies")
val testJarSize = taskKey[Unit]("Test that jars have the expected size")

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
  val rootDir = (compiler / Compile / classDirectory).value
  val quickOutDir = buildDirectory.value / "quick/bin"
  val packOutDir = buildDirectory.value / "pack/bin"
  def writeScripts(scalaTool: ScalaTool, file: String, outDir: File): Seq[File] = {
    val res = Seq(
      scalaTool.writeScript(file, "unix", rootDir, outDir),
      scalaTool.writeScript(file, "windows", rootDir, outDir)
    )
    res.foreach { f =>
      if(!f.getAbsoluteFile.setExecutable(true, /* ownerOnly: */ false))
        throw new IOException("setExecutable failed")
      if(!f.getAbsoluteFile.setReadable(true, /* ownerOnly: */ false))
        throw new IOException("setReadable failed")
    }
    res
  }

  def mkBin(file: String, mainCls: String, classpath: Seq[Attributed[File]]): Seq[File] =
    writeScripts(mkScalaTool(mainCls, classpath), file, quickOutDir) ++
    writeScripts(mkScalaTool(mainCls, Nil      ), file, packOutDir)

  streams.value.log.info(s"Creating scripts in $quickOutDir and $packOutDir")

  mkBin("scala"    , "scala.tools.nsc.MainGenericRunner", (replFrontend / Compile / fullClasspath).value) ++
  mkBin("scalac"   , "scala.tools.nsc.Main",              (compiler / Compile / fullClasspath).value) ++
  mkBin("fsc"      , "scala.tools.nsc.fsc.CompileClient", (compiler / Compile / fullClasspath).value) ++
  mkBin("scaladoc" , "scala.tools.nsc.ScalaDoc",          (scaladoc / Compile / fullClasspath).value) ++
  mkBin("scalap"   , "scala.tools.scalap.Main",           (scalap / Compile / fullClasspath).value)
}

/** Generate service provider definition files under META-INF/services */
def generateServiceProviderResources(services: (String, String)*): Setting[_] =
  Compile / resourceGenerators += Def.task {
    services.map { case (k, v) =>
      val f = (Compile / resourceManaged).value / "META-INF/services" / k
      IO.write(f, v + "\n")
      f
    }
  }.taskValue

// Add tab completion to partest
commands += Command("partest")(_ => PartestUtil.partestParser((ThisBuild / baseDirectory).value, (ThisBuild / baseDirectory).value / "test")) { (state, parsed) =>
  ("test/IntegrationTest/testOnly -- " + parsed) :: state
}

// Watch the test files also so ~partest triggers on test case changes
watchSources ++= PartestUtil.testFilePaths((ThisBuild / baseDirectory).value, (ThisBuild / baseDirectory).value / "test")

// Add tab completion to scalac et al.
commands ++= {
  val commands =
  List(("scalac",   "compiler", "scala.tools.nsc.Main"),
       ("scala",    "replFrontend", "scala.tools.nsc.MainGenericRunner"),
       ("scaladoc", "scaladoc", "scala.tools.nsc.ScalaDoc"))

  commands.map {
    case (entryPoint, projectRef, mainClassName) =>
      Command(entryPoint)(_ => ScalaOptionParser.scalaParser(entryPoint, (ThisBuild / baseDirectory).value)) { (state, parsedOptions) =>
        (projectRef + "/runMain " + mainClassName + " -usejavacp " + parsedOptions) :: state
      }
  }
}

addCommandAlias("scalap",   "scalap/compile:runMain              scala.tools.scalap.Main -usejavacp")

lazy val intellij = taskKey[Unit]("Update the library classpaths in the IntelliJ project files.")

def moduleDeps(p: Project, config: Configuration = Compile) = (p / config / externalDependencyClasspath).map(a => (p.id, a.map(_.data)))

// aliases to projects to prevent name clashes
def compilerP = compiler
def testP = test

intellij := {
  import xml._
  import xml.transform._

  val s = streams.value
  val compilerScalaInstance = (LocalProject("compiler") / scalaInstance).value

  val modules: List[(String, Seq[File])] = {
    // for the sbt build module, the dependencies are fetched from the project's build using sbt-buildinfo
    val buildModule = ("scala-build", scalabuild.BuildInfo.buildClasspath.split(java.io.File.pathSeparator).toSeq.map(new File(_)))
    // `sbt projects` lists all modules in the build
    buildModule :: List(
      moduleDeps(bench).value,
      moduleDeps(compilerP).value,
      moduleDeps(interactive).value,
      moduleDeps(junit).value,
      moduleDeps(library).value,
      moduleDeps(manual).value,
      moduleDeps(partest).value,
      moduleDeps(partestJavaAgent).value,
      moduleDeps(reflect).value,
      moduleDeps(repl).value,
      moduleDeps(replFrontend).value,
      moduleDeps(scalacheck, config = Test).value.copy(_1 = "scalacheck-test"),
      moduleDeps(scaladoc).value,
      moduleDeps(scalap).value,
      moduleDeps(tastytest).value,
      moduleDeps(testP).value,
      moduleDeps(testkit).value,
    )
  }

  def moduleDep(name: String, jars: Seq[File]) = {
    val entries = jars.map(f => s"""        <root url="jar://${f.toURI.getPath}!/" />""").mkString("\n")
    s"""|    <library name="$name-deps">
        |      <CLASSES>
        |$entries
        |      </CLASSES>
        |      <JAVADOC />
        |      <SOURCES />
        |    </library>""".stripMargin
  }

  def starrDep(jars: Seq[File]) = {
    val entries = jars.map(f => s"""          <root url="file://${f.toURI.getPath}" />""").mkString("\n")
    s"""|    <library name="starr" type="Scala">
        |      <properties>
        |        <option name="languageLevel" value="Scala_2_12" />
        |        <compiler-classpath>
        |$entries
        |        </compiler-classpath>
        |      </properties>
        |      <CLASSES />
        |      <JAVADOC />
        |      <SOURCES />
        |    </library>""".stripMargin
  }

  def replaceLibrary(data: Node, libName: String, libType: Option[String], newContent: String) = {
    object rule extends RewriteRule {
      var transformed = false
      def checkAttrs(attrs: MetaData) = {
        def check(key: String, expected: String) = {
          val a = attrs(key)
          a != null && a.text == expected
        }
        check("name", libName) && libType.forall(tp => check("type", tp))
      }

      override def transform(n: Node): Seq[Node] = n match {
        case e @ Elem(_, "library", attrs, _, _*) if checkAttrs(attrs) =>
          transformed = true
          XML.loadString(newContent)
        case other =>
          other
      }
    }
    object trans extends RuleTransformer(rule)
    val r = trans(data)
    if (!rule.transformed) sys.error(s"Replacing library classpath for $libName failed, no existing library found.")
    r
  }

  val intellijDir = (ThisBuild / baseDirectory).value / "src/intellij"
  val ipr = intellijDir / "scala.ipr"
  backupIdea(intellijDir)
  if (!ipr.exists) {
    intellijCreateFromSample((ThisBuild / baseDirectory).value)
  }
  s.log.info("Updating library classpaths in src/intellij/scala.ipr.")
  val content = XML.loadFile(ipr)

  val newStarr = replaceLibrary(content, "starr", Some("Scala"), starrDep(compilerScalaInstance.allJars))
  val newModules = modules.foldLeft(newStarr)({
    case (res, (modName, jars)) =>
      if (jars.isEmpty) res // modules without dependencies
      else replaceLibrary(res, s"$modName-deps", None, moduleDep(modName, jars))
  })

  // I can't figure out how to keep the entity escapes for \n in the attribute values after this use of XML transform.
  // Patching the original version back in with more brutish parsing.
  val R = """(?ims)(.*)(<copyright>.*</copyright>)(.*)""".r
  val oldContents = IO.read(ipr)
  XML.save(ipr.getAbsolutePath, newModules)
  oldContents match {
    case R(_, withEscapes, _) =>
      val newContents = IO.read(ipr)
      val R(pre, toReplace, post) = newContents
      IO.write(ipr, pre + withEscapes + post)
    case _ =>
      // .ipr file hasn't been updated from `intellijFromSample` yet
  }
}

lazy val intellijFromSample = taskKey[Unit]("Create fresh IntelliJ project files from src/intellij/*.SAMPLE.")

def backupIdea(ideaDir: File): Unit = {
  val temp = IO.createTemporaryDirectory
  IO.copyDirectory(ideaDir, temp)
  println(s"Backed up existing src/intellij to $temp")
}

intellijFromSample := {
  val s = streams.value
  val intellijDir = (ThisBuild / baseDirectory).value / "src/intellij"
  val ipr = intellijDir / "scala.ipr"
  backupIdea(intellijDir)
  intellijCreateFromSample((ThisBuild / baseDirectory).value)
}

def intellijCreateFromSample(basedir: File): Unit = {
  val files = basedir / "src/intellij" * "*.SAMPLE"
  val copies = files.get.map(f => (f, new File(f.getAbsolutePath.stripSuffix(".SAMPLE"))))
  IO.copy(copies, CopyOptions() withOverwrite true)
}

lazy val intellijToSample = taskKey[Unit]("Update src/intellij/*.SAMPLE using the current IntelliJ project files.")

intellijToSample := {
  val s = streams.value
  val intellijDir = (ThisBuild / baseDirectory).value / "src/intellij"
  val ipr = intellijDir / "scala.ipr"
  backupIdea(intellijDir)
  val existing =intellijDir * "*.SAMPLE"
  IO.delete(existing.get)
  val current = intellijDir * ("*.iml" || "*.ipr")
  val copies = current.get.map(f => (f, new File(f.getAbsolutePath + ".SAMPLE")))
  IO.copy(copies)
}

/** Find a specific module's JAR in a classpath, comparing only organization and name */
def findJar(files: Seq[Attributed[File]], dep: ModuleID): Option[Attributed[File]] = {
  def extract(m: ModuleID) = (m.organization, m.name)
  files.find(_.get(moduleID.key).map(extract _) == Some(extract(dep)))
}

{
  scala.build.TravisOutput.installIfOnTravis()
  Nil
}
