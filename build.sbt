/*
 * The new, sbt-based build definition for Scala.
 *
 * What you see below is very much work-in-progress. The following features are implemented:
 *   - Compiling all classses for the compiler and library ("compile" in the respective subprojects)
 *   - Running JUnit ("junit/test"), ScalaCheck ("scalacheck/test"), and partest ("test/IntegrationTest/test") tests
 *   - Creating build/quick with all compiled classes and launcher scripts ("dist/mkQuick")
 *   - Creating build/pack with all JARs and launcher scripts ("dist/mkPack")
 *   - Building all scaladoc sets ("doc")
 *   - Publishing ("publishDists" and standard sbt tasks like "publish" and "publishLocal")
 *
 * You'll notice that this build definition is much more complicated than your typical sbt build.
 * The main reason is that we are not benefiting from sbt's conventions when it comes project
 * layout. For that reason we have to configure a lot more explicitly. I've tried explain in
 * comments the less obvious settings.
 *
 * This nicely leads me to explaining goal and non-goals of this build definition. Goals are:
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

// Scala dependencies:
val scalaSwingDep                = scalaDep("org.scala-lang.modules", "scala-swing")
val scalaXmlDep                  = scalaDep("org.scala-lang.modules", "scala-xml")
val scalaParserCombinatorsDep    = scalaDep("org.scala-lang.modules", "scala-parser-combinators")

// Non-Scala dependencies:
val junitDep          = "junit"                          % "junit"           % "4.12"
val junitInterfaceDep = "com.novocode"                   % "junit-interface" % "0.11"                            % Test
val scalacheckDep     = "org.scalacheck"                %% "scalacheck"      % "1.18.0"                          % Test
val jolDep            = "org.openjdk.jol"                % "jol-core"        % "0.16"
val asmDep            = "org.scala-lang.modules"         % "scala-asm"       % versionProps("scala-asm.version")
val jlineDep          = "jline"                          % "jline"           % versionProps("jline.version")
val jansiDep          = "org.fusesource.jansi"           % "jansi"           % "1.12"
val antDep            = "org.apache.ant"                 % "ant"             % "1.10.12"
val testInterfaceDep  = "org.scala-sbt"                  % "test-interface"  % "1.0"
val diffUtilsDep      = "com.googlecode.java-diff-utils" % "diffutils"       % "1.3.0"

/** Publish to ./dists/maven-sbt, similar to the Ant build which publishes to ./dists/maven. This
  * can be used to compare the output of the sbt and Ant builds during the transition period. Any
  * real publishing should be done with sbt's standard `publish` task. */
lazy val publishDists = taskKey[Unit]("Publish to ./dists/maven-sbt.")

(Global / credentials) ++= {
  val file = Path.userHome / ".credentials"
  if (file.exists && !file.isDirectory) List(Credentials(file))
  else Nil
}

lazy val publishSettings : Seq[Setting[_]] = Seq(
  publishDists := {
    val artifacts = (publish / packagedArtifacts).value
    val ver = VersionUtil.versionProperties.value.canonicalVersion
    val log = streams.value.log
    val mappings = artifacts.toSeq.map { case (a, f) =>
      val typeSuffix = a.`type` match {
        case "pom" => "-pom.xml"
        case "jar" => ".jar"
        case "doc" => "-docs.jar"
        case tpe => s"-$tpe.${a.extension}"
      }
      val to = file("dists/maven-sbt") / ver / a.name / (a.name + typeSuffix)
      log.info(s"Publishing $f to $to")
      (f, to)
    }
    IO.copy(mappings)
  },
  credentials ++= {
    val file = Path.userHome / ".credentials"
    if (file.exists && !file.isDirectory) List(Credentials(file))
    else Nil
  },
  // Add a "default" Ivy configuration because sbt expects the Scala distribution to have one:
  ivyConfigurations += Configuration.of("Default", "default", "Default", true, Vector(Configurations.Runtime), true),
  publishMavenStyle := true
)

// Set the version number: We use the two settings `baseVersion` and `baseVersionSuffix` to compute all versions
// (canonical, Maven, OSGi). See VersionUtil.versionPropertiesImpl for details. The standard sbt `version` setting
// should not be set directly. It is the same as the Maven version and derived automatically from `baseVersion` and
// `baseVersionSuffix`.
globalVersionSettings
(Global / baseVersion)       := "2.12.21"
(Global / baseVersionSuffix) := "SNAPSHOT"
(ThisBuild / organization)   := "org.scala-lang"
(ThisBuild / homepage)       := Some(url("https://www.scala-lang.org"))
(ThisBuild / startYear)      := Some(2002)
(ThisBuild / licenses)       += (("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0")))
(ThisBuild / headerLicense)  := Some(HeaderLicense.Custom(
  s"""Scala (${(ThisBuild / homepage).value.get})
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

(Global / scalaVersion)         := versionProps("starr.version")

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
  (Compile / javacOptions) ++= Seq("-g", "-source", "1.8", "-target", "1.8", "-Xlint:unchecked"),
  (Compile / unmanagedJars) := Seq.empty,  // no JARs in version control!
  (Compile / sourceDirectory) := baseDirectory.value,
  (Compile / unmanagedSourceDirectories) := List(baseDirectory.value),
  (Compile / unmanagedResourceDirectories) += (ThisBuild / baseDirectory).value / "src" / thisProject.value.id,
  sourcesInBase := false,
  (Compile / scalaSource) := (Compile / sourceDirectory).value,
  (Compile / javaSource) := (Compile / sourceDirectory).value,
  // resources are stored along source files in our current layout
  (Compile / resourceDirectory) := (Compile / sourceDirectory).value,
  // each subproject has to ask specifically for files they want to include
  (Compile / unmanagedResources / includeFilter) := NothingFilter,
  target := (ThisBuild / baseDirectory).value / "target" / thisProject.value.id,
  (Compile / classDirectory) := buildDirectory.value / "quick/classes" / thisProject.value.id,
  (Compile / doc  / target) := buildDirectory.value / "scaladoc" / thisProject.value.id,
  // given that classDirectory and doc target are overridden to be _outside_ of target directory, we have
  // to make sure they are being cleaned properly
  cleanFiles += (Compile / classDirectory).value,
  cleanFiles += (Compile / doc / target).value,
  (run / fork) := true,
  (run / connectInput) := true,
  (Compile / scalacOptions) += "-Ywarn-unused:imports",
  // work around https://github.com/scala/bug/issues/11534
  Compile / scalacOptions += "-Wconf:cat=unchecked&msg=The outer reference in this type test cannot be checked at run time.:s",
  (Compile / doc / scalacOptions) ++= Seq(
    "-doc-footer", "epfl",
    "-diagrams",
    "-implicits",
    "-groups",
    "-doc-version", versionProperties.value.canonicalVersion,
    "-doc-title", description.value,
    "-sourcepath", (ThisBuild / baseDirectory).value.toString,
    "-doc-source-url", s"https://github.com/scala/scala/tree/${versionProperties.value.githubTree}€{FILE_PATH_EXT}#L€{FILE_LINE}"
  ),
  setIncOptions,
  apiURL := Some(url("https://www.scala-lang.org/api/" + versionProperties.value.mavenVersion + "/")),
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
  (Compile / packageBin / packageOptions) := Seq.empty,
  (Compile / packageSrc / packageOptions) := Seq.empty,

  // Lets us CTRL-C partest without exiting SBT entirely
  (Global / cancelable) := true,

  // Don't pick up source files from the project root.
  sourcesInBase := false,

  // Don't log process output (e.g. of forked `compiler/runMain ...Main`), just pass it
  // directly to stdout
  (run / outputStrategy) := Some(StdoutOutput)
) ++ removePomDependencies ++ setForkedWorkingDirectory

/** Extra post-processing for the published POM files. These are needed to create POMs that
  * are equivalent to the ones from the Ant build. In the long term this should be removed and
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

val pomDependencyExclusions =
  settingKey[Seq[(String, String)]]("List of (groupId, artifactId) pairs to exclude from the POM and ivy.xml")

(Global / pomDependencyExclusions) := Nil

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
        case n => Seq(n)
      }
    }).transform(Seq(n2)).head
  },
  deliverLocal := {
    import scala.xml._
    import scala.xml.transform._
    val f = deliverLocal.value
    val deps = pomDependencyExclusions.value
    val e = new RuleTransformer(new RewriteRule {
      override def transform(node: Node) = node match {
        case e: Elem if e.label == "dependency" && {
          val org = e.attribute("org").getOrElse("").toString
          val name = e.attribute("name").getOrElse("").toString
          deps.exists { case (g, a) =>
             org == g && (name == a || name == (a + "_" + scalaBinaryVersion.value))
          }
        } => Seq.empty
        case n => Seq(n)
      }
    }).transform(Seq(XML.loadFile(f))).head
    XML.save(f.getAbsolutePath, e, xmlDecl = true)
    f
  }
)

val disableDocs = Seq[Setting[_]](
  (Compile / doc / sources) := Seq.empty,
  (Compile / packageDoc / publishArtifact) := false
)

val disablePublishing = Seq[Setting[_]](
  publishArtifact := false,
  // The above is enough for Maven repos but it doesn't prevent publishing of ivy.xml files
  publish := {},
  publishLocal := {},
)

lazy val setJarLocation: Setting[_] =
  (Compile / packageBin / artifactPath) := {
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
  (Compile / doc / sources) ~= (_.filter(ff.accept)),
  // Excluded sources may still be referenced by the included sources, so we add the compiler
  // output to the scaladoc classpath to resolve them. For the `library` project this is
  // always required because otherwise the compiler cannot even initialize Definitions without
  // binaries of the library on the classpath. Specifically, we get this error:
  // (library/compile:doc) scala.reflect.internal.FatalError: package class scala does not have a member Int
  (Compile / doc / dependencyClasspath) += (Compile / classDirectory).value,
  (Compile / doc) := (Compile / doc).dependsOn((Compile / compile)).value
)

def regexFileFilter(s: String): FileFilter = new FileFilter {
  val pat = s.r.pattern
  def accept(f: File) = pat.matcher(f.getAbsolutePath.replace('\\', '/')).matches()
}

def setForkedWorkingDirectory: Seq[Setting[_]] = {
  // When we fork subprocesses, use the base directory as the working directory.
  // This“ enables `sbt> partest test/files/run/t1.scala` or `sbt> scalac sandbox/test.scala`
  val setting = (Compile / forkOptions) := (Compile / forkOptions).value.withWorkingDirectory((ThisBuild / baseDirectory).value)
  setting ++ inTask(run)(setting)
}

// This project provides the STARR scalaInstance for bootstrapping
lazy val bootstrap = project in file("target/bootstrap")

lazy val library = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings)
  .settings(Osgi.settings)
  .settings(AutomaticModuleName.settings("scala.library"))
  .settings(
    name := "scala-library",
    description := "Scala Standard Library",
    compileOrder := CompileOrder.Mixed, // needed for JFunction classes in scala.runtime.java8
    (Compile / scalacOptions) ++= Seq[String]("-sourcepath", (Compile / scalaSource).value.toString),
    (Compile / doc/ scalacOptions) ++= {
      val libraryAuxDir = (ThisBuild / baseDirectory).value / "src/library-aux"
      Seq(
        "-doc-no-compile", libraryAuxDir.toString,
        "-skip-packages", "scala.concurrent.impl",
        "-doc-root-content", (Compile / sourceDirectory).value + "/rootdoc.txt"
      )
    },
    (Compile / unmanagedResources / includeFilter) := "*.tmpl" | "*.xml" | "*.js" | "*.css" | "rootdoc.txt",
    // Include *.txt files in source JAR:
    (Compile / packageSrc / mappings) ++= {
      val base = (Compile / unmanagedResourceDirectories).value
      base ** "*.txt" pair Path.relativeTo(base)
    },
    Osgi.headers += "Import-Package" -> "sun.misc;resolution:=optional, *",
    Osgi.jarlist := true,
    fixPom(
      "/project/name" -> <name>Scala Library</name>,
      "/project/description" -> <description>Standard library for the Scala Programming Language</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    ),
    // Remove the dependency on "forkjoin" from the POM because it is included in the JAR:
    pomDependencyExclusions += ((organization.value, "forkjoin")),
    MimaFilters.mimaSettings,
  )
  .settings(filterDocSources("*.scala" -- (regexFileFilter(".*/runtime/.*\\$\\.scala") ||
                                           regexFileFilter(".*/runtime/ScalaRunTime\\.scala") ||
                                           regexFileFilter(".*/runtime/StringAdd\\.scala"))))

lazy val reflect = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings)
  .settings(Osgi.settings)
  .settings(AutomaticModuleName.settings("scala.reflect"))
  .settings(
    name := "scala-reflect",
    description := "Scala Reflection Library",
    Osgi.bundleName := "Scala Reflect",
    (Compile / doc / scalacOptions) ++= Seq(
      "-skip-packages", "scala.reflect.macros.internal:scala.reflect.internal:scala.reflect.io"
    ),
    Osgi.headers +=
      "Import-Package" -> ("scala.*;version=\"${range;[==,=+);${ver}}\","+
                           "scala.tools.nsc;resolution:=optional;version=\"${range;[==,=+);${ver}}\","+
                           "*"),
    fixPom(
      "/project/name" -> <name>Scala Reflect</name>,
      "/project/description" -> <description>Reflection Library for the Scala Programming Language</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    ),
    MimaFilters.mimaSettings,
  )
  .dependsOn(library)

lazy val compilerOptionsExporter = Project("compilerOptionsExporter", file(".") / "src" / "compilerOptionsExporter")
  .dependsOn(compiler, reflect, library)
  .settings(clearSourceAndResourceDirectories)
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    libraryDependencies ++= {
      val jacksonVersion = "2.18.0"
      Seq(
        "com.fasterxml.jackson.core" % "jackson-core" % jacksonVersion,
        "com.fasterxml.jackson.core" % "jackson-annotations" % jacksonVersion,
        "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion,
        "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % jacksonVersion,
        "com.fasterxml.jackson.module" %% "jackson-module-scala" % jacksonVersion,
      )
    }
  )

lazy val compiler = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings)
  .settings(generateBuildCharacterFileSettings)
  .settings(Osgi.settings)
  .settings(AutomaticModuleName.settings("scala.tools.nsc"))
  .settings(
    name := "scala-compiler",
    description := "Scala Compiler",
    libraryDependencies ++= Seq(antDep, asmDep),
    // These are only needed for the POM.
    libraryDependencies ++= Seq(scalaXmlDep, jlineDep % "optional"),
    buildCharacterPropertiesFile := (Compile / resourceManaged).value / "scala-buildcharacter.properties",
    (Compile / resourceGenerators) += generateBuildCharacterPropertiesFile.map(file => Seq(file)).taskValue,
    // this a way to make sure that classes from interactive and scaladoc projects
    // end up in compiler jar. note that we need to use LocalProject references
    // (with strings) to deal with mutual recursion
    (Compile / packageBin / products) :=
      (Compile / packageBin / products).value ++
        Seq((Compile / dependencyClasspath).value.find(_.get(moduleID.key).map(id => (id.organization, id.name, id.revision)).contains((asmDep.organization, asmDep.name, asmDep.revision))).get.data) ++
        (LocalProject("interactive") / Compile / packageBin / products).value ++
        (LocalProject("scaladoc") / Compile / packageBin / products).value ++
        (LocalProject("repl") / Compile / packageBin / products).value ++
        (LocalProject("repl-jline")  / Compile / packageBin / products).value ++
        (LocalProject("repl-jline-embedded") / Compile / packageBin / products).value,
    (Compile / unmanagedResources / includeFilter) :=
      "*.tmpl" | "*.xml" | "*.js" | "*.css" | "*.html" | "*.properties" | "*.swf" |
      "*.png" | "*.gif" | "*.gif" | "*.txt",
    // Also include the selected unmanaged resources and source files from the additional projects in the source JAR:
    (Compile / packageSrc / mappings) ++= {
      val base = (Compile / unmanagedResourceDirectories).value ++
        (LocalProject("interactive") / Compile / unmanagedResourceDirectories).value ++
        (LocalProject("scaladoc") / Compile / unmanagedResourceDirectories).value ++
        (LocalProject("repl")/ Compile / unmanagedResourceDirectories).value
      base ** ((Compile / unmanagedResources / includeFilter).value || "*.scala" || "*.psd" || "*.ai" || "*.java") pair Path.relativeTo(base)
    },
    // Include the additional projects in the scaladoc JAR:
    (Compile / doc / sources) ++= {
      val base =
        (LocalProject("interactive") / Compile / unmanagedSourceDirectories).value ++
        (LocalProject("scaladoc") / Compile / unmanagedSourceDirectories).value ++
        (LocalProject("repl") / Compile / unmanagedSourceDirectories).value
      ((base ** ("*.scala" || "*.java"))
        --- (base ** "Scaladoc*ModelTest.scala") // exclude test classes that depend on partest
      ).get
    },
    (Compile / doc / scalacOptions) ++= Seq(
      "-doc-root-content", (Compile / sourceDirectory).value + "/rootdoc.txt"
    ),
    Osgi.headers ++= Seq(
      "Import-Package" -> ("jline.*;resolution:=optional," +
                           "org.apache.tools.ant.*;resolution:=optional," +
                           "scala.xml.*;version=\"${range;[====,====];"+versionNumber("scala-xml")+"}\";resolution:=optional," +
                           "scala.*;version=\"${range;[==,=+);${ver}}\"," +
                           "*"),
      "Class-Path" -> "scala-reflect.jar scala-library.jar"
    ),
    // Generate the ScriptEngineFactory service definition. The Ant build does this when building
    // the JAR but sbt has no support for it and it is easier to do as a resource generator:
    generateServiceProviderResources("javax.script.ScriptEngineFactory" -> "scala.tools.nsc.interpreter.Scripted$Factory"),
    (Compile / managedResourceDirectories) := Seq((Compile / resourceManaged).value),
    fixPom(
      "/project/name" -> <name>Scala Compiler</name>,
      "/project/description" -> <description>Compiler for the Scala Programming Language</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    ),
    apiURL := None,
    pomDependencyExclusions ++= List(("org.apache.ant", "ant"), ("org.scala-lang.modules", "scala-asm"))
  )
  .dependsOn(library, reflect)

lazy val interactive = configureAsSubproject(project)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    name := "scala-compiler-interactive",
    description := "Scala Interactive Compiler"
  )
  .dependsOn(compiler)

lazy val repl = configureAsSubproject(project)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    (run / connectInput) := true,
    run := (Compile / run).partialInput(" -usejavacp").evaluated // Automatically add this so that `repl/run` works without additional arguments.
  )
  .dependsOn(compiler, interactive)

lazy val replJline = configureAsSubproject(Project("repl-jline", file(".") / "src" / "repl-jline"))
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    libraryDependencies += jlineDep,
    name := "scala-repl-jline"
  )
  .dependsOn(repl)

lazy val replJlineEmbedded = Project("repl-jline-embedded", file(".") / "target" / "repl-jline-embedded-src-dummy")
  .settings(scalaSubprojectSettings)
  .settings(disablePublishing)
  .settings(
    name := "scala-repl-jline-embedded",
    // There is nothing to compile for this project. Instead we use the compile task to create
    // shaded versions of repl-jline and jline.jar. dist/mkBin puts all of quick/repl,
    // quick/repl-jline and quick/repl-jline-shaded on the classpath for quick/bin scripts.
    // This is different from the Ant build where all parts are combined into quick/repl, but
    // it is cleaner because it avoids circular dependencies.
    (Compile / compile) := (Compile / compile).dependsOn(Def.task {
      import java.util.jar._
      import collection.JavaConverters._
      val inputs: Iterator[JarJar.Entry] = {
        val repljlineClasses = (replJline / Compile/ products).value.flatMap(base => Path.allSubpaths(base).map(x => (base, x._1)))
        val jlineJAR = findJar((Compile / dependencyClasspath).value, jlineDep).get.data
        val jarFile = new JarFile(jlineJAR)
        val jarEntries = jarFile.entries.asScala.filterNot(_.isDirectory).map(entry => JarJar.JarEntryInput(jarFile, entry))
        def compiledClasses = repljlineClasses.iterator.map { case (base, file) => JarJar.FileInput(base, file) }
        (jarEntries ++ compiledClasses).filter(x =>
          x.name.endsWith(".class") || x.name.endsWith(".properties") || x.name.startsWith("META-INF/native") || x.name.startsWith("META-INF/maven")
        )
      }
      import JarJar.JarJarConfig._
      val config: Seq[JarJar.JarJarConfig] = Seq(
        Rule("org.fusesource.**", "scala.tools.fusesource_embedded.@1"),
        Rule("jline.**", "scala.tools.jline_embedded.@1"),
        Rule("scala.tools.nsc.interpreter.jline.**", "scala.tools.nsc.interpreter.jline_embedded.@1"),
        Keep("scala.tools.**")
      )
      val outdir = (Compile / classDirectory).value
      JarJar(inputs, outdir, config)
    }).value,
    (run / connectInput) := true

  )
  .dependsOn(replJline)

lazy val scaladoc = configureAsSubproject(project)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    name := "scala-compiler-doc",
    description := "Scala Documentation Generator",
    libraryDependencies ++= Seq(scalaXmlDep),
    (Compile / unmanagedResources / includeFilter) := "*.html" | "*.css" | "*.gif" | "*.png" | "*.js" | "*.txt" | "*.svg" | "*.eot" | "*.woff" | "*.ttf",
    libraryDependencies ++= ScaladocSettings.webjarResources,
    (Compile / resourceGenerators) += ScaladocSettings.extractResourcesFromWebjar
  )
  .dependsOn(compiler)

lazy val scalap = configureAsSubproject(project)
  .settings(
    description := "Scala Bytecode Parser",
    // Include decoder.properties
    (Compile / unmanagedResources / includeFilter) := "*.properties",
    fixPom(
      "/project/name" -> <name>Scalap</name>,
      "/project/description" -> <description>bytecode analysis tool</description>,
      "/project/properties" -> scala.xml.Text("")
    ),
    headerLicense  := Some(HeaderLicense.Custom(
      s"""Scala classfile decoder (${(ThisBuild / homepage).value.get})
         |
         |Copyright EPFL and Lightbend, Inc.
         |
         |Licensed under Apache License 2.0
         |(http://www.apache.org/licenses/LICENSE-2.0).
         |
         |See the NOTICE file distributed with this work for
         |additional information regarding copyright ownership.
         |""".stripMargin)),
    (Compile / headerSources) ~= { xs =>
      val excluded = Set("Memoisable.scala", "Result.scala", "Rule.scala", "Rules.scala", "SeqRule.scala")
      xs filter { x => !excluded(x.getName) }
    },
    (Compile / headerResources) := Nil
  )
  .dependsOn(compiler)

lazy val partest = configureAsSubproject(project)
  .dependsOn(library, reflect, compiler, scalap, replJlineEmbedded, scaladoc)
  .settings(Osgi.settings)
  .settings(AutomaticModuleName.settings("scala.partest"))
  .settings(
    name := "scala-partest",
    description := "Scala Compiler Testing Tool",
    Compile / javacOptions += "-XDenableSunApiLintControl",
    libraryDependencies ++= List(testInterfaceDep, diffUtilsDep, junitDep),
    pomDependencyExclusions ++= List((organization.value, "scala-repl-jline-embedded"), (organization.value, "scala-compiler-doc")),
    fixPom(
      "/project/name" -> <name>Scala Partest</name>,
      "/project/description" -> <description>Scala Compiler Testing Tool</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    ),
  )

// An instrumented version of BoxesRunTime and ScalaRunTime for partest's "specialized" test category
lazy val specLib = project.in(file("test") / "instrumented")
  .dependsOn(library, reflect, compiler)
  .settings(clearSourceAndResourceDirectories)
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    (Compile / sourceGenerators) += Def.task {
      import scala.collection.JavaConverters._
      val srcBase = (library / Compile / sourceDirectories).value.head / "scala/runtime"
      val targetBase = (Compile / sourceManaged).value / "scala/runtime"
      def patch(srcFile: String, patchFile: String): File = try {
        val patchLines: List[String] = IO.readLines(baseDirectory.value / patchFile)
        val origLines: List[String] = IO.readLines(srcBase / srcFile)
        import difflib.DiffUtils
        val p = DiffUtils.parseUnifiedDiff(patchLines.asJava)
        val r = DiffUtils.patch(origLines.asJava, p)
        val target = targetBase / srcFile
        val patched = r.asScala.toList
        IO.writeLines(target, patched)
        if (patched == origLines) {
          println(p)
          println(patchLines.mkString("\n"))
          println(origLines.mkString("\n"))
          throw new RuntimeException("Patch did not apply any changes! " + baseDirectory.value / patchFile + " / " + (srcBase / srcFile))
        }

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
    }.taskValue
  )

lazy val bench = project.in(file("test") / "benchmarks")
  .dependsOn(library, compiler)
  .settings(instanceSettings)
  .settings(disableDocs)
  .settings(disablePublishing)
  .enablePlugins(JmhPlugin)
  .settings(
    name := "test-benchmarks",
    libraryDependencies += "org.openjdk.jol" % "jol-core" % "0.6",
    compileOrder := CompileOrder.JavaThenScala, // to allow inlining from Java ("... is defined in a Java source (mixed compilation), no bytecode is available")
    scalacOptions ++= Seq("-feature", "-opt:l:inline", "-opt-inline-from:scala/**", "-opt-warnings"),
  ).settings(inConfig(JmhPlugin.JmhKeys.Jmh)(scalabuild.JitWatchFilePlugin.jitwatchSettings))

// Jigsaw: reflective access between modules (`setAccessible(true)`) requires an `opens` directive.
// This is enforced by error (not just by warning) since JDK 16. In our tests we use reflective access
// from the unnamed package (the classpath) to JDK modules in testing utilities like `assertNotReachable`.
// `add-exports=jdk.jdeps/com.sun.tools.javap` is tests that use `:javap` in the REPL, see scala/bug#12378
val addOpensForTesting = "-XX:+IgnoreUnrecognizedVMOptions" +: "--add-exports=jdk.jdeps/com.sun.tools.javap=ALL-UNNAMED" +:
  Seq("java.util.concurrent.atomic", "java.lang", "java.lang.reflect", "java.net").map(p => s"--add-opens=java.base/$p=ALL-UNNAMED")

lazy val junit = project.in(file("test") / "junit")
  .dependsOn(library, reflect, compiler, partest, scaladoc)
  .settings(clearSourceAndResourceDirectories)
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    (Test / fork) := true,
    (Test / javaOptions) ++= "-Xss1M" +: addOpensForTesting,
    (Test / forkOptions) := (Test / forkOptions).value.withWorkingDirectory((ThisBuild / baseDirectory).value),
    (Test / testOnly / forkOptions) := (Test / testOnly / forkOptions).value.withWorkingDirectory((ThisBuild / baseDirectory).value),
    libraryDependencies ++= Seq(junitDep, junitInterfaceDep, jolDep),
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s"),
    (Compile / unmanagedSourceDirectories) := Nil,
    (Test / unmanagedSourceDirectories) := List(baseDirectory.value),
    Test / headerSources := Nil,
  )

lazy val scalacheck = project.in(file("test") / "scalacheck")
  .dependsOn(library, reflect, compiler, scaladoc)
  .settings(clearSourceAndResourceDirectories)
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    // enable forking to workaround https://github.com/sbt/sbt/issues/4009
    (Test / fork) := true,
    (Test / javaOptions) ++= "-Xss1M" +: addOpensForTesting,
    testOptions ++= {
      if ((Test / fork).value) Nil
      else List(Tests.Cleanup { loader =>
        sbt.internal.inc.ModuleUtilities.getObject("scala.TestCleanup", loader).asInstanceOf[Runnable].run()
      })
    },
    libraryDependencies ++= Seq(scalacheckDep),
    (Compile / unmanagedSourceDirectories) := Nil,
    (Test / unmanagedSourceDirectories) := List(baseDirectory.value)
  ).settings(
    // Workaround for https://github.com/sbt/sbt/pull/3985
    List(Keys.test, Keys.testOnly).map(task => (task / parallelExecution) := false) : _*
  )

lazy val osgiTestFelix = osgiTestProject(
  project.in(file(".") / "target" / "osgiTestFelix"),
  "org.apache.felix" % "org.apache.felix.framework" % "5.6.10")

lazy val osgiTestEclipse = osgiTestProject(
  project.in(file(".") / "target" / "osgiTestEclipse"),
  "org.eclipse.tycho" % "org.eclipse.osgi" % "3.13.0.v20180226-1711")

def osgiTestProject(p: Project, framework: ModuleID) = p
  .dependsOn(library, reflect, compiler)
  .settings(clearSourceAndResourceDirectories)
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    (Test / fork) := true,
    (Test / parallelExecution) := false,
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
        framework % "test"
      )
    },
    (Test / Keys.test) := (Test / Keys.test).dependsOn((Compile / packageBin)).value,
    (Test / Keys.testOnly) := (Test / Keys.testOnly).dependsOn((Compile / packageBin)).evaluated,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-q"),
    (Test / javaOptions) ++= ("-Dscala.bundle.dir=" + (ThisBuild / buildDirectory).value / "osgi") +: addOpensForTesting,
    (test / Test / forkOptions) := (test / Test / forkOptions).value.withWorkingDirectory((ThisBuild / baseDirectory).value),
    (Test / unmanagedSourceDirectories) := List((ThisBuild / baseDirectory).value / "test" / "osgi" / "src"),
    (Compile / unmanagedResourceDirectories) := (Test / unmanagedSourceDirectories).value,
    (Compile / unmanagedResources / includeFilter) := "*.xml",
    (Compile / packageBin) := { // Put the bundle JARs required for the tests into build/osgi
      val targetDir = (ThisBuild / buildDirectory).value / "osgi"
      val mappings = ((dist / mkPack).value / "lib").listFiles.collect {
        case f if f.getName.startsWith("scala-") && f.getName.endsWith(".jar") => (f, targetDir / f.getName)
      }
      IO.copy(mappings, CopyOptions() withOverwrite true)
      targetDir
    },
    cleanFiles += (ThisBuild / buildDirectory).value / "osgi"
  )

lazy val partestJavaAgent = Project("partest-javaagent", file(".") / "src" / "partest-javaagent")
  .settings(commonSettings)
  .settings(generatePropertiesFileSettings)
  .settings(disableDocs)
  .settings(
    libraryDependencies += asmDep,
    publishLocal := {},
    publish := {},
    // Setting name to "scala-partest-javaagent" so that the jar file gets that name, which the Runner relies on
    name := "scala-partest-javaagent",
    description := "Scala Compiler Testing Tool (compiler-specific java agent)",
    // add required manifest entry - previously included from file
    (Compile / packageBin / packageOptions) +=
      Package.ManifestAttributes( "Premain-Class" -> "scala.tools.partest.javaagent.ProfilingAgent" ),
    // we need to build this to a JAR
    exportJars := true
  )

lazy val test = project
  .dependsOn(compiler, interactive, replJlineEmbedded, scalap, partest, partestJavaAgent, scaladoc)
  .disablePlugins(plugins.JUnitXmlReportPlugin)
  .configs(IntegrationTest)
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(Defaults.itSettings)
  .settings(
    libraryDependencies ++= Seq(asmDep, scalaXmlDep),
    // no main sources
    (Compile / sources) := Seq.empty,
    // test sources are compiled in partest run, not here
    (IntegrationTest / sources) := Seq.empty,
    (IntegrationTest / fork) := true,
    // enable this in 2.13, when tests pass
    //scalacOptions in Compile += "-Yvalidate-pos:parser,typer",
    (Compile / scalacOptions) -= "-Ywarn-unused:imports",
    (IntegrationTest / javaOptions) ++= List("-Xmx2G", "-Dpartest.exec.in.process=true", "-Dfile.encoding=UTF-8", "-Duser.language=en", "-Duser.country=US") ++ addOpensForTesting,
    IntegrationTest / javaOptions ++= { if (scala.util.Properties.isJavaAtLeast("18")) List("-Djava.security.manager=allow") else Nil },
    (IntegrationTest / testOptions) += Tests.Argument("-Dfile.encoding=UTF-8", "-Duser.language=en", "-Duser.country=US"),
    testFrameworks += new TestFramework("scala.tools.partest.sbt.Framework"),
    (IntegrationTest / testOptions) += Tests.Argument(s"""-Dpartest.java_opts=-Xmx1024M -Xms64M ${addOpensForTesting.mkString(" ")}"""),
    (IntegrationTest / testOptions) += Tests.Argument("-Dpartest.scalac_opts=" + (Compile / scalacOptions).value.mkString(" ")),
    (IntegrationTest / forkOptions) := (IntegrationTest / forkOptions).value.withWorkingDirectory((ThisBuild / baseDirectory).value),
    (IntegrationTest / testOptions) += {
      val cp = (Test / dependencyClasspath).value
      val baseDir = (ThisBuild / baseDirectory).value
      val instrumentedJar = (LocalProject("specLib") / Compile / packageBin / packagedArtifact).value._2
      Tests.Setup { () =>
        // Copy code.jar (resolved in the otherwise unused scope "test") and instrumented.jar (from specLib)to the location where partest expects them
        IO.copyFile(instrumentedJar, baseDir / "test/files/speclib/instrumented.jar")
      }
    },
    (IntegrationTest / definedTests) += new sbt.TestDefinition(
      "partest",
      // marker fingerprint since there are no test classes
      // to be discovered by sbt:
      new sbt.testing.AnnotatedFingerprint {
        def isModule = true
        def annotationName = "partest"
      }, true, Array()
    ),
    (IntegrationTest / executeTests) := {
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
    (IntegrationTest / testListeners) += new PartestTestListener(target.value)
  )

lazy val manual = configureAsSubproject(project)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    libraryDependencies ++= Seq(scalaXmlDep, antDep, "org.scala-lang" % "scala-library" % scalaVersion.value),
    (Compile / classDirectory) := (Compile / target).value / "classes"
  )

lazy val libraryAll = Project("library-all", file(".") / "target" / "library-all-src-dummy")
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(
    name := "scala-library-all",
    (Compile / packageBin / publishArtifact) := false,
    (Compile / packageSrc / publishArtifact) := false,
    libraryDependencies ++= Seq(scalaXmlDep, scalaParserCombinatorsDep, scalaSwingDep),
    apiURL := None,
    fixPom(
      "/project/name" -> <name>Scala Library Powerpack</name>,
      "/project/description" -> <description>The Scala Standard Library and Official Modules</description>
    )
  )
  .dependsOn(library, reflect)

lazy val scalaDist = Project("scala-dist", file(".") / "target" / "scala-dist-dist-src-dummy")
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(
    (Compile / packageBin / mappings) ++= {
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
    (Compile / resourceGenerators) += Def.task {
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
        // what the Ant build does.
        IO.write(out, IO.readBytes(in).filterNot(_ == '\r'))
      }
      (htmlOut ** "*.html").get ++ (fixedManOut ** "*.1").get
    }.taskValue,
    (Compile / managedResourceDirectories) := Seq((Compile / resourceManaged).value),
    libraryDependencies += jlineDep,
    apiURL := None,
    fixPom(
      "/project/name" -> <name>Scala Distribution Artifacts</name>,
      "/project/description" -> <description>The Artifacts Distributed with Scala</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    ),
    (Compile / packageSrc / publishArtifact) := false
  )
  .dependsOn(libraryAll, compiler, scalap)

lazy val root: Project = (project in file("."))
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(generateBuildCharacterFileSettings)
  .settings(
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
      val dir = (((ThisBuild / baseDirectory).value) / "src" / "library" / "scala")
      genprod.main(Array(dir.getPath))
      GenerateAnyVals.run(dir.getAbsoluteFile)
      state
    },

    testAll := {
      val results = ScriptCommands.sequence[(Result[Unit], String)](List(
        (junit / Test / Keys.test).result map (_ -> "junit/test"),
        (scalacheck / Test / Keys.test).result map (_ -> "scalacheck/test"),
        (testP / IntegrationTest / testOnly).toTask(" -- run").result map (_ -> "partest run"),
        (testP / IntegrationTest / testOnly).toTask(" -- pos neg jvm").result map (_ -> "partest pos neg jvm"),
        (testP / IntegrationTest / testOnly).toTask(" -- res scalap specialized").result map (_ -> "partest res scalap specialized"),
        (testP / IntegrationTest / testOnly).toTask(" -- instrumented presentation").result map (_ -> "partest instrumented presentation"),
        (testP / IntegrationTest / testOnly).toTask(" -- --srcpath scaladoc").result map (_ -> "partest --srcpath scaladoc"),
        (testP / IntegrationTest / testOnly).toTask(" -- --srcpath async").result map (_ -> "partest --srcpath async"),
        (osgiTestFelix / Test / Keys.test).result map (_ -> "osgiTestFelix/test"),
        (osgiTestEclipse / Test / Keys.test).result map (_ -> "osgiTestEclipse/test"),
        (library / mimaReportBinaryIssues).result.map(_ -> "library/mimaReportBinaryIssues"), // doesn't aggregate..
        (reflect / mimaReportBinaryIssues).result.map(_ -> "reflect/mimaReportBinaryIssues"), // ..so specify both
        (bench / Compile / compile).map(_ => ()).result map (_ -> "bench/compile"),
        Def.task(()).dependsOn( // Run these in parallel:
          (library / Compile / doc),
          (reflect / Compile / doc),
          (compiler / Compile / doc),
          (scalap / Compile / doc)
        ).result map (_ -> "doc")
      )).value
      val log = streams.value.log
      val failed = results.collect { case (Inc(i), d) => (i, d) }
      if (failed.nonEmpty) {
        def showScopedKey(k: Def.ScopedKey[_]): String =
          Vector(
            k.scope.project.toOption.map {
              case p: ProjectRef => p.project
              case p => p
            }.map(_ + "/"),
            k.scope.config.toOption.map(_.name + ":"),
            k.scope.task.toOption.map(_.label + "::")
          ).flatten.mkString + k.key
        val loggedThis, loggedAny = new scala.collection.mutable.HashSet[String]
        def findRootCauses(i: Incomplete, currentTask: String): Vector[(String, Option[Throwable])] = {
          val sk = i.node match {
            case Some(t: Task[_]) =>
              t.info.attributes.entries.collectFirst { case e if e.key == Keys.taskDefinitionKey => e.value.asInstanceOf[Def.ScopedKey[_]] }
                .map(showScopedKey)
            case _ => None
          }
          val task = sk.getOrElse(currentTask)
          val dup = sk.exists(s => !loggedAny.add(s))
          if(sk.exists(s => !loggedThis.add(s))) Vector.empty
          else i.directCause match {
            case Some(e) => Vector((task, if(dup) None else Some(e)))
            case None => i.causes.toVector.flatMap(ch => findRootCauses(ch, task))
          }
        }
        log.error(s"${failed.size} of ${results.length} test tasks failed:")
        failed.foreach { case (i, d) =>
          log.error(s"- $d")
          loggedThis.clear
          findRootCauses(i, "<unkown task>").foreach {
            case (task, Some(ex)) => log.error(s"  - $task failed: $ex")
            case (task, None)     => log.error(s"  - ($task failed)")
          }
        }
        throw new RuntimeException
      }
    },
    setIncOptions
  )
  .aggregate(library, reflect, compiler, compilerOptionsExporter, interactive, repl, replJline, replJlineEmbedded,
    scaladoc, scalap, partest, junit, libraryAll, scalaDist).settings(
    (Compile / sources) := Seq.empty,
    onLoadMessage := s"""|*** Welcome to the sbt build definition for Scala! ***
      |version=${(Global / version).value} scalaVersion=${(Global / scalaVersion).value}
      |Check README.md for more information.""".stripMargin
  )

def setIncOptions = incOptions := {
  incOptions.value
    .withRecompileOnMacroDef(Some(Boolean box false).asJava) // macros in library+reflect are hard-wired to implementations with `FastTrack`.
}

// The following subprojects' binaries are required for building "pack":
lazy val distDependencies = Seq(replJline, replJlineEmbedded, compiler, library, reflect, scalap, scaladoc)

lazy val dist = (project in file("dist"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(scalaSwingDep, jlineDep),
    mkBin := mkBinImpl.value,
    mkQuick := Def.task {
      val cp = (LocalProject("test") / IntegrationTest / fullClasspath).value
      val propsFile = (ThisBuild / buildDirectory).value / "quick" / "partest.properties"
      val props = new java.util.Properties()
      props.setProperty("partest.classpath", cp.map(_.data.getAbsolutePath).mkString(sys.props("path.separator")))
      IO.write(props, null, propsFile)
      (ThisBuild / buildDirectory).value / "quick"
    }.dependsOn((distDependencies.map((_ / Runtime / products)) :+ mkBin): _*).value,
    mkPack := Def.task { (ThisBuild / buildDirectory).value / "pack" }.dependsOn((Compile / packageBin / packagedArtifact), mkBin).value,
    target := (ThisBuild / baseDirectory).value / "target" / thisProject.value.id,
    (Compile / packageBin) := {
      val extraDeps = Set(scalaSwingDep, scalaParserCombinatorsDep, scalaXmlDep)
      val targetDir = (ThisBuild / buildDirectory).value / "pack" / "lib"
      def uniqueModule(m: ModuleID) = (m.organization, m.name.replaceFirst("_.*", ""))
      val extraModules = extraDeps.map(uniqueModule)
      val extraJars = (Compile / externalDependencyClasspath).value.map(a => (a.get(moduleID.key), a.data)).collect {
        case (Some(m), f) if extraModules contains uniqueModule(m) => f
      }
      val jlineJAR = findJar((Compile / dependencyClasspath).value, jlineDep).get.data
      val mappings = extraJars.map(f => (f, targetDir / f.getName)) :+ ((jlineJAR, targetDir / "jline.jar"))
      IO.copy(mappings, CopyOptions() withOverwrite true)
      targetDir
    },
    cleanFiles += (ThisBuild / buildDirectory).value / "quick",
    cleanFiles += (ThisBuild / buildDirectory).value / "pack",
    (Compile / packageBin / packagedArtifact) :=
      (Compile / packageBin / packagedArtifact)
        .dependsOn(distDependencies.map((_ / Runtime / packageBin/ packagedArtifact)): _*)
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
def configureAsSubproject(project: Project): Project = {
  val base = file(".") / "src" / project.id
  (project in base)
    .settings(scalaSubprojectSettings)
}

lazy val buildDirectory = settingKey[File]("The directory where all build products go. By default ./build")
lazy val mkBin = taskKey[Seq[File]]("Generate shell script (bash or Windows batch).")
lazy val mkQuick = taskKey[File]("Generate a full build, including scripts, in build/quick")
lazy val mkPack = taskKey[File]("Generate a full build, including scripts, in build/pack")
lazy val testAll = taskKey[Unit]("Run all test tasks sequentially")

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

  mkBin("scala"    , "scala.tools.nsc.MainGenericRunner", (replJlineEmbedded / Compile / fullClasspath).value) ++
  mkBin("scalac"   , "scala.tools.nsc.Main",              (compiler / Compile / fullClasspath).value) ++
  mkBin("fsc"      , "scala.tools.nsc.CompileClient",     (compiler / Compile /  fullClasspath).value) ++
  mkBin("scaladoc" , "scala.tools.nsc.ScalaDoc",          (scaladoc / Compile /  fullClasspath).value) ++
  mkBin("scalap"   , "scala.tools.scalap.Main",           (scalap / Compile /  fullClasspath).value)
}

/** Generate service provider definition files under META-INF/services */
def generateServiceProviderResources(services: (String, String)*): Setting[_] =
  (Compile / resourceGenerators) += Def.task {
    services.map { case (k, v) =>
      val f = (Compile / resourceManaged).value / "META-INF/services" / k
      IO.write(f, v + "\n")
      f
    }
  }.taskValue

(ThisBuild / buildDirectory) := (ThisBuild / baseDirectory).value / "build"

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
       ("scala",    "repl-jline-embedded", "scala.tools.nsc.MainGenericRunner"),
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
      // moduleDeps(dist).value,                // No sources, therefore no module in IntelliJ
      moduleDeps(interactive).value,
      moduleDeps(junit).value,
      moduleDeps(library).value,
      // moduleDeps(libraryAll).value,          // No sources
      moduleDeps(manual).value,
      moduleDeps(partest).value,
      moduleDeps(partestJavaAgent).value,
      moduleDeps(reflect).value,
      moduleDeps(repl).value,
      moduleDeps(replJline).value,
      // moduleDeps(replJlineEmbedded).value,   // No sources
      // moduleDeps(root).value,                // No sources
      // moduleDeps(scalaDist).value,           // No sources
      moduleDeps(scalacheck, config = Test).value,
      moduleDeps(scaladoc).value,
      moduleDeps(scalap).value,
      moduleDeps(testP).value,
      moduleDeps(compilerOptionsExporter).value
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
        case e @ Elem(_, "library", attrs, _, _, _*) if checkAttrs(attrs) =>
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

Global / excludeLintKeys := (Global / excludeLintKeys).value ++ Set(scalaSource, javaSource, resourceDirectory)

{
  scala.build.TravisOutput.installIfOnTravis()
  Nil
}
