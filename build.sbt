/*
 * The new, sbt-based build definition for Scala.
 *
 * What you see below is very much work-in-progress. The following features are implemented:
 *   - Compiling all classses for the compiler and library ("compile" in the respective subprojects)
 *   - Running JUnit tests ("test") and partest ("test/it:test")
 *   - Creating build-sbt/quick with all compiled classes and launcher scripts ("dist/mkQuick")
 *   - Creating build-sbt/pack with all JARs and launcher scripts ("dist/mkPack")
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

import VersionUtil._

val bootstrapScalaVersion = versionProps("starr.version")

def withoutScalaLang(moduleId: ModuleID): ModuleID = moduleId exclude("org.scala-lang", "*")

// exclusion of the scala-library transitive dependency avoids eviction warnings during `update`.
val actorsMigrationDep = withoutScalaLang("org.scala-lang" %% "scala-actors-migration" % versionNumber("actors-migration"))
val akkaActorDep = withoutScalaLang("com.typesafe.akka" %% "akka-actor" % versionNumber("akka-actor"))
val scalaContinuationsLibraryDep = withoutScalaLang("org.scala-lang.plugins" %% "scala-continuations-library" % versionNumber("scala-continuations-library"))
val scalaContinuationsPluginDep = withoutScalaLang("org.scala-lang.plugins" % ("scala-continuations-plugin_" + versionProps("scala.full.version")) % versionNumber("scala-continuations-plugin"))
val scalaParserCombinatorsDep = withoutScalaLang("org.scala-lang.modules" %% "scala-parser-combinators" % versionNumber("scala-parser-combinators"))
val scalaSwingDep = withoutScalaLang("org.scala-lang.modules" %% "scala-swing" % versionNumber("scala-swing"))
val scalaXmlDep = withoutScalaLang("org.scala-lang.modules" %% "scala-xml" % versionNumber("scala-xml"))
val partestDep = withoutScalaLang("org.scala-lang.modules" %% "scala-partest" % versionNumber("partest"))
val junitDep = "junit" % "junit" % "4.11"
val junitIntefaceDep = "com.novocode" % "junit-interface" % "0.11" % "test"
val asmDep = "org.scala-lang.modules" % "scala-asm" % versionProps("scala-asm.version")
val jlineDep = "jline" % "jline" % versionProps("jline.version")
val antDep = "org.apache.ant" % "ant" % "1.9.4"
val scalacheckDep = withoutScalaLang("org.scalacheck" %% "scalacheck" % versionNumber("scalacheck") % "it")

/** Publish to ./dists/maven-sbt, similar to the ANT build which publishes to ./dists/maven. This
  * can be used to compare the output of the sbt and ANT builds during the transition period. Any
  * real publishing should be done with sbt's standard `publish` task. */
lazy val publishDists = taskKey[Unit]("Publish to ./dists/maven-sbt.")

lazy val publishSettings : Seq[Setting[_]] = Seq(
  publishDists := {
    val artifacts = (packagedArtifacts in publish).value
    val ver = VersionUtil.versionProperties.value.canonicalVersion
    val log = streams.value.log
    val mappings = artifacts.toSeq.map { case (a, f) =>
      val typeSuffix = a.`type` match {
        case "pom" => "-pom.xml"
        case "bundle" | "jar" => ".jar"
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
    if (file.exists) List(Credentials(file))
    else Nil
  },
  publishMavenStyle := true
)

// Set the version number: The ANT build uses the file "build.number" to get the base version. Overriding versions or
// suffixes for certain builds is done by directly setting variables from the shell scripts. For example, in
// publish-core this requires computing the commit SHA first and then passing it to ANT. In the sbt build we use
// the two settings `baseVersion` and `baseVersionSuffix` to compute all versions (canonical, Maven, OSGi). See
// VersionUtil.versionPropertiesImpl for details. The standard sbt `version` setting should not be set directly. It
// is the same as the Maven version and derived automatically from `baseVersion` and `baseVersionSuffix`.
globalVersionSettings
baseVersion in Global := "2.11.9"
baseVersionSuffix in Global := "SNAPSHOT"

lazy val commonSettings = clearSourceAndResourceDirectories ++ publishSettings ++ Seq[Setting[_]](
  organization := "org.scala-lang",
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
  ),
  homepage := Some(url("http://www.scala-lang.org")),
  startYear := Some(2002),
  licenses += ("BSD 3-Clause", url("http://www.scala-lang.org/license.html")),
  apiURL := Some(url("http://www.scala-lang.org/api/" + versionProperties.value.mavenVersion + "/")),
  pomIncludeRepository := { _ => false },
  pomExtra := {
    val base =
      <scm>
        <connection>scm:git:git://github.com/scala/scala.git</connection>
        <url>https://github.com/scala/scala.git</url>
      </scm>
        <issueManagement>
          <system>JIRA</system>
          <url>https://issues.scala-lang.org/</url>
        </issueManagement>
        <developers>
          <developer>
            <id>lamp</id>
            <name>EPFL LAMP</name>
          </developer>
          <developer>
            <id>Lightbend</id>
            <name>Lightbend, Inc.</name>
          </developer>
        </developers>
    apiURL.value match {
      case Some(url) => base ++
        <properties>
          <info.apiURL>{url.toString}</info.apiURL>
        </properties>
      case None => base
    }
  },
  // Remove auto-generated manifest attributes
  packageOptions in Compile in packageBin := Seq.empty,
  packageOptions in Compile in packageSrc := Seq.empty,

  // Lets us CTRL-C partest without exiting SBT entirely
  cancelable in Global := true,
  // When we fork subprocesses, use the base directory as the working directory.
  // This enables `sbt> partest test/files/run/t1.scala` or `sbt> scalac sandbox/test.scala`
  baseDirectory in Compile := (baseDirectory in ThisBuild).value,
  baseDirectory in Test := (baseDirectory in ThisBuild).value,

  // Don't log process output (e.g. of forked `compiler/runMain ...Main`), just pass it
  // directly to stdout
  outputStrategy in run := Some(StdoutOutput)
)

/** Extra post-processing for the published POM files. These are needed to create POMs that
  * are equivalent to the ones from the ANT build. In the long term this should be removed and
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
        <url>http://lamp.epfl.ch/</url>
      </organization>,
    "/project/url" -> <url>http://www.scala-lang.org/</url>
  ) ++ extra) }
}

/** Remove unwanted dependencies from the POM. */
def removePomDependencies(deps: (String, String)*): Setting[_] = {
  pomPostProcess := { n =>
    val n2 = pomPostProcess.value.apply(n)
    import scala.xml._
    import scala.xml.transform._
    (new RuleTransformer(new RewriteRule {
      override def transform(node: Node) = node match {
        case e: Elem if e.label == "dependency" &&
            deps.exists { case (g, a) =>
              e.child.contains(<groupId>{g}</groupId>) &&
                (e.child.contains(<artifactId>{a}</artifactId>) || e.child.contains(<artifactId>{a + "_" + scalaBinaryVersion.value}</artifactId>))
            } => Seq.empty
        case n => Seq(n)
      }
    })).transform(Seq(n2)).head
  }
}

val disableDocs = Seq[Setting[_]](
  sources in (Compile, doc) := Seq.empty,
  publishArtifact in (Compile, packageDoc) := false
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
  .settings(Osgi.settings: _*)
  .settings(
    name := "scala-library",
    description := "Scala Standard Library",
    scalacOptions in Compile ++= Seq[String]("-sourcepath", (scalaSource in Compile).value.toString),
    scalacOptions in Compile in doc ++= {
      val libraryAuxDir = (baseDirectory in ThisBuild).value / "src/library-aux"
      Seq(
        "-doc-no-compile", libraryAuxDir.toString,
        "-skip-packages", "scala.concurrent.impl",
        "-doc-root-content", (sourceDirectory in Compile).value + "/rootdoc.txt"
      )
    },
    includeFilter in unmanagedResources in Compile := "*.tmpl" | "*.xml" | "*.js" | "*.css" | "rootdoc.txt",
    // Include *.txt files in source JAR:
    mappings in Compile in packageSrc ++= {
      val base = (unmanagedResourceDirectories in Compile).value
      base ** "*.txt" pair relativeTo(base)
    },
    // Include forkjoin classes in scala-library.jar
    products in Compile in packageBin ++=
      (products in Compile in packageBin in forkjoin).value,
    Osgi.headers += "Import-Package" -> "sun.misc;resolution:=optional, *",
    fixPom(
      "/project/name" -> <name>Scala Library</name>,
      "/project/description" -> <description>Standard library for the Scala Programming Language</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    ),
    // Remove the dependency on "forkjoin" from the POM because it is included in the JAR:
    removePomDependencies(("org.scala-lang", "forkjoin"))
  )
  .settings(filterDocSources("*.scala" -- (regexFileFilter(".*/runtime/.*\\$\\.scala") ||
                                           regexFileFilter(".*/runtime/ScalaRunTime\\.scala") ||
                                           regexFileFilter(".*/runtime/StringAdd\\.scala"))): _*)
  .dependsOn(forkjoin)

lazy val reflect = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings: _*)
  .settings(Osgi.settings: _*)
  .settings(
    name := "scala-reflect",
    description := "Scala Reflection Library",
    Osgi.bundleName := "Scala Reflect",
    scalacOptions in Compile in doc ++= Seq(
      "-skip-packages", "scala.reflect.macros.internal:scala.reflect.internal:scala.reflect.io"
    ),
    Osgi.headers +=
      "Import-Package" -> ("scala.*;version=\"${range;[==,=+);${ver}}\","+
                           "scala.tools.nsc;resolution:=optional;version=\"${range;[==,=+);${ver}}\","+
                           "*"),
    fixPom(
      "/project/name" -> <name>Scala Compiler</name>,
      "/project/description" -> <description>Compiler for the Scala Programming Language</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    )
  )
  .dependsOn(library)

lazy val compiler = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings: _*)
  .settings(Osgi.settings: _*)
  .settings(
    name := "scala-compiler",
    description := "Scala Compiler",
    libraryDependencies ++= Seq(antDep, asmDep),
    // These are only needed for the POM:
    libraryDependencies ++= Seq(scalaXmlDep, scalaParserCombinatorsDep, jlineDep % "optional"),
    // this a way to make sure that classes from interactive and scaladoc projects
    // end up in compiler jar (that's what Ant build does)
    // we need to use LocalProject references (with strings) to deal with mutual recursion
    products in Compile in packageBin :=
      (products in Compile in packageBin).value ++
        Seq((dependencyClasspath in Compile).value.find(_.get(moduleID.key) == Some(asmDep)).get.data) ++
        (products in Compile in packageBin in LocalProject("interactive")).value ++
        (products in Compile in packageBin in LocalProject("scaladoc")).value ++
        (products in Compile in packageBin in LocalProject("repl")).value ++
        (products in Compile in packageBin in LocalProject("repl-jline")).value ++
        (products in Compile in packageBin in LocalProject("repl-jline-embedded")).value,
    includeFilter in unmanagedResources in Compile :=
      "*.tmpl" | "*.xml" | "*.js" | "*.css" | "*.html" | "*.properties" | "*.swf" |
      "*.png" | "*.gif" | "*.gif" | "*.txt",
    // Also include the selected unmanaged resources and source files from the additional projects in the source JAR:
    mappings in Compile in packageSrc ++= {
      val base = (unmanagedResourceDirectories in Compile).value ++
        (unmanagedResourceDirectories in Compile in LocalProject("interactive")).value ++
        (unmanagedResourceDirectories in Compile in LocalProject("scaladoc")).value ++
        (unmanagedResourceDirectories in Compile in LocalProject("repl")).value
      base ** ((includeFilter in unmanagedResources in Compile).value || "*.scala" || "*.psd" || "*.ai" || "*.java") pair relativeTo(base)
    },
    scalacOptions in Compile in doc ++= Seq(
      "-doc-root-content", (sourceDirectory in Compile).value + "/rootdoc.txt"
    ),
    Osgi.headers +=
      "Import-Package" -> ("jline.*;resolution:=optional," +
                           "org.apache.tools.ant.*;resolution:=optional," +
                           "scala.util.parsing.*;version=\"${range;[====,====];"+versionNumber("scala-parser-combinators")+"}\";resolution:=optional," +
                           "scala.xml.*;version=\"${range;[====,====];"+versionNumber("scala-xml")+"}\";resolution:=optional," +
                           "scala.*;version=\"${range;[==,=+);${ver}}\"," +
                           "*"),
    // Generate the ScriptEngineFactory service definition. The ant build does this when building
    // the JAR but sbt has no support for it and it is easier to do as a resource generator:
    generateServiceProviderResources("javax.script.ScriptEngineFactory" -> "scala.tools.nsc.interpreter.IMain$Factory"),
    managedResourceDirectories in Compile := Seq((resourceManaged in Compile).value),
    fixPom(
      "/project/name" -> <name>Scala Compiler</name>,
      "/project/description" -> <description>Compiler for the Scala Programming Language</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    ),
    apiURL := None,
    removePomDependencies(
      ("org.apache.ant", "ant"),
      ("org.scala-lang.modules", "scala-asm")
    )
  )
  .dependsOn(library, reflect)

lazy val interactive = configureAsSubproject(project)
  .settings(disableDocs: _*)
  .settings(
    name := "scala-compiler-interactive",
    description := "Scala Interactive Compiler",
    publishArtifact := false
  )
  .dependsOn(compiler)

lazy val repl = configureAsSubproject(project)
  .settings(disableDocs: _*)
  .settings(
    connectInput in run := true,
    publishArtifact := false,
    run <<= (run in Compile).partialInput(" -usejavacp") // Automatically add this so that `repl/run` works without additional arguments.
  )
  .dependsOn(compiler, interactive)

lazy val replJline = configureAsSubproject(Project("repl-jline", file(".") / "src" / "repl-jline"))
  .settings(disableDocs: _*)
  .settings(
    libraryDependencies += jlineDep,
    name := "scala-repl-jline",
    publishArtifact := false
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
    publishArtifact := false,
    connectInput in run := true
  )
  .dependsOn(replJline)

lazy val scaladoc = configureAsSubproject(project)
  .settings(disableDocs: _*)
  .settings(
    name := "scala-compiler-doc",
    description := "Scala Documentation Generator",
    libraryDependencies ++= Seq(scalaXmlDep, scalaParserCombinatorsDep, partestDep),
    publishArtifact := false,
    includeFilter in unmanagedResources in Compile := "*.html" | "*.css" | "*.gif" | "*.png" | "*.js" | "*.txt"
  )
  .dependsOn(compiler)

lazy val scalap = configureAsSubproject(project)
  .settings(
    description := "Scala Bytecode Parser",
    // Include decoder.properties
    includeFilter in unmanagedResources in Compile := "*.properties",
    fixPom(
      "/project/name" -> <name>Scalap</name>,
      "/project/description" -> <description>bytecode analysis tool</description>,
      "/project/properties" -> scala.xml.Text("")
    )
  )
  .dependsOn(compiler)

// deprecated Scala Actors project
lazy val actors = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings: _*)
  .settings(Osgi.settings: _*)
  .settings(
    name := "scala-actors",
    description := "Scala Actors Library",
    Osgi.bundleName := "Scala Actors",
    startYear := Some(2006),
    fixPom(
      "/project/name" -> <name>Scala Actors library</name>,
      "/project/description" -> <description>Deprecated Actors Library for Scala</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    )
  )
  .settings(filterDocSources("*.scala"): _*)
  .dependsOn(library)

lazy val forkjoin = configureAsForkOfJavaProject(project)

lazy val partestExtras = configureAsSubproject(Project("partest-extras", file(".") / "src" / "partest-extras"))
  .dependsOn(replJlineEmbedded)
  .settings(clearSourceAndResourceDirectories: _*)
  .settings(disableDocs: _*)
  .settings(
    name := "scala-partest-extras",
    description := "Scala Compiler Testing Tool (compiler-specific extras)",
    publishArtifact := false,
    libraryDependencies += partestDep,
    unmanagedSourceDirectories in Compile := List(baseDirectory.value)
  )

lazy val junit = project.in(file("test") / "junit")
  .dependsOn(library, reflect, compiler, partestExtras, scaladoc)
  .settings(clearSourceAndResourceDirectories: _*)
  .settings(commonSettings: _*)
  .settings(disableDocs: _*)
  .settings(
    publishArtifact := false,
    fork in Test := true,
    libraryDependencies ++= Seq(junitDep, junitIntefaceDep),
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v"),
    unmanagedSourceDirectories in Test := List(baseDirectory.value)
  )

lazy val partestJavaAgent = Project("partest-javaagent", file(".") / "src" / "partest-javaagent")
  .settings(commonSettings: _*)
  .settings(generatePropertiesFileSettings: _*)
  .settings(disableDocs: _*)
  .settings(
    libraryDependencies += asmDep,
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

lazy val test = project
  .dependsOn(compiler, interactive, actors, replJlineEmbedded, scalap, partestExtras, partestJavaAgent, scaladoc)
  .configs(IntegrationTest)
  .settings(commonSettings: _*)
  .settings(disableDocs: _*)
  .settings(Defaults.itSettings: _*)
  .settings(
    publishArtifact := false,
    libraryDependencies ++= Seq(asmDep, partestDep, scalaXmlDep, scalacheckDep),
    unmanagedBase in IntegrationTest := baseDirectory.value / "files" / "lib",
    unmanagedJars in IntegrationTest <+= (unmanagedBase) (j => Attributed.blank(j)) map(identity),
    // no main sources
    sources in Compile := Seq.empty,
    // test sources are compiled in partest run, not here
    sources in IntegrationTest := Seq.empty,
    fork in IntegrationTest := true,
    javaOptions in IntegrationTest += "-Xmx1G",
    testFrameworks += new TestFramework("scala.tools.partest.sbt.Framework"),
    testOptions in IntegrationTest += Tests.Setup( () => root.base.getAbsolutePath + "/pull-binary-libs.sh" ! ),
    testOptions in IntegrationTest += Tests.Argument("-Dpartest.java_opts=-Xmx1024M -Xms64M -XX:MaxPermSize=128M"),
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
  .settings(disableDocs: _*)
  .settings(
    publishArtifact := false,
    libraryDependencies ++= Seq(scalaXmlDep, antDep),
    classDirectory in Compile := (target in Compile).value / "classes"
  )
  .dependsOn(library)

lazy val libraryAll = Project("library-all", file(".") / "target" / "library-all-src-dummy")
  .settings(commonSettings: _*)
  .settings(disableDocs: _*)
  .settings(
    name := "scala-library-all",
    publishArtifact in (Compile, packageBin) := false,
    publishArtifact in (Compile, packageSrc) := false,
    libraryDependencies ++= Seq(scalaXmlDep, scalaParserCombinatorsDep, scalaContinuationsLibraryDep, scalaSwingDep, akkaActorDep, actorsMigrationDep),
    apiURL := None,
    fixPom(
      "/project/name" -> <name>Scala Library Powerpack</name>,
      "/project/description" -> <description>The Scala Standard Library and Official Modules</description>
    )
  )
  .dependsOn(library, reflect)

lazy val scalaDist = Project("scala-dist", file(".") / "target" / "scala-dist-dist-src-dummy")
  .settings(commonSettings: _*)
  .settings(disableDocs: _*)
  .settings(
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
    libraryDependencies ++= Seq(scalaContinuationsPluginDep, jlineDep),
    apiURL := None,
    fixPom(
      "/project/name" -> <name>Scala Distribution Artifacts</name>,
      "/project/description" -> <description>The Artifacts Distributed with Scala</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    ),
    publishArtifact in (Compile, packageSrc) := false
  )
  .dependsOn(libraryAll, compiler, scalap)

lazy val root = (project in file("."))
  .settings(disableDocs: _*)
  .settings(generateBuildCharacterFileSettings: _*)
  .settings(
    publishArtifact := false,
    publish := {},
    publishLocal := {},
    commands ++= ScriptCommands.all
  )
  .aggregate(library, forkjoin, reflect, compiler, interactive, repl, replJline, replJlineEmbedded,
    scaladoc, scalap, actors, partestExtras, junit, libraryAll, scalaDist).settings(
    sources in Compile := Seq.empty,
    onLoadMessage := """|*** Welcome to the sbt build definition for Scala! ***
      |This build definition has an EXPERIMENTAL status. If you are not
      |interested in testing or working on the build itself, please use
      |the Ant build definition for now. Check README.md for more information.""".stripMargin
  )

// The following subprojects' binaries are required for building "pack":
lazy val distDependencies = Seq(replJline, replJlineEmbedded, compiler, library, partestExtras, partestJavaAgent, reflect, scalap, actors, scaladoc)

lazy val dist = (project in file("dist"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(scalaContinuationsLibraryDep, scalaContinuationsPluginDep, scalaSwingDep, jlineDep),
    mkBin := mkBinImpl.value,
    mkQuick <<= Def.task {
      val cp = (fullClasspath in IntegrationTest in LocalProject("test")).value
      val propsFile = (buildDirectory in ThisBuild).value / "quick" / "partest.properties"
      val props = new java.util.Properties()
      props.setProperty("partest.classpath", cp.map(_.data.getAbsolutePath).mkString(sys.props("path.separator")))
      IO.write(props, null, propsFile)
    } dependsOn ((distDependencies.map(products in Runtime in _) :+ mkBin): _*),
    mkPack <<= Def.task {} dependsOn (packagedArtifact in (Compile, packageBin), mkBin),
    target := (baseDirectory in ThisBuild).value / "target" / thisProject.value.id,
    packageBin in Compile := {
      val extraDeps = Set(scalaContinuationsLibraryDep, scalaContinuationsPluginDep, scalaSwingDep, scalaParserCombinatorsDep, scalaXmlDep)
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
    packagedArtifact in (Compile, packageBin) <<= (packagedArtifact in (Compile, packageBin)).dependsOn(distDependencies.map(packagedArtifact in (Compile, packageBin) in _): _*)
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
    .settings(scalaSubprojectSettings: _*)
    .settings(generatePropertiesFileSettings: _*)
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
  (project in base)
    .settings(commonSettings: _*)
    .settings(disableDocs: _*)
    .settings(
      publishArtifact := false,
      sourceDirectory in Compile := baseDirectory.value,
      javaSource in Compile := (sourceDirectory in Compile).value,
      sources in Compile in doc := Seq.empty,
      classDirectory in Compile := buildDirectory.value / "libs/classes" / thisProject.value.id
    )
}

lazy val buildDirectory = settingKey[File]("The directory where all build products go. By default ./build")
lazy val mkBin = taskKey[Seq[File]]("Generate shell script (bash or Windows batch).")
lazy val mkQuick = taskKey[Unit]("Generate a full build, including scripts, in build-sbt/quick")
lazy val mkPack = taskKey[Unit]("Generate a full build, including scripts, in build-sbt/pack")

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

// Add tab completion to partest
commands += Command("partest")(_ => PartestUtil.partestParser((baseDirectory in ThisBuild).value, (baseDirectory in ThisBuild).value / "test")) { (state, parsed) =>
  ("test/it:testOnly -- " + parsed) :: state
}

// Add tab completion to scalac et al.
commands ++= {
  val commands =
  List(("scalac",   "compiler", "scala.tools.nsc.Main"),
       ("scala",    "repl-jline-embedded", "scala.tools.nsc.MainGenericRunner"),
       ("scaladoc", "scaladoc", "scala.tools.nsc.ScalaDoc"))

  commands.map {
    case (entryPoint, projectRef, mainClassName) =>
      Command(entryPoint)(_ => ScalaOptionParser.scalaParser(entryPoint, (baseDirectory in ThisBuild).value)) { (state, parsedOptions) =>
        (projectRef + "/runMain " + mainClassName + " -usejavacp " + parsedOptions) :: state
      }
  }
}

addCommandAlias("scalap",   "scalap/compile:runMain              scala.tools.scalap.Main -usejavacp")

lazy val intellij = taskKey[Unit]("Update the library classpaths in the IntelliJ project files.")

def moduleDeps(p: Project) = (externalDependencyClasspath in Compile in p).map(a => (p.id, a.map(_.data)))

// aliases to projects to prevent name clashes
def compilerP = compiler
def testP = test

intellij := {
  import xml._
  import xml.transform._

  val s = streams.value

  val modules: List[(String, Seq[File])] = {
    // for the sbt build module, the dependencies are fetched from the project's build using sbt-buildinfo
    val buildModule = ("scala-build", scalabuild.BuildInfo.buildClasspath.split(":").toSeq.map(new File(_)))
    // `sbt projects` lists all modules in the build
    buildModule :: List(
      moduleDeps(actors).value,
      moduleDeps(compilerP).value,
      // moduleDeps(dist).value,                // No sources, therefore no module in IntelliJ
      moduleDeps(forkjoin).value,
      moduleDeps(interactive).value,
      moduleDeps(junit).value,
      moduleDeps(library).value,
      // moduleDeps(libraryAll).value,          // No sources
      moduleDeps(manual).value,
      moduleDeps(partestExtras).value,
      moduleDeps(partestJavaAgent).value,
      moduleDeps(reflect).value,
      moduleDeps(repl).value,
      moduleDeps(replJline).value,
      // moduleDeps(replJlineEmbedded).value,   // No sources
      // moduleDeps(root).value,                // No sources
      // moduleDeps(scalaDist).value,           // No sources
      moduleDeps(scaladoc).value,
      moduleDeps(scalap).value,
      moduleDeps(testP).value)
  }

  def moduleDep(name: String, jars: Seq[File]) = {
    val entries = jars.map(f => s"""        <root url="jar://${f.toURI.getRawPath}!/" />""").mkString("\n")
    s"""|    <library name="${name}-deps">
        |      <CLASSES>
        |$entries
        |      </CLASSES>
        |      <JAVADOC />
        |      <SOURCES />
        |    </library>""".stripMargin
  }

  def starrDep(jars: Seq[File]) = {
    val entries = jars.map(f => s"""          <root url="file://${f.toURI.getRawPath}" />""").mkString("\n")
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

  val ipr = (baseDirectory in ThisBuild).value / "src/intellij/scala.ipr"

  var continue = false
  if (!ipr.exists) {
    scala.Console.print(s"Could not find src/intellij/scala.ipr. Create new project files from src/intellij/*.SAMPLE (y/N)? ")
    if (scala.Console.readLine() == "y") {
      intellijCreateFromSample((baseDirectory in ThisBuild).value)
      continue = true
    }
  } else {
    scala.Console.print("Update library classpaths in the current src/intellij/scala.ipr (y/N)? ")
    continue = scala.Console.readLine() == "y"
  }
  if (continue) {
    s.log.info("Updating library classpaths in src/intellij/scala.ipr.")
    val content = XML.loadFile(ipr)

    val newStarr = replaceLibrary(content, "starr", Some("Scala"), starrDep((scalaInstance in LocalProject("compiler")).value.jars))
    val newModules = modules.foldLeft(newStarr)({
      case (res, (modName, jars)) =>
        if (jars.isEmpty) res // modules without dependencies
        else replaceLibrary(res, s"$modName-deps", None, moduleDep(modName, jars))
    })

    XML.save(ipr.getAbsolutePath, newModules)
  } else {
    s.log.info("Aborting.")
  }
}

lazy val intellijFromSample = taskKey[Unit]("Create fresh IntelliJ project files from src/intellij/*.SAMPLE.")

intellijFromSample := {
  val s = streams.value
  scala.Console.print(s"Create new project files from src/intellij/*.SAMPLE (y/N)? ")
  if (scala.Console.readLine() == "y")
    intellijCreateFromSample((baseDirectory in ThisBuild).value)
  else
    s.log.info("Aborting.")
}

def intellijCreateFromSample(basedir: File): Unit = {
  val files = basedir / "src/intellij" * "*.SAMPLE"
  val copies = files.get.map(f => (f, new File(f.getAbsolutePath.stripSuffix(".SAMPLE"))))
  IO.copy(copies, overwrite = true)
}

lazy val intellijToSample = taskKey[Unit]("Update src/intellij/*.SAMPLE using the current IntelliJ project files.")

intellijToSample := {
  val s = streams.value
  scala.Console.print(s"Update src/intellij/*.SAMPLE using the current IntelliJ project files (y/N)? ")
  if (scala.Console.readLine() == "y") {
    val basedir = (baseDirectory in ThisBuild).value
    val existing = basedir / "src/intellij" * "*.SAMPLE"
    IO.delete(existing.get)
    val current = basedir / "src/intellij" * ("*.iml" || "*.ipr")
    val copies = current.get.map(f => (f, new File(f.getAbsolutePath + ".SAMPLE")))
    IO.copy(copies)
  } else
    s.log.info("Aborting.")
}
