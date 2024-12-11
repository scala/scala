package scala.build

import sbt._
import sbt.Keys._
import java.io.File

import sbt.librarymanagement.{
  DependencyResolution, ScalaModuleInfo, UpdateConfiguration, UnresolvedWarningConfiguration
}

/**
  * Settings to support validation of TastyUnpickler against the release of dotty with the matching TASTy version
  */
object TastySupport {
  val supportedTASTyRelease = "3.6.2" // TASTY: 28.6-0
  val scala3Compiler = "org.scala-lang" % "scala3-compiler_3" % supportedTASTyRelease
  val scala3Library = "org.scala-lang" % "scala3-library_3" % supportedTASTyRelease

  val CompilerClasspath = Configuration.of("TastySupport.CompilerClasspath", "TastySupport.CompilerClasspath")
  val LibraryClasspath = Configuration.of("TastySupport.LibraryClasspath", "TastySupport.LibraryClasspath")
}

/** Settings needed to compile with Dotty,
 *  Only active when sbt is started with `sbt -Dscala.build.compileWithDotty=true`
 *  This is currently only used to check that the standard library compiles with
 *  Dotty in .travis.yml.
 */
object DottySupport {
  val dottyVersion = TastySupport.supportedTASTyRelease
  val compileWithDotty: Boolean =
    Option(System.getProperty("scala.build.compileWithDotty")).exists(_.toBoolean)
  lazy val commonSettings = Seq(
    Compile / scalacOptions ++= Seq(
      "-language:implicitConversions" // Avoid a million warnings
    )
  )
  lazy val librarySettings = Seq(
    // Needed to compile scala3-library together with scala-library
    compileOrder := CompileOrder.Mixed,

    // Add the scala3-library sources to the sourcepath and disable fatal warnings
    Compile / scalacOptions := {
      val old = (Compile / scalacOptions).value
      val withoutFatalWarnings = old.filterNot(opt => opt == "-Werror" || opt.startsWith("-Wconf"))

      val (beforeSourcepath, "-sourcepath" :: oldSourcepath :: afterSourcePath) = withoutFatalWarnings.span(_ != "-sourcepath")

      val newSourcepath =
        ((Compile / sourceManaged).value / "scala3-library-src").getAbsolutePath +
        File.pathSeparator + oldSourcepath

      beforeSourcepath ++ ("-sourcepath" :: newSourcepath :: afterSourcePath)
    },

    Compile / scalacOptions ++= Seq(
      "-Yerased-terms" // needed to compile scala3-library
    ),

    // Some files shouldn't be compiled
    unmanagedSources / excludeFilter ~= (old =>
      old ||
      "AnyVal.scala"
    ),

    // Add the sources of scala3-library to the current project to compile the
    // complete standard library of Dotty in one go.
    // Adapted from similar code in the scala-js build.
    Compile / sourceGenerators += Def.task {
      object DottyLibrarySourceFilter extends FileFilter {
        def accept(file: File): Boolean = {
          val name = file.getName
          file.isFile &&
            (name.endsWith(".scala") || name.endsWith(".java")) &&
            !Set("AnyKind.scala", "Matchable.scala").contains(name)
        }
      }

      val s = streams.value
      val cacheDir = s.cacheDirectory
      val trgDir = (Compile / sourceManaged).value / "scala3-library-src"

      val dottyLibrarySourceJar = fetchSourceJarOf(
        dependencyResolution.value,
        scalaModuleInfo.value,
        updateConfiguration.value,
        (update / unresolvedWarningConfiguration).value,
        streams.value.log,
        scalaOrganization.value %% "scala3-library" % scalaVersion.value)

      FileFunction.cached(cacheDir / s"fetchDottyLibrarySource",
        FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
        s.log.info(s"Unpacking scala3-library sources to $trgDir...")
        if (trgDir.exists)
          IO.delete(trgDir)
        IO.createDirectory(trgDir)
        IO.unzip(dottyLibrarySourceJar, trgDir)

        (trgDir ** DottyLibrarySourceFilter).get.toSet
      } (Set(dottyLibrarySourceJar)).toSeq
    }.taskValue
  )

  /** Fetch source jar for `moduleID` */
  def fetchSourceJarOf(
    dependencyRes: DependencyResolution,
    scalaInfo: Option[ScalaModuleInfo],
    updateConfig: UpdateConfiguration,
    warningConfig: UnresolvedWarningConfiguration,
    log: Logger,
    moduleID: ModuleID): File = {
    val sourceClassifiersConfig = sbt.librarymanagement.GetClassifiersConfiguration(
      sbt.librarymanagement.GetClassifiersModule(
        moduleID,
        scalaInfo,
        Vector(moduleID),
        Vector(Configurations.Default) ++ Configurations.default,
        Vector("sources")
      ),
      Vector.empty,
      updateConfig.withArtifactFilter(
        librarymanagement.ArtifactTypeFilter.allow(Artifact.DefaultSourceTypes)
      ),
      Artifact.DefaultSourceTypes.toVector,
      Vector.empty
    )

    dependencyRes.updateClassifiers(sourceClassifiersConfig, warningConfig, Vector.empty, log) match {
      case Right(report) =>
        val Vector(jar) = report.allFiles
        jar
      case _ =>
        throw new MessageOnlyException(
          s"Couldn't retrieve `$moduleID`.")
    }
  }
}
