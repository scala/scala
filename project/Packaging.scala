import sbt._
import Keys._
import ScalaBuildKeys._

/** All the settings related to *packaging* the built scala software. */
trait Packaging { self: ScalaBuild.type =>

  // --------------------------------------------------------------
  //  Packaging a distro
  // --------------------------------------------------------------
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
        )
    },
    // Add in some more dependencies
    makeDistMappings <+= (packageBin in swing in Compile) map (s => s -> "lib/scala-swing.jar"),
    makeDistMappings <+= (packageBin in scalaReflect in Compile) map (s => s -> "lib/scala-reflect.jar"),
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
      maps map (_._2) filter (_ startsWith "bin/") foreach (dir / _ setExecutable true)
      dir
    }
  )
  lazy val scaladist = (
    Project("dist", file("."))
    settings (scalaDistSettings: _*)
  )


// Helpers to make a distribution

  /** Generates runner scripts for distribution. */
  def genBinTask(
    runner: ScopedTask[ScalaToolRunner], 
    outputDir: ScopedSetting[File], 
    classpath: ScopedTask[Classpath], 
    useClasspath: Boolean
  ): Project.Initialize[sbt.Task[Seq[(File,String)]]] = {
    (runner, outputDir, classpath, streams) map { (runner, outDir, cp, s) =>
      IO.createDirectory(outDir)
      val classToFilename = Seq(
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
      def makeBinMappings(cls: String, binName: String): Seq[(File,String)] = {
        val file       = outDir / binName
        val winBinName = binName + ".bat"
        genBinFiles(cls, file)
        Seq( file -> ("bin/"+binName), outDir / winBinName -> ("bin/"+winBinName) )
      }
      classToFilename.flatMap((makeBinMappings _).tupled)
    }
  }
  /** Creates man pages for distribution. */
  def runManmakerTask(classpath: ScopedTask[Classpath], scalaRun: ScopedTask[ScalaRun], mainClass: String, dir: String, ext: String): Project.Initialize[Task[Seq[(File,String)]]] =
    (classpath, scalaRun, streams, target) map { (cp, runner, s, target) =>
      val binaries = Seq("fsc", "scala", "scalac", "scaladoc", "scalap")
      binaries map { bin =>
        val file = target / "man" / dir / (bin + ext)
        val classname = "scala.man1." + bin
        IO.createDirectory(file.getParentFile)
        toError(runner.run(mainClass, Build.data(cp), Seq(classname, file.getAbsolutePath), s.log))   
        file -> ("man/" + dir + "/" + bin + ext)
      }
    }
}
