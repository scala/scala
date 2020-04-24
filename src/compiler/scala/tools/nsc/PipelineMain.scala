/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.ConcurrentHashMap
import java.util.{Collections, Locale}

import javax.tools.Diagnostic.Kind
import javax.tools.{Diagnostic, DiagnosticListener, JavaFileObject, ToolProvider}

import scala.collection.JavaConverters._
import scala.collection.{mutable, parallel}
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.reflect.internal.util.{BatchSourceFile, FakePos, NoPosition, Position}
import scala.reflect.io.PlainNioFile
import scala.tools.nsc.PipelineMain._
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.{ConsoleReporter, Reporter}
import scala.tools.nsc.util.ClassPath
import scala.util.{Failure, Success, Try}

class PipelineMainClass(argFiles: Seq[Path], pipelineSettings: PipelineMain.PipelineSettings) {
  import pipelineSettings._
  private val pickleCache: Path = configuredPickleCache.getOrElse(Files.createTempDirectory("scala.picklecache"))
  private def cachePath(file: Path): Path = {
    val newExtension = if (useJars) ".jar" else ""
    val root = file.getRoot
    // An empty component on Unix, just the drive letter on Windows
    val validRootPathComponent = root.toString.replaceAllLiterally("/", "").replaceAllLiterally(":", "")
    val result = changeExtension(pickleCache.resolve(validRootPathComponent).resolve(root.relativize(file)).normalize(), newExtension)
    if (useJars) Files.createDirectories(result.getParent)
    strippedAndExportedClassPath.put(file.toRealPath().normalize(), result)
    result
  }

  private val strippedAndExportedClassPath = new ConcurrentHashMap[Path, Path]().asScala

  /** Forward errors to the (current) reporter. */
  protected def scalacError(msg: String): Unit = {
    reporter.error(FakePos("scalac"), msg + "\n  scalac -help  gives more information")
  }

  private var reporter: Reporter = _
  private val pool = new java.util.concurrent.ForkJoinPool(parallelism)
  implicit val executor = ExecutionContext.fromExecutor(pool, { t => t.printStackTrace(); sys.exit(-1) })

  def changeExtension(p: Path, newExtension: String): Path = {
    val fileName = p.getFileName.toString
    val changedFileName = fileName.lastIndexOf('.') match {
      case -1 => fileName + newExtension
      case n => fileName.substring(0, n) + newExtension
    }
    p.getParent.resolve(changedFileName)
  }

  def writeDotFile(logDir: Path, dependsOn: Map[Task, List[Dependency]]): Unit = {
    val builder = new java.lang.StringBuilder()
    builder.append("digraph projects {\n")
    for ((p, deps) <- dependsOn) {
      //builder.append("  node \"[]").append(p.label).append("\";\n")
      for (dep <- deps) {
        builder.append("   \"").append(p.label).append("\" -> \"").append(dep.t.label).append("\" [")
        if (dep.isMacro) builder.append("label=M")
        else if (dep.isPlugin) builder.append("label=P")
        builder.append("];\n")
      }
    }
    builder.append("}\n")
    val path = logDir.resolve("projects.dot")
    Files.write(path, builder.toString.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    reporter.echo("Wrote project dependency graph to: " + path.toAbsolutePath)
  }

  private case class Dependency(t: Task, isMacro: Boolean, isPlugin: Boolean)

  def process(): Boolean = {
    reporter = createReporter(new Settings(scalacError))
    reporter.echo(s"parallelism = $parallelism, strategy = $strategy")

    def commandFor(argFileArg: Path): Task = {
      val ss = new Settings(scalacError)
      val command = new CompilerCommand(s"@$argFileArg" :: Nil, ss)
      Task(argFileArg, command, command.files)
    }

    val projects: List[Task] = argFiles.toList.map(commandFor)

    if (reporter.hasErrors) return false

    val produces: Map[Path, Task] = projects.map(p => p.outputDir -> p).toMap

    val dependsOn: Map[Task, List[Dependency]] = projects.map { p =>
      def toTask(path: Path) = produces.get(path).filter(p != _)
      val macroDeps     = p.macroClassPath.flatMap(toTask).map(Dependency(_, isMacro = true, isPlugin = false))
      val pluginDeps    = p.pluginClassPath.flatMap(toTask).map(Dependency(_, isMacro = false, isPlugin = true))
      val classPathDeps = p.classPath.flatMap(toTask).filter(t => macroDeps.forall(_.t != t)).map(Dependency(_, isMacro = false, isPlugin = false))
      p -> (classPathDeps ::: macroDeps ::: pluginDeps)
    }.toMap.withDefaultValue(Nil)

    val dependedOn: Set[Task] = dependsOn.valuesIterator.flatten.map(_.t).toSet

    if (strategy != Traditional) {
      if (stripExternalClassPath) {
        val externalClassPath =
          for (p <- projects; e <- p.classPath; if !produces.contains(e) && Files.exists(e)) yield e
        val exportTimer = new Timer
        for (entry <- externalClassPath.distinct) {
          val extracted = cachePath(entry)
          val sourceTimeStamp = Files.getLastModifiedTime(entry)
          if (Files.exists(extracted) && Files.getLastModifiedTime(extracted) == sourceTimeStamp) {
            // println(s"Skipped export of pickles from $entry to $extracted (up to date)")
          } else {
            PickleExtractor.process(entry, extracted)
            Files.setLastModifiedTime(extracted, sourceTimeStamp)
            reporter.echo(s"Exported pickles from $entry to $extracted")
            Files.setLastModifiedTime(extracted, sourceTimeStamp)
          }
          strippedAndExportedClassPath(entry) = extracted
        }
        exportTimer.stop()
        reporter.echo(f"Exported external classpath in ${exportTimer.durationMs}%.0f ms")
      }
    }

    def sequenceFailSlow[A](fs: Seq[Future[A]]): Future[Seq[A]] = {
      Future.traverse(fs)(_.transform(tr => Success(tr.toEither))).map { results =>
        results.collect { case Left(throwable) => throwable }.toList match {
          case head :: rest => rest.foreach(head.addSuppressed(_)); throw head
          case _            => results.collect { case Right(value) => value }
        }
      }
    }

    def awaitDone(timer: Timer): Unit = {
      val allFutures = projects.flatMap(_.futures)
      val numAllFutures = allFutures.size
      val awaitAllFutures = sequenceFailSlow[Any](allFutures)
      var lastNumCompleted = allFutures.count(_.isCompleted)
      while (true) try {
        Await.result(awaitAllFutures, Duration(60, "s"))
        timer.stop()
        val numCompleted = allFutures.count(_.isCompleted)
        reporter.echo(s"PROGRESS: $numCompleted / $numAllFutures")
        return
      } catch {
        case _: TimeoutException =>
          val numCompleted = allFutures.count(_.isCompleted)
          if (numCompleted == lastNumCompleted) {
            reporter.echo(s"STALLED: $numCompleted / $numAllFutures")
            reporter.echo("Outline/Scala/Javac")
            def toX(b: Future[_]): String = b.value match {
              case None             => "-"
              case Some(Success(_)) => "x"
              case Some(Failure(_)) => "!"
            }
            projects.foreach { p =>
              val s = p.stages.map(toX).mkString(" ")
              reporter.echo(s"$s ${p.label}")
            }
          } else {
            reporter.echo(s"PROGRESS: $numCompleted / $numAllFutures")
            lastNumCompleted = numCompleted
          }
      }
    }

    def compile(p: Task) = {
      lazy val depsReady = Future.traverse(dependsOn(p))(dep => p.dependencyReadyFuture(dep))
      lazy val depsDone = Future.traverse(dependsOn(p))(dep => dep.t.javaDone.future)

      val f = strategy match {
        case OutlineTypePipeline => for {
          _ <- depsReady
          _ <- p.outlineCompile()
          _ <- p.fullCompile()
          _ <- depsDone
          _ <- p.javaCompile()
        } yield ()
        case Pipeline => for {
          _ <- depsReady
          isLeaf = !dependedOn.contains(p)
          useTraditional = isLeaf && useTraditionalForLeaf
          _ <- if (useTraditional) p.fullCompile() else p.fullCompileExportPickles()
          _ <- depsDone
          _ <- p.javaCompile()
        } yield ()
        case Traditional => for {
          _ <- depsDone
          _ <- p.fullCompile()
          _ <- p.javaCompile()
        } yield ()
      }
      f.onComplete(_ => p.close())
    }

    def reportAll(timer: Timer) = {
      for (p <- projects) {
        val dependencies = dependsOn(p).map(_.t)
        if (p.outlineTimer.durationMs > 0) {
          val maxOutlineCriticalPathMs = maxByOrZero(dependencies)(_.outlineCriticalPathMs)
          p.outlineCriticalPathMs = maxOutlineCriticalPathMs + p.outlineTimer.durationMs
          p.regularCriticalPathMs = maxOutlineCriticalPathMs + maxByOrZero(p.groups)(_.timer.durationMs)
        }
        p.fullCriticalPathMs = maxByOrZero(dependencies)(_.fullCriticalPathMs) + p.groups.map(_.timer.durationMs).sum
      }

      if (parallelism == 1) {
        val criticalPathMs = projects.map { p =>
          if (p.outlineTimer.durationMs > 0) p.regularCriticalPathMs else p.fullCriticalPathMs
        }.max
        reporter.echo(f"Critical path: $criticalPathMs%.0f ms. Wall Clock: ${timer.durationMs}%.0f ms")
      } else
        reporter.echo(f" Wall Clock: ${timer.durationMs}%.0f ms")
    }

    val timer = new Timer
    for (p <- projects) compile(p)
    awaitDone(timer)
    reportAll(timer)

    logDir.foreach { dir =>
      Files.createDirectories(dir)
      writeDotFile(dir, dependsOn)
      writeChromeTrace(dir, projects)
    }
    deleteTempPickleCache()
    !reporter.hasErrors
  }

  private def deleteTempPickleCache(): Unit = {
    if (configuredPickleCache.isEmpty) {
      AbstractFile.getDirectory(pickleCache.toFile).delete()
    }
  }

  private def writeChromeTrace(logDir: Path, projects: List[Task]) = {
    val trace = new java.lang.StringBuilder()
    trace.append("""{"traceEvents": [""")
    val sb = new mutable.StringBuilder(trace)

    def durationEvent(name: String, cat: String, t: Timer): String = {
      s"""{"name": "$name", "cat": "$cat", "ph": "X", "ts": ${(t.startMicros).toLong}, "dur": ${(t.durationMicros).toLong}, "pid": 0, "tid": ${t.thread.getId}}"""
    }

    def projectEvents(p: Task): List[String] = {
      val events = List.newBuilder[String]
      if (p.outlineTimer.durationMicros > 0d) {
        val desc = if (strategy == OutlineTypePipeline) "outline-type" else "parser-to-pickler"
        events += durationEvent(p.label, desc, p.outlineTimer)
      }
      for ((g, ix) <- p.groups.zipWithIndex) {
        if (g.timer.durationMicros > 0d)
          events += durationEvent(p.label, s"compile-$ix", g.timer)
      }
      if (p.javaTimer.durationMicros > 0d) {
        val desc = "javac"
        events += durationEvent(p.label, desc, p.javaTimer)
      }
      events.result()
    }

    projects.iterator.flatMap(projectEvents).addString(sb, ",\n")
    trace.append("]}")
    val traceFile = logDir.resolve(s"build-${label}.trace")
    Files.write(traceFile, trace.toString.getBytes())
    reporter.echo("Chrome trace written to " + traceFile.toAbsolutePath)
  }

  case class Group(prefix: String, files: List[String]) {
    val done = new TimedPromise(prefix)
    def timer = done.timer
  }

  private case class Task(argsFile: Path, command: CompilerCommand, files: List[String]) {
    val label = argsFile.toString.replaceAll(".*/target/", "").replaceAll("""(.*)/(.*).args""", "$1:$2")

    override def toString: String = argsFile.toString

    val outDir: File    = command.settings.outputDirs.getSingleOutput.get.file
    def outputDir: Path = outDir.toPath.toAbsolutePath.normalize()
    outDir.mkdirs()
    command.settings.classpath.prepend(outDir.getPath)

    private def expand(s: command.settings.PathSetting): List[Path] = {
      ClassPath.expandPath(s.value, expandStar = true).map(s => Paths.get(s).toAbsolutePath.normalize())
    }

    lazy val       classPath: List[Path] = expand(command.settings.classpath)
    lazy val  macroClassPath: List[Path] = expand(command.settings.YmacroClasspath)
    lazy val pluginClassPath: List[Path] = command.settings.plugin.value.flatMap(ClassPath.split(_)).map(Paths.get(_)).distinct

    def dependencyReadyFuture(dependency: Dependency) = if (dependency.isMacro) {
      log(s"dependency is on macro classpath, will wait for .class files: ${dependency.t.label}")
      dependency.t.javaDone.future
    } else if (dependency.isPlugin) {
      log(s"dependency is on plugin classpath, will wait for .class files: ${dependency.t.label}")
      dependency.t.javaDone.future
    } else
      dependency.t.outlineDone.future


    if (cacheMacro)
      command.settings.YcacheMacroClassLoader.value = "always"
    if (cachePlugin)
      command.settings.YcachePluginClassLoader.value = "always"

    if (strategy != Traditional) {
      command.settings.YpickleJava.value = true
    }

    val groupSize = Integer.getInteger("scala.pipeline.group.size", 128)
    val groups: List[Group] = {
      val isScalaLibrary = files.exists(_.endsWith("Predef.scala"))
      if (strategy != OutlineTypePipeline || isScalaLibrary) {
        Group("scalac", files) :: Nil
      } else {
        val numGroups = (files.length.toDouble / groupSize).toInt.max(1)
        val groupedFiles = files.grouped((files.length.toDouble / numGroups).ceil.toInt.max(1))
        groupedFiles.zipWithIndex.map { case (files, ix) =>
          val prefix = if (numGroups > 1) s"scalac (${ix + 1}/$numGroups)" else "scalac"
          Group(prefix, files)
        }.toList
      }
    }

    def outlineTimer = outlineDone.timer
    def javaTimer = javaDone.timer

    var outlineCriticalPathMs = 0d
    var regularCriticalPathMs = 0d
    var fullCriticalPathMs = 0d
    val outlineDone = new TimedPromise("scalac outline")
    val outlineDoneFuture: Future[Unit] = outlineDone.future
    val javaDone = new TimedPromise("javac")
    val javaDoneFuture: Future[Unit] = javaDone.future
    val groupsDoneFuture: Future[List[Unit]] = Future.traverse(groups)(_.done.future)
    val futures: List[Future[Unit]] = {
      outlineDoneFuture :: javaDoneFuture :: groups.map(_.done.future)
    }
    val stages: List[Future[Unit]] = {
      List(outlineDoneFuture, groupsDoneFuture.map(_ => ()), javaDoneFuture)
    }

    private[this] var initCompiler: () => Global = () => {
      val g = newCompiler(command.settings)
      val reporter = g.reporter
      if (reporter.hasErrors)
        reporter.flush()
      else if (command.shouldStopWithInfo)
        reporter.echo(command.getInfoMessage(g))
      g.reporter = createReporter(g.settings)
      g
    }

    lazy val compiler: Global = { val res = initCompiler(); initCompiler = null; res }

    def close() = if (initCompiler == null) compiler.close()

    def outlineCompile(): Future[Unit] = doCompile(outlineDone) {
      command.settings.Youtline.value = true
      command.settings.stopAfter.value = List("pickler")
      command.settings.Ymacroexpand.value = command.settings.MacroExpand.None
      command.settings.YpickleWrite.value = cachePath(outDir.toPath).toAbsolutePath.toString
      runCompile(compiler)(files)
    }

    def doCompile(done: TimedPromise, earlyTimed: Option[TimedPromise] = None)(thunk: => Boolean): Future[Unit] = {
      import done.{ prefix, timer }
      try {
        log(s"$prefix: start")
        earlyTimed.getOrElse(done).timer
        if (thunk) {
          val failure = Failure(new RuntimeException(s"$label: compile failed: "))
          earlyTimed.foreach(_.tryComplete(failure))
          done.complete(failure)
          log(f"$prefix: failed ${timer.durationMs}%.0f ms")
        } else {
          done.complete(Success(()))
          log(f"$prefix: done ${timer.durationMs}%.0f ms")
        }
      } catch {
        case t: Throwable =>
          val failure = Failure(new RuntimeException(s"$label: compile failed: ", t))
          earlyTimed.foreach(_.complete(failure))
          done.complete(failure)
      }
      done.future
    }

    def fullCompile(): Future[List[Unit]] = {
      outlineDone.tryComplete(Success(()))

      command.settings.Youtline.value = false
      command.settings.stopAfter.value = Nil
      command.settings.Ymacroexpand.value = command.settings.MacroExpand.Normal
      command.settings.YpickleWrite.value = ""

      Future.traverse(groups)(group => doCompile(group.done) {
        val g = newCompiler(command.settings)
        try runCompile(g)(group.files) finally g.close()
      })
    }

    def fullCompileExportPickles(): Future[List[Unit]] = {
      val List(group) = groups
      doCompile(group.done, Some(outlineDone)) {
        command.settings.YpickleWrite.value = cachePath(outDir.toPath).toString
        runCompile(compiler)(group.files, new compiler.Run() {
          override def advancePhase(): Unit = {
            if (compiler.phase == picklerPhase) {
              outlineDone.complete(Success(()))
              log(f"${outlineDone.prefix}: done ${outlineTimer.durationMs}%.0f ms")
              group.timer
            }
            super.advancePhase()
          }
        })
      }.map(_ :: Nil)
    }

    def javaCompile(): Future[Unit] = {
      val javaSources = files.filter(_.endsWith(".java"))
      if (javaSources.nonEmpty) {
        doCompile(javaDone) {
          val opts: java.util.List[String] = java.util.Arrays.asList("-d", command.settings.outdir.value, "-cp", command.settings.classpath.value)
          val javaCompiler = ToolProvider.getSystemJavaCompiler()
          //If the running JRE isn't from a JDK distribution, getSystemJavaCompiler returns null
          if (javaCompiler == null) throw new UnsupportedOperationException("no java compiler found in current Java runtime")
          val fileManager = javaCompiler.getStandardFileManager(null, null, null)
          val units = fileManager.getJavaFileObjects(javaSources: _*)
          val compileTask = javaCompiler.getTask(null, fileManager, javacDiagnosticListener, opts, null, units)
          compileTask.setProcessors(Collections.emptyList())
          !compileTask.call()
        }
      } else {
        javaDone.complete(Success(()))
      }
    }

    def log(msg: String): Unit = reporter.echo(s"$label: $msg")
  }

  final class Timer {
    private var stopped = false

    val startNanos: Long    = System.nanoTime()
    lazy val thread: Thread = Thread.currentThread()
    lazy val endNanos: Long = { stopped = true; thread; System.nanoTime() }

    def stop(): Unit = endNanos: Unit

    def wasStopped: Boolean    = stopped
    def startMicros: Double    = startNanos.toDouble / 1000
    def durationMicros: Double = (if (stopped) endNanos - startNanos else 0.0) / 1000
    def durationMs: Double     = durationMicros / 1000
  }

  final class TimedPromise(val prefix: String) {
    private val promise = Promise[Unit]()
    lazy val timer      = new Timer
    def future          = promise.future

    def complete(t: Try[Unit]): Future[Unit] = { timer.stop(); promise.complete(t).future  }
    def tryComplete(t: Try[Unit]): Unit      = { if (promise.tryComplete(t)) timer.stop()  }
  }

  protected def newCompiler(settings: Settings): Global = {
    if (strategy != Traditional) {
      val classPath = ClassPath.expandPath(settings.classpath.value, expandStar = true)
      val modifiedClassPath = classPath.map { entry =>
        val entryPath = Paths.get(entry)
        if (Files.exists(entryPath))
          strippedAndExportedClassPath.getOrElse(entryPath.toRealPath().normalize(), entryPath)
        else
          entryPath
      }
      settings.classpath.value = modifiedClassPath.mkString(File.pathSeparator)
    }
    Global(settings)
  }

  private def runCompile[G <: Global](g: G)(files: List[String], run: g.Run = new g.Run()) = {
    run.compile(files)
    g.reporter.finish()
    g.reporter.hasErrors
  }

  def javacDiagnosticListener = new DiagnosticListener[JavaFileObject] {
    def report(diagnostic: Diagnostic[_ <: JavaFileObject]) = {
      val msg = diagnostic.getMessage(Locale.getDefault)
      val position = if (diagnostic.getPosition == Diagnostic.NOPOS) NoPosition else {
        val sourceFile = new BatchSourceFile(new PlainNioFile(Paths.get(diagnostic.getSource.toUri)))
        Position.range(sourceFile, diagnostic.getStartPosition.toInt, diagnostic.getPosition.toInt, diagnostic.getEndPosition.toInt)
      }
      diagnostic.getKind match {
        case Kind.ERROR             => reporter.error(position, msg)
        case Kind.WARNING           => reporter.warning(position, msg)
        case Kind.MANDATORY_WARNING => reporter.warning(position, msg)
        case Kind.NOTE              => reporter.info(position, msg, force = true)
        case Kind.OTHER             => reporter.echo(position, msg)
      }
    }
  }

  private def maxByOrZero[A](as: List[A])(f: A => Double): Double = if (as.isEmpty) 0d else as.map(f).max
}

object PipelineMain {
  sealed abstract class BuildStrategy

  /** Outline type check sources to compute type signatures as input to downstream compilation. Compile sources (optionally). */
  case object OutlineTypePipeline extends BuildStrategy

  /** Transport pickles as an input to downstream compilation. */
  case object Pipeline extends BuildStrategy

  /** Emit class files before triggering downstream compilation */
  case object Traditional extends BuildStrategy

  case class PipelineSettings(label: String, parallelism: Int, strategy: BuildStrategy, useJars: Boolean,
                              configuredPickleCache: Option[Path], cacheMacro: Boolean, cachePlugin: Boolean,
                              stripExternalClassPath: Boolean, useTraditionalForLeaf: Boolean, logDir: Option[Path],
                              createReporter: (Settings => Reporter))

  private val strategies = List(OutlineTypePipeline, Pipeline, Traditional)

  def defaultSettings: PipelineSettings = PipelineSettings(
    label = "1",
    java.lang.Integer.getInteger("scala.pipeline.parallelism", parallel.availableProcessors),
    strategies.find(_.productPrefix.equalsIgnoreCase(System.getProperty("scala.pipeline.strategy", "pipeline"))).get,
    java.lang.Boolean.getBoolean("scala.pipeline.use.jar"),
    Option(System.getProperty("scala.pipeline.picklecache")).map(Paths.get(_)),
    java.lang.Boolean.getBoolean("scala.pipeline.cache.macro.classloader"),
    java.lang.Boolean.getBoolean("scala.pipeline.cache.plugin.classloader"),
    java.lang.Boolean.getBoolean("scala.pipeline.strip.external.classpath"),
    java.lang.Boolean.getBoolean("scala.pipeline.use.traditional.for.leaf"),
    logDir = Some(Paths.get(".")),
    createReporter = new ConsoleReporter(_)
  )

  def main(args: Array[String]): Unit = {
    val argFiles: Seq[Path] = args.map(Paths.get(_)) match {
      case Array(path) if Files.isDirectory(path) =>
        Files.walk(path).iterator().asScala.filter(_.toString.endsWith(".args")).toList
      case xs => xs
    }
    val main = new PipelineMainClass(argFiles, defaultSettings)
    val result = main.process()
    if (!result)
      System.exit(1)
    else
      System.exit(0)
  }
}

//object PipelineMainTest {
//  def main(args: Array[String]): Unit = {
//    var i = 0
////    val argsFiles = Files.walk(Paths.get("/code/guardian-frontend")).iterator().asScala.filter(_.getFileName.toString.endsWith(".args")).toList
//    val argsFiles = List(Paths.get("/Users/jz/code/guardian-frontend/common/target/compile.args"))
//    val useJars = java.lang.Boolean.getBoolean("scala.pipeline.use.jar")
//    for (_ <- 1 to 20; n <- List(parallel.availableProcessors); strat <- List(OutlineTypePipeline)) {
//      i += 1
//      val main = new PipelineMainClass(strat + "-" + i, n, strat, argsFiles, useJars)
//      println(s"====== ITERATION $i=======")
//      val result = main.process()
//      if (!result)
//        System.exit(1)
//    }
//    System.exit(0)
//  }
//}
