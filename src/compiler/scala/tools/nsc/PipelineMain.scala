/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc

import java.io.File
import java.lang.Thread.UncaughtExceptionHandler
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.ConcurrentHashMap
import java.util.{Collections, Locale}

import javax.tools.Diagnostic.Kind
import javax.tools.{Diagnostic, DiagnosticListener, JavaFileObject, ToolProvider}

import scala.annotation.nowarn
import scala.collection.{immutable, mutable}
import scala.collection.immutable.ArraySeq.unsafeWrapArray
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters._
import scala.math.Ordering.Double.TotalOrdering
import scala.reflect.internal.util.{BatchSourceFile, FakePos, NoPosition, Position}
import scala.reflect.io.PlainNioFile
import scala.tools.nsc.PipelineMain.{OutlineTypePipeline, Pipeline, Traditional}
import scala.tools.nsc.Reporting.WarningCategory
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.{ConsoleReporter, Reporter}
import scala.tools.nsc.util.ClassPath
import scala.util.{Failure, Success}

class PipelineMainClass(argFiles: Seq[Path], pipelineSettings: PipelineMain.PipelineSettings) {
  import pipelineSettings._
  private val pickleCache: Path = configuredPickleCache.getOrElse(Files.createTempDirectory("scala.picklecache"))
  private def cachePath(file: Path): Path = {
    val newExtension = if (useJars) ".jar" else ""
    val root = file.getRoot
    // An empty component on Unix, just the drive letter on Windows
    val validRootPathComponent = root.toString.replace("/", "").replace(":", "")
    val result = changeExtension(pickleCache.resolve(validRootPathComponent).resolve(root.relativize(file)).normalize(), newExtension)
    if (useJars) Files.createDirectories(result.getParent)
    strippedAndExportedClassPath.update(file.toRealPath().normalize(), result)
    result
  }

  private val strippedAndExportedClassPath = new ConcurrentHashMap[Path, Path]().asScala

  /** Forward errors to the (current) reporter. */
  protected def scalacError(msg: String): Unit = {
    reporterError(FakePos("scalac"), msg + "\n  scalac -help  gives more information")
  }

  private var reporter: Reporter = _
  private def reporterEcho(pos: Position, msg: String): Unit = synchronized {
    reporter.echo(pos, msg)
  }
  private def reporterEcho(msg: String): Unit = synchronized {
    reporter.echo(NoPosition, msg)
  }
  private def reporterError(pos: Position, msg: String): Unit = synchronized {
    reporter.error(pos, msg)
  }

  private object handler extends UncaughtExceptionHandler {
    override def uncaughtException(t: Thread, e: Throwable): Unit = {
      e.printStackTrace()
      System.exit(-1)
    }
  }

  implicit val executor: ExecutionContext = ExecutionContext.fromExecutor(new java.util.concurrent.ForkJoinPool(parallelism), t => handler.uncaughtException(Thread.currentThread(), t))
  def changeExtension(p: Path, newExtension: String): Path = {
    val fileName = p.getFileName.toString
    val changedFileName = fileName.lastIndexOf('.') match {
      case -1 => fileName + newExtension
      case n => fileName.substring(0, n) + newExtension
    }
    p.getParent.resolve(changedFileName)
  }

  @nowarn("cat=lint-inaccessible")
  def writeDotFile(logDir: Path, dependsOn: mutable.LinkedHashMap[Task, List[Dependency]]): Unit = {
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
    Files.write(path, builder.toString.getBytes(UTF_8))
    reporterEcho("Wrote project dependency graph to: " + path.toAbsolutePath)
  }

  private case class Dependency(t: Task, isMacro: Boolean, isPlugin: Boolean)

  def process(): Boolean = {
    reporter = createReporter(new Settings(scalacError))
    reporterEcho(s"parallelism = $parallelism, strategy = $strategy")

    def commandFor(argFileArg: Path): Task = {
      val ss = new Settings(scalacError)
      val command = new CompilerCommand(("@" + argFileArg) :: Nil, ss)
      Task(argFileArg, command, command.files)
    }

    val projects: List[Task] = argFiles.toList.map(commandFor)
    if (reporter.hasErrors) return false

    val produces = mutable.LinkedHashMap[Path, Task]()
    for (p <- projects) {
      produces(p.outputDir) = p
    }
    val dependsOn = mutable.LinkedHashMap[Task, List[Dependency]]()
    for (p <- projects) {
      val macroDeps = p.macroClassPath.flatMap(p => produces.get(p)).toList.filterNot(_ == p).map(t => Dependency(t, isMacro = true, isPlugin = false))
      val pluginDeps = p.pluginClassPath.flatMap(p => produces.get(p)).toList.filterNot(_ == p).map(t => Dependency(t, isMacro = false, isPlugin = true))
      val classPathDeps = p.classPath.flatMap(p => produces.get(p)).toList.filterNot(_ == p).filterNot(p => macroDeps.exists(_.t == p)).map(t => Dependency(t, isMacro = false, isPlugin = false))
      dependsOn(p) = classPathDeps ++ macroDeps ++ pluginDeps
    }
    val dependedOn: Set[Task] = dependsOn.valuesIterator.flatten.map(_.t).toSet
    val externalClassPath = projects.iterator.flatMap(_.classPath).filter(p => !produces.contains(p) && Files.exists(p)).toSet

    if (strategy != Traditional) {
      if (stripExternalClassPath) {
        val exportTimer = new Timer
        exportTimer.start()
        for (entry <- externalClassPath) {
          val extracted = cachePath(entry)
          val sourceTimeStamp = Files.getLastModifiedTime(entry)
          if (Files.exists(extracted) && Files.getLastModifiedTime(extracted) == sourceTimeStamp) {
            // println(s"Skipped export of pickles from $entry to $extracted (up to date)")
          } else {
            PickleExtractor.process(entry, extracted)
            Files.setLastModifiedTime(extracted, sourceTimeStamp)
            reporterEcho(s"Exported pickles from $entry to $extracted")
            Files.setLastModifiedTime(extracted, sourceTimeStamp)
          }
          strippedAndExportedClassPath(entry) = extracted
        }
        exportTimer.stop()
        reporterEcho(f"Exported external classpath in ${exportTimer.durationMs}%.0f ms")
      }
    }

    val timer = new Timer
    timer.start()

    def sequenceFailSlow[A](fs: Seq[Future[A]]): Future[Seq[A]] = {
      Future.traverse(fs)(_.transform(tr => Success(tr.toEither))).map { results =>
        val (failures, successes) = results.partitionMap(identity)
        failures.toList match {
          case head :: rest => rest.foreach(head.addSuppressed(_)); throw head
          case _            => successes
        }
      }
    }

    def awaitDone(): Unit = {
      val allFutures: immutable.Seq[Future[_]] = projects.flatMap(_.futures)
      val numAllFutures = allFutures.size
      val awaitAllFutures: Future[_] = sequenceFailSlow(allFutures)
      var lastNumCompleted = allFutures.count(_.isCompleted)
      while (true) try {
        Await.ready(awaitAllFutures, Duration(60, "s"))
        timer.stop()
        val numCompleted = allFutures.count(_.isCompleted)
        reporterEcho(s"PROGRESS: $numCompleted / $numAllFutures")
        return
      } catch {
        case _: TimeoutException =>
          val numCompleted = allFutures.count(_.isCompleted)
          if (numCompleted == lastNumCompleted) {
            reporterEcho(s"STALLED: $numCompleted / $numAllFutures")
            reporterEcho("Outline/Scala/Javac")
            projects.map {
              p =>
                def toX(b: Future[_]): String = b.value match { case None => "-"; case Some(Success(_)) => "x"; case Some(Failure(_)) => "!" }
                val s = List(p.outlineDoneFuture, p.groupsDoneFuture, p.javaDoneFuture).map(toX).mkString(" ")
                reporter.echo(s + " " + p.label)
            }
          } else {
            reporterEcho(s"PROGRESS: $numCompleted / $numAllFutures")
            lastNumCompleted = numCompleted
          }
      }
    }
    strategy match {
      case OutlineTypePipeline =>
        projects.foreach { p: Task =>
          val depsReady = Future.traverse(dependsOn.getOrElse(p, Nil))(task => p.dependencyReadyFuture(task))
          val f = for {
            _ <- depsReady
            _ <- {
              p.outlineCompile()
              p.outlineDone.future
            }
            _ <- {
              p.fullCompile()
              Future.traverse(p.groups)(_.done.future)
            }
            _ <- Future.traverse(dependsOn.getOrElse(p, Nil))(task => task.t.javaDone.future)
          } yield {
            p.javaCompile()
          }
          f.onComplete { _ => p.close() }
        }

        awaitDone()

        for (p <- projects) {
          val dependencies = dependsOn(p).map(_.t)

          def maxByOrZero[A](as: List[A])(f: A => Double): Double = if (as.isEmpty) 0d else as.map(f).max

          val maxOutlineCriticalPathMs = maxByOrZero(dependencies)(_.outlineCriticalPathMs)
          p.outlineCriticalPathMs = maxOutlineCriticalPathMs + p.outlineTimer.durationMs
          p.regularCriticalPathMs = maxOutlineCriticalPathMs + maxByOrZero(p.groups)(_.timer.durationMs)
          p.fullCriticalPathMs = maxByOrZero(dependencies)(_.fullCriticalPathMs) + p.groups.map(_.timer.durationMs).sum
        }

        if (parallelism == 1) {
          val criticalPath = projects.maxBy(_.regularCriticalPathMs)
          reporterEcho(f"Critical path: ${criticalPath.regularCriticalPathMs}%.0f ms. Wall Clock: ${timer.durationMs}%.0f ms")
        } else
          reporterEcho(f" Wall Clock: ${timer.durationMs}%.0f ms")
      case Pipeline =>
        projects.foreach { p =>
          val depsReady = Future.traverse(dependsOn.getOrElse(p, Nil))(task => p.dependencyReadyFuture(task))
          val f = for {
            _ <- depsReady
            _ <- {
              val isLeaf = !dependedOn.contains(p)
              if (isLeaf && useTraditionalForLeaf) {
                p.outlineDone.complete(Success(()))
                p.fullCompile()
              } else
                p.fullCompileExportPickles()
              // Start javac after scalac has completely finished
              Future.traverse(p.groups)(_.done.future)
            }
            _ <- Future.traverse(dependsOn.getOrElse(p, Nil))(task => task.t.javaDone.future)
          } yield {
            p.javaCompile()
          }
          f.onComplete { _ => p.close() }
        }
        awaitDone()

        for (p <- projects) {
          val dependencies = dependsOn(p).map(_.t)

          def maxByOrZero[A](as: List[A])(f: A => Double): Double = if (as.isEmpty) 0d else as.map(f).max

          val maxOutlineCriticalPathMs = maxByOrZero(dependencies)(_.outlineCriticalPathMs)
          p.outlineCriticalPathMs = maxOutlineCriticalPathMs + p.outlineTimer.durationMs
          p.regularCriticalPathMs = maxOutlineCriticalPathMs + maxByOrZero(p.groups)(_.timer.durationMs)
          p.fullCriticalPathMs = maxByOrZero(dependencies)(_.fullCriticalPathMs) + p.groups.map(_.timer.durationMs).sum
        }

        if (parallelism == 1) {
          val criticalPath = projects.maxBy(_.regularCriticalPathMs)
          reporterEcho(f"Critical path: ${criticalPath.regularCriticalPathMs}%.0f ms. Wall Clock: ${timer.durationMs}%.0f ms")
        } else
          reporterEcho(f" Wall Clock: ${timer.durationMs}%.0f ms")
      case Traditional =>
        projects.foreach { p =>
          val f1 = Future.traverse(dependsOn.getOrElse(p, Nil))(_.t.javaDone.future)
          val f2 = f1.flatMap { _ =>
            p.outlineDone.complete(Success(()))
            p.fullCompile()
            val eventualUnits: Future[List[Unit]] = Future.traverse(p.groups)(_.done.future)
            eventualUnits.map(_ => p.javaCompile())
          }
          f2.onComplete { _ => p.close() }
        }
        awaitDone()

        for (p <- projects) {
          val dependencies = dependsOn(p).map(_.t)

          def maxByOrZero[A](as: List[A])(f: A => Double): Double = if (as.isEmpty) 0d else as.map(f).max

          p.fullCriticalPathMs = maxByOrZero(dependencies)(_.fullCriticalPathMs) + p.groups.map(_.timer.durationMs).sum
        }
        if (parallelism == 1) {
          val maxFullCriticalPath: Double = projects.map(_.fullCriticalPathMs).max
          reporterEcho(f"Critical path: $maxFullCriticalPath%.0f ms. Wall Clock: ${timer.durationMs}%.0f ms")
        } else {
          reporterEcho(f"Wall Clock: ${timer.durationMs}%.0f ms")
        }
    }

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

    @annotation.nowarn("cat=deprecation")
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
          events += durationEvent(p.label, "compile-" + ix, g.timer)
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
    Files.write(traceFile, trace.toString.getBytes(UTF_8))
    reporterEcho("Chrome trace written to " + traceFile.toAbsolutePath)
  }

  case class Group(files: List[String]) {
    val timer = new Timer
    val done = Promise[Unit]()
  }

  private case class Task(argsFile: Path, command: CompilerCommand, files: List[String]) {
    val label = argsFile.toString.replaceAll(".*/target/", "").replaceAll("""(.*)/(.*).args""", "$1:$2")
    override def toString: String = argsFile.toString
    def outputDir: Path = command.settings.outputDirs.getSingleOutput.get.file.toPath.toAbsolutePath.normalize()
    private def expand(s: command.settings.PathSetting): List[Path] = {
      ClassPath.expandPath(s.value, expandStar = true).map(s => Paths.get(s).toAbsolutePath.normalize())
    }
    lazy val classPath: Seq[Path] = expand(command.settings.classpath)
    lazy val macroClassPath: Seq[Path] = expand(command.settings.YmacroClasspath)
    lazy val macroClassPathSet: Set[Path] = macroClassPath.toSet
    lazy val pluginClassPath: Set[Path] = {
      def asPath(p: String) = ClassPath split p

      val paths = command.settings.plugin.value filter (_ != "") flatMap (s => asPath(s) map (s => Paths.get(s)))
      paths.toSet
    }
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
        Group(files) :: Nil
      } else {
        command.settings.classpath.value = command.settings.outputDirs.getSingleOutput.get.toString + File.pathSeparator + command.settings.classpath.value
        val length = files.length
        val groups = (length.toDouble / groupSize).toInt.max(1)
        files.grouped((length.toDouble / groups).ceil.toInt.max(1)).toList.map(Group(_))
      }
    }
    command.settings.outputDirs.getSingleOutput.get.file.mkdirs()

    val isGrouped = groups.size > 1

    val outlineTimer = new Timer()
    val javaTimer = new Timer()

    var outlineCriticalPathMs = 0d
    var regularCriticalPathMs = 0d
    var fullCriticalPathMs = 0d
    val outlineDone: Promise[Unit] = Promise[Unit]()
    val outlineDoneFuture = outlineDone.future
    val javaDone: Promise[Unit] = Promise[Unit]()
    val javaDoneFuture: Future[_] = javaDone.future
    val groupsDoneFuture: Future[List[Unit]] = Future.traverse(groups)(_.done.future)
    val futures: List[Future[_]] = {
      outlineDone.future :: javaDone.future :: groups.map(_.done.future)
    }

    val originalClassPath: String = command.settings.classpath.value

    private[this] var initCompiler: () => Global = () => try {
      val result = newCompiler(command.settings)
      val reporter = result.reporter
      if (reporter.hasErrors)
        reporter.flush()
      else if (command.shouldStopWithInfo)
        reporterEcho(command.getInfoMessage(result))
      result.reporter = createReporter(result.settings)
      result
    } catch {
      case t: Throwable =>
        t.printStackTrace()
        throw t
    }

    lazy val compiler: Global = { val res = initCompiler(); initCompiler = null; res }

    def close() = if (initCompiler == null) compiler.close()

    def outlineCompile(): Unit = {
      outlineTimer.start()
      try {
        log("scalac outline: start")
        command.settings.Youtline.value = true
        command.settings.stopAfter.value = List("pickler")
        command.settings.Ymacroexpand.value = command.settings.MacroExpand.None
        command.settings.YpickleWrite.value = cachePath(command.settings.outputDirs.getSingleOutput.get.file.toPath).toAbsolutePath.toString
        val run1 = new compiler.Run()
        run1 compile files
        outlineTimer.stop()
        log(f"scalac outline: done ${outlineTimer.durationMs}%.0f ms")
        compiler.reporter.finish()
        if (compiler.reporter.hasErrors) {
          log("scalac outline: failed")
          outlineDone.complete(Failure(new RuntimeException(label + ": compile failed: ")))
        } else {
          log(f"scalac outline: done ${outlineTimer.durationMs}%.0f ms")
          outlineDone.complete(Success(()))
        }
      } catch {
        case t: Throwable =>
          outlineDone.complete(Failure(new RuntimeException(label + ": compile failed: ", t)))
      }
    }

    def fullCompile(): Unit = {
      command.settings.Youtline.value = false
      command.settings.stopAfter.value = Nil
      command.settings.Ymacroexpand.value = command.settings.MacroExpand.Normal
      command.settings.YpickleWrite.value = ""

      val groupCount = groups.size
      for ((group, ix) <- groups.zipWithIndex) {
        group.done.completeWith {
          Future {
            log(s"scalac (${ix + 1}/$groupCount): start")
            group.timer.start()
            val compiler2 = newCompiler(command.settings)
            try {
              try {
                val run2 = new compiler2.Run()
                run2 compile group.files
                compiler2.reporter.finish()
              } finally {
                group.timer.stop()
                log(f"scalac (${ix + 1}/$groupCount): done ${group.timer.durationMs}%.0f ms")
              }
              if (compiler2.reporter.hasErrors) {
                throw new RuntimeException(label + ": compile failed: ")
              }
            } finally {
              compiler2.close()
            }
          }
        }
      }
    }

    def fullCompileExportPickles(): Unit = {
      assert(groups.size == 1)
      val group = groups.head
      log("scalac: start")
      command.settings.YpickleWrite.value = cachePath(command.settings.outputDirs.getSingleOutput.get.file.toPath).toString
      outlineTimer.start()
      try {
        val run2 = new compiler.Run() {
          override def advancePhase(): Unit = {
            if (compiler.phase == this.picklerPhase) {
              outlineTimer.stop()
              log(f"scalac outline: done ${outlineTimer.durationMs}%.0f ms")
              outlineDone.complete(Success(()))
              group.timer.start()
            }
            super.advancePhase()
          }
        }

        run2 compile group.files
        compiler.reporter.finish()
        group.timer.stop()
        if (compiler.reporter.hasErrors) {
          log("scalac: failed")
          if (!outlineDone.isCompleted)
            outlineDone.complete(Failure(new RuntimeException(label + ": compile failed: ")))
          group.done.complete(Failure(new RuntimeException(label + ": compile failed: ")))
        } else {
          log(f"scalac: done ${group.timer.durationMs}%.0f ms")
          group.done.complete(Success(()))
        }
      } catch {
        case t: Throwable =>
          t.printStackTrace()
          if (!outlineDone.isCompleted)
            outlineDone.complete(Failure(new RuntimeException(label + ": compile failed: ")))
          if (!group.done.isCompleted)
            group.done.complete(Failure(new RuntimeException(label + ": compile failed: ")))
      }
    }

    def javaCompile(): Unit = {
      val javaSources = files.filter(_.endsWith(".java"))
      if (javaSources.nonEmpty) {
        log("javac: start")
        javaTimer.start()
        javaDone.completeWith(Future {
          val opts: java.util.List[String] = java.util.Arrays.asList("-d", command.settings.outdir.value, "-cp", command.settings.outdir.value + File.pathSeparator + originalClassPath)
          val javaCompiler = ToolProvider.getSystemJavaCompiler()
          //If the running JRE isn't from a JDK distribution, getSystemJavaCompiler returns null
          if (javaCompiler == null) throw new UnsupportedOperationException("no java compiler found in current Java runtime")
          val listener = new DiagnosticListener[JavaFileObject] {
            override def report(diagnostic: Diagnostic[_ <: JavaFileObject]): Unit = {
              val msg = diagnostic.getMessage(Locale.getDefault)
              val source: JavaFileObject = diagnostic.getSource
              val path = Paths.get(source.toUri)
              val position = if (diagnostic.getPosition == Diagnostic.NOPOS) NoPosition else {
                val sourceFile = new BatchSourceFile(new PlainNioFile(path))
                Position.range(sourceFile, diagnostic.getStartPosition.toInt, diagnostic.getPosition.toInt, diagnostic.getEndPosition.toInt)
              }
              diagnostic.getKind match {
                case Kind.ERROR                            => reporterError(position, msg)
                case Kind.WARNING | Kind.MANDATORY_WARNING => Task.this.compiler.runReporting.warning(position, msg, WarningCategory.JavaSource, site = "")
                case Kind.NOTE | Kind.OTHER                => reporterEcho(position, msg)
              }
            }
          }

          val fileManager = javaCompiler.getStandardFileManager(null, null, null)
          val compileTask = javaCompiler.getTask(null, fileManager, listener, opts, null, fileManager.getJavaFileObjects(javaSources.toArray: _*))
          compileTask.setProcessors(Collections.emptyList())
          val success = compileTask.call()
          javaTimer.stop()
          if (success)
            log(f"javac: done ${javaTimer.durationMs}%.0f ms ")
          else
            throw new RuntimeException(f"javac: error ${javaTimer.durationMs}%.0f ms ")
        })
      } else {
        javaDone.complete(Success(()))
      }
    }
    def log(msg: String): Unit = reporterEcho(this.label + ": " + msg)
  }

  final class Timer() {
    private var startNanos: Long = 0
    private var endNanos: Long = 0
    def start(): Unit = {
      assert(startNanos == 0L)
      startNanos = System.nanoTime
    }
    var thread: Thread = Thread.currentThread()
    def stop(): Unit = {
      thread = Thread.currentThread()
      endNanos = System.nanoTime()
    }
    def startMs: Double = startNanos.toDouble / 1000 / 1000
    def durationMs: Double = {
      val result = (endNanos - startNanos).toDouble / 1000 / 1000
      if (result < 0)
        getClass
      result
    }
    def startMicros: Double = startNanos.toDouble / 1000d
    def durationMicros: Double = (endNanos - startNanos).toDouble / 1000d
  }

  protected def newCompiler(settings: Settings): Global = {
    if (strategy != Traditional) {
      val classPath = ClassPath.expandPath(settings.classpath.value, expandStar = true)
      val modifiedClassPath = classPath.map { entry =>
        val entryPath = Paths.get(entry)
        if (Files.exists(entryPath))
          strippedAndExportedClassPath.getOrElse(entryPath.toRealPath().normalize(), entryPath).toString
        else
          entryPath
      }
      settings.classpath.value = modifiedClassPath.mkString(File.pathSeparator)
    }
    Global(settings)
  }
}


object PipelineMain {
  sealed abstract class BuildStrategy

  /** Outline type check sources to compute type signatures an input to downstream compilation. Compile sources (optionally */
  case object OutlineTypePipeline extends BuildStrategy
  /** Transport pickles as an input to downstream compilation. */
  case object Pipeline extends BuildStrategy

  /** Emit class files before triggering downstream compilation */
  case object Traditional extends BuildStrategy

  case class PipelineSettings(label: String, parallelism: Int, strategy: BuildStrategy, useJars: Boolean,
                              configuredPickleCache: Option[Path], cacheMacro: Boolean, cachePlugin: Boolean,
                              stripExternalClassPath: Boolean, useTraditionalForLeaf: Boolean, logDir: Option[Path],
                              createReporter: (Settings => Reporter))
  def defaultSettings: PipelineSettings = {
    val strategies = List(OutlineTypePipeline, Pipeline, Traditional)
    val strategy = strategies.find(_.productPrefix.equalsIgnoreCase(System.getProperty("scala.pipeline.strategy", "pipeline"))).get
    val parallelism = java.lang.Integer.getInteger("scala.pipeline.parallelism", java.lang.Runtime.getRuntime.availableProcessors())
    val useJars = java.lang.Boolean.getBoolean("scala.pipeline.use.jar")
    val cacheMacro = java.lang.Boolean.getBoolean("scala.pipeline.cache.macro.classloader")
    val cachePlugin = java.lang.Boolean.getBoolean("scala.pipeline.cache.plugin.classloader")
    val stripExternalClassPath = java.lang.Boolean.getBoolean("scala.pipeline.strip.external.classpath")
    val useTraditionalForLeaf = java.lang.Boolean.getBoolean("scala.pipeline.use.traditional.for.leaf")
    val configuredPickleCache = Option(System.getProperty("scala.pipeline.picklecache")).map(Paths.get(_))
    val logDir = Paths.get(".")
    new PipelineSettings("1", parallelism, strategy, useJars, configuredPickleCache,
      cacheMacro, cachePlugin, stripExternalClassPath, useTraditionalForLeaf, Some(logDir), new ConsoleReporter(_))
  }

  def main(args: Array[String]): Unit = {
    val argFiles: Seq[Path] = args match {
      case Array(path) if Files.isDirectory(Paths.get(path)) =>
        Files.walk(Paths.get(path)).iterator().asScala.filter(_.getFileName.toString.endsWith(".args")).toList
      case _ =>
        unsafeWrapArray(args.map(Paths.get(_)))
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
//    for (_ <- 1 to 20; n <- List(parallel.availableProcessors); start <- List(OutlineTypePipeline)) {
//      i += 1
//      val main = new PipelineMainClass(start + "-" + i, n, start, argsFiles, useJars)
//      println(s"====== ITERATION $i=======")
//      val result = main.process()
//      if (!result)
//        System.exit(1)
//    }
//    System.exit(0)
//  }
//}
