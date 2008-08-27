/* NEST (New Scala Test)
 * Copyright 2007-2008 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest.nest

import scala.tools.nsc.{Global, Settings, CompilerCommand}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}

import java.io.{File, BufferedReader, PrintWriter, FileWriter, StringWriter}

class ExtConsoleReporter(override val settings: Settings, reader: BufferedReader, var writer: PrintWriter) extends ConsoleReporter(settings, reader, writer) {
  def this(settings: Settings) = {
    this(settings, Console.in, new PrintWriter(new FileWriter("/dev/null")))
  }
  def hasWarnings: Boolean = WARNING.count != 0
}

abstract class SimpleCompiler {
  def compile(file: File, kind: String): Boolean
  def compile(out: Option[File], files: List[File], kind: String, log: File): Boolean
}

class DirectCompiler(val fileManager: FileManager) extends SimpleCompiler {
  def newGlobal(settings: Settings, reporter: Reporter): Global =
    new Global(settings, reporter)

  def newGlobal(settings: Settings, logWriter: FileWriter): Global = {
    val rep = new ExtConsoleReporter(new Settings(x => ()),
                                     Console.in,
                                     new PrintWriter(logWriter))
    rep.shortname = true
    newGlobal(settings, rep)
  }

  def newSettings = {
    val settings = new Settings(x => ())
    settings.deprecation.value = true
    settings.nowarnings.value = false
    settings.encoding.value = "iso-8859-1"
    settings
  }

  def newReporter(sett: Settings) = new ExtConsoleReporter(sett,
                                                           Console.in,
                                                           new PrintWriter(new StringWriter))

  def compile(out: Option[File], files: List[File], kind: String, log: File): Boolean = {
    val testSettings = newSettings
    val logWriter = new FileWriter(log)
    val args = List.fromArray(fileManager.SCALAC_OPTS.split("\\s"))
    val command = new CompilerCommand(args, testSettings, x => {}, false)
    val global = newGlobal(command.settings, logWriter)
    val testRep: ExtConsoleReporter = global.reporter.asInstanceOf[ExtConsoleReporter]

    val test: TestFile = kind match {
      case "pos"      => PosTestFile(files(0), fileManager, out.isEmpty)
      case "neg"      => NegTestFile(files(0), fileManager, out.isEmpty)
      case "run"      => RunTestFile(files(0), fileManager, out.isEmpty)
      case "jvm"      => JvmTestFile(files(0), fileManager, out.isEmpty)
      case "jvm5"     => Jvm5TestFile(files(0), fileManager, out.isEmpty)
      case "shootout" => ShootoutTestFile(files(0), fileManager, out.isEmpty)
    }
    test.defineSettings(testSettings)
    out match {
      case Some(outDir) =>
        testSettings.outdir.value = outDir.getAbsolutePath
        testSettings.classpath.value = testSettings.classpath.value+
          File.pathSeparator+outDir.getAbsolutePath
      case None =>
        // do nothing
    }

    val toCompile = files.map(_.getPath)
    try {
      NestUI.verbose("compiling "+toCompile)
      (new global.Run) compile toCompile
      testRep.printSummary
      testRep.writer.flush
      testRep.writer.close
    } catch {
      case e: Exception =>
        e.printStackTrace()
        return false
    } finally {
      logWriter.close()
    }
    !testRep.hasErrors
  }

  def compile(file: File, kind: String): Boolean = {
    val testSettings = newSettings
    val testRep = newReporter(testSettings)
    val args = List.fromArray(fileManager.SCALAC_OPTS.split("\\s"))
    val command = new CompilerCommand(args, testSettings, x => {}, false)
    val global = newGlobal(command.settings, testRep)

    val test: TestFile = kind match {
      case "pos"      => PosTestFile(file, fileManager, true)
      case "neg"      => NegTestFile(file, fileManager, true)
      case "run"      => RunTestFile(file, fileManager, true)
      case "jvm"      => JvmTestFile(file, fileManager, true)
      case "jvm5"     => Jvm5TestFile(file, fileManager, true)
      case "shootout" => ShootoutTestFile(file, fileManager, true)
    }
    test.defineSettings(testSettings)

    val toCompile = List(file.getPath)
    try {
      (new global.Run) compile toCompile
      testRep.printSummary
      testRep.writer.flush
      testRep.writer.close
    } catch {
      case e: Exception =>
        e.printStackTrace()
        return false
    }
    !testRep.hasErrors
  }
}

class ReflectiveCompiler(val fileManager: ConsoleFileManager) extends SimpleCompiler {
  import fileManager.{latestCompFile, latestPartestFile}

  val sepUrls = Array(latestCompFile.toURL, latestPartestFile.toURL)
  //NestUI.verbose("constructing URLClassLoader from URLs "+latestCompFile+" and "+latestPartestFile)

  val sepLoader = new java.net.URLClassLoader(sepUrls, null)

  val sepCompilerClass =
    sepLoader.loadClass("scala.tools.partest.nest.DirectCompiler")
  val sepCompiler = sepCompilerClass.newInstance()

  // needed for reflective invocation
  val fileClass = Class.forName("java.io.File")
  val stringClass = Class.forName("java.lang.String")
  val sepCompileMethod =
    sepCompilerClass.getMethod("compile", Array(fileClass, stringClass): _*)
  val sepCompileMethod2 =
    sepCompilerClass.getMethod("compile", Array(fileClass, stringClass, fileClass): _*)

  /* This method throws java.lang.reflect.InvocationTargetException
   * if the compiler crashes.
   * This exception is handled in the shouldCompile and shouldFailCompile
   * methods of class CompileManager.
   */
  def compile(file: File, kind: String): Boolean = {
    val fileArgs: Array[AnyRef] = Array(file, kind)
    val res = sepCompileMethod.invoke(sepCompiler, fileArgs: _*).asInstanceOf[java.lang.Boolean]
    res.booleanValue()
  }

  /* This method throws java.lang.reflect.InvocationTargetException
   * if the compiler crashes.
   * This exception is handled in the shouldCompile and shouldFailCompile
   * methods of class CompileManager.
   */
  def compile(out: Option[File], files: List[File], kind: String, log: File): Boolean = {
    val fileArgs: Array[AnyRef] = Array(out, files, kind, log)
    val res = sepCompileMethod2.invoke(sepCompiler, fileArgs: _*).asInstanceOf[java.lang.Boolean]
    res.booleanValue()
  }
}

class CompileManager(val fileManager: FileManager) {

  import scala.actors.Actor._
  import scala.actors.{Actor, Exit, TIMEOUT}

  var compiler: SimpleCompiler = new /*ReflectiveCompiler*/ DirectCompiler(fileManager)

  var numSeparateCompilers = 1
  def createSeparateCompiler() = {
    numSeparateCompilers += 1
    compiler = new /*ReflectiveCompiler*/ DirectCompiler(fileManager)
  }

  val delay = fileManager.timeout.toLong

  def withTimeout(files: List[File])(thunk: => Boolean): Boolean = {
    createSeparateCompiler()

    val parent = self
    self.trapExit = true
    val child = link {
      parent ! (self, thunk)
    }

    receiveWithin(delay) {
      case TIMEOUT =>
        println("compilation timed out")
        false
      case Exit(from, reason) if from == child =>
        val From = from
        reason match {
          case 'normal =>
            receive {
              case (From, result: Boolean) => result
            }
          case t: Throwable =>
            NestUI.verbose("while invoking compiler ("+files+"):")
            NestUI.verbose("caught "+t)
            t.printStackTrace
            if (t.getCause != null)
              t.getCause.printStackTrace
            false
        }
    }
  }

  /* This method returns true iff compilation succeeds.
   */
  def shouldCompile(files: List[File], kind: String, log: File): Boolean =
    withTimeout(files) {
      compiler.compile(None, files, kind, log)
    }

  /* This method returns true iff compilation succeeds.
   */
  def shouldCompile(out: File, files: List[File], kind: String, log: File): Boolean =
    withTimeout(files) {
      compiler.compile(Some(out), files, kind, log)
    }

  /* This method returns true iff compilation fails
   * _and_ the compiler does _not_ crash or loop.
   *
   * If the compiler crashes, this method returns false.
   */
  def shouldFailCompile(files: List[File], kind: String, log: File): Boolean =
    withTimeout(files) {
      !compiler.compile(None, files, kind, log)
    }
}
