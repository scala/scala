/* NEST (New Scala Test)
 * @author Philipp Haller
 */

package scala.tools.partest.nest

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}

import java.io.{File, BufferedReader, PrintWriter, FileWriter}
import java.net.URLClassLoader

class ExtConsoleReporter(override val settings: Settings, reader: BufferedReader, var writer: PrintWriter) extends ConsoleReporter(settings, reader, writer) {
  def this(settings: Settings) = {
    this(settings, Console.in, new PrintWriter(new FileWriter("/dev/null")))
  }
  def hasWarnings: Boolean = WARNING.count != 0
}

abstract class SimpleCompiler {
  def compile(file: File, kind: String): Boolean
  def compile(file: File, kind: String, log: File): Boolean
}

class DirectCompiler extends SimpleCompiler {
  def newGlobal(settings: Settings, reporter: Reporter): Global =
    new Global(settings, reporter)

  def newGlobal(settings: Settings, log: File): Global = {
    val rep = new ExtConsoleReporter(new Settings(x => ()),
                                     Console.in,
                                     new PrintWriter(new FileWriter(log)))
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
                                                           new PrintWriter(new FileWriter("scalac-out")))

  def compile(file: File, kind: String, log: File): Boolean = {
    val testSettings = newSettings
    val global = newGlobal(testSettings, log)
    val testRep: ExtConsoleReporter = global.reporter.asInstanceOf[ExtConsoleReporter]

    val test: TestFile = kind match {
      case "pos"      => PosTestFile(file)
      case "neg"      => NegTestFile(file)
      case "run"      => RunTestFile(file)
      case "jvm"      => JvmTestFile(file)
      case "jvm5"     => Jvm5TestFile(file)
      case "shootout" => ShootoutTestFile(file)
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
        false
    }
    !testRep.hasErrors
  }

  def compile(file: File, kind: String): Boolean = {
    val testSettings = newSettings
    val testRep = newReporter(testSettings)
    val global = newGlobal(testSettings, testRep)

    val test: TestFile = kind match {
      case "pos"      => PosTestFile(file)
      case "neg"      => NegTestFile(file)
      case "run"      => RunTestFile(file)
      case "jvm"      => JvmTestFile(file)
      case "jvm5"     => Jvm5TestFile(file)
      case "shootout" => ShootoutTestFile(file)
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
        false
    }
    !testRep.hasErrors
  }
}

class ReflectiveCompiler extends SimpleCompiler {
  import FileManager.{latestCompFile, latestPartestFile}

  val sepUrls = Array(latestCompFile.toURL, latestPartestFile.toURL)
  val sepLoader = new URLClassLoader(sepUrls)

  val sepCompilerClass =
    sepLoader.loadClass("scala.tools.partest.nest.DirectCompiler")
  val sepCompiler = sepCompilerClass.newInstance()

  // needed for reflective invocation
  val fileClass = Class.forName("java.io.File")
  val stringClass = Class.forName("java.lang.String")
  val sepCompileMethod =
    sepCompilerClass.getMethod("compile", Array(fileClass, stringClass))
  val sepCompileMethod2 =
    sepCompilerClass.getMethod("compile", Array(fileClass, stringClass, fileClass))

  def compile(file: File, kind: String): Boolean = {
    val fileArgs: Array[AnyRef] = Array(file, kind)
    val res = sepCompileMethod.invoke(sepCompiler, fileArgs).asInstanceOf[java.lang.Boolean]
    res.booleanValue()
  }

  def compile(file: File, kind: String, log: File): Boolean = {
    val fileArgs: Array[AnyRef] = Array(file, kind, log)
    val res = sepCompileMethod2.invoke(sepCompiler, fileArgs).asInstanceOf[java.lang.Boolean]
    res.booleanValue()
  }
}

class CompileManager {
  var compiler: SimpleCompiler = new ReflectiveCompiler

  var numSeparateCompilers = 1
  def createSeparateCompiler() = {
    numSeparateCompilers += 1
    compiler = new ReflectiveCompiler
  }

  def shouldCompile(file: File, kind: String): Boolean = {
    createSeparateCompiler()
    compiler.compile(file, kind)

    /*compiler.compile(file, kind) || {
      NestUI.verbose("creating new separate compiler")
      createSeparateCompiler()
      compiler.compile(file, kind)
    }*/
  }

  def shouldFailCompile(file: File, kind: String, log: File): Boolean = {
    // always create new separate compiler
    createSeparateCompiler()
    !compiler.compile(file, kind, log)
  }
}
