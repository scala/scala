/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import scala.tools.nsc.{ Global, Settings, CompilerCommand, FatalError, io }
import scala.tools.nsc.reporters.{ Reporter, ConsoleReporter }
import scala.tools.nsc.util.ClassPath
import scala.tools.util.PathResolver
import io.Path

import java.io.{ File, BufferedReader, PrintWriter, FileReader, Writer, FileWriter, StringWriter }
import File.pathSeparator

class ExtConsoleReporter(override val settings: Settings, reader: BufferedReader, var writer: PrintWriter)
extends ConsoleReporter(settings, reader, writer) {
  def this(settings: Settings) = this(settings, Console.in, new PrintWriter(new FileWriter("/dev/null")))
}

abstract class SimpleCompiler {
  def compile(out: Option[File], files: List[File], kind: String, log: File): Boolean
}

class DirectCompiler(val fileManager: FileManager) extends SimpleCompiler {
  def newGlobal(settings: Settings, reporter: Reporter): Global =
    new Global(settings, reporter)

  def newGlobal(settings: Settings, logWriter: FileWriter): Global = {
    val rep = newReporter(settings, logWriter)
    rep.shortname = true
    newGlobal(settings, rep)
  }

  def newSettings(out: Option[String]) = {
    val settings = new Settings(_ => ())
    settings.deprecation.value = true
    settings.nowarnings.value = false
    settings.encoding.value = "ISO-8859-1"    // XXX why?

    val classpathElements = fileManager.LATEST_LIB :: out.toList
    settings.classpath.value = ClassPath.join(classpathElements: _*)
    out foreach (settings.outdir.value = _)

    settings
  }

  def newReporter(sett: Settings, writer: Writer = new StringWriter) =
    new ExtConsoleReporter(sett, Console.in, new PrintWriter(writer))

  private def updatePluginPath(options: String): String = {
    val dir = fileManager.testRootDir
    def absolutize(path: String) = Path(path) match {
      case x if x.isAbsolute  => x.path
      case x                  => (fileManager.testRootDir / x).toAbsolute.path
    }

    val (opt1, opt2) = (options split "\\s").toList partition (_ startsWith "-Xplugin:")
    val plugins = opt1 map (_ stripPrefix "-Xplugin:") flatMap (_ split pathSeparator) map absolutize
    val pluginOption = if (opt1.isEmpty) Nil else List("-Xplugin:" + (plugins mkString pathSeparator))

    (opt2 ::: pluginOption) mkString " "
  }

  def compile(out: Option[File], files: List[File], kind: String, log: File): Boolean = {
    val testSettings = newSettings(out map (_.getAbsolutePath))
    val logWriter = new FileWriter(log)

    // check whether there is a ".flags" file
    val flagsFileName = "%s.flags" format (basename(log.getName) dropRight 4) // 4 is "-run" or similar
    val argString = (io.File(log).parent / flagsFileName) ifFile (x => updatePluginPath(x.slurp())) getOrElse ""
    val allOpts = fileManager.SCALAC_OPTS+" "+argString
    val args = (allOpts split "\\s").toList

    NestUI.verbose("scalac options: "+allOpts)

    val command = new CompilerCommand(args, testSettings)
    val global = newGlobal(command.settings, logWriter)
    val testRep: ExtConsoleReporter = global.reporter.asInstanceOf[ExtConsoleReporter]

    val testFileFn: (File, FileManager) => TestFile = kind match {
      case "pos"        => PosTestFile.apply
      case "neg"        => NegTestFile.apply
      case "run"        => RunTestFile.apply
      case "jvm"        => JvmTestFile.apply
      case "shootout"   => ShootoutTestFile.apply
      case "scalap"     => ScalapTestFile.apply
      case "scalacheck" => ScalaCheckTestFile.apply
    }
    val test: TestFile = testFileFn(files.head, fileManager)
    test.defineSettings(command.settings, out.isEmpty)
    val toCompile = files map (_.getPath)

    try {
      NestUI.verbose("compiling "+toCompile)
      try new global.Run compile toCompile
      catch {
        case FatalError(msg) =>
          testRep.error(null, "fatal error: " + msg)
      }

      testRep.printSummary
      testRep.writer.flush
      testRep.writer.close
    }
    catch {
      case e =>
        e.printStackTrace()
        return false
    }
    finally logWriter.close()

    !testRep.hasErrors
  }
}

// class ReflectiveCompiler(val fileManager: ConsoleFileManager) extends SimpleCompiler {
//   import fileManager.{latestCompFile, latestPartestFile}
//
//   val sepUrls = Array(latestCompFile.toURI.toURL, latestPartestFile.toURI.toURL)
//   //NestUI.verbose("constructing URLClassLoader from URLs "+latestCompFile+" and "+latestPartestFile)
//
//   val sepLoader = new java.net.URLClassLoader(sepUrls, null)
//
//   val sepCompilerClass =
//     sepLoader.loadClass("scala.tools.partest.nest.DirectCompiler")
//   val sepCompiler = sepCompilerClass.newInstance()
//
//   // needed for reflective invocation
//   val fileClass = Class.forName("java.io.File")
//   val stringClass = Class.forName("java.lang.String")
//   val sepCompileMethod =
//     sepCompilerClass.getMethod("compile", fileClass, stringClass)
//   val sepCompileMethod2 =
//     sepCompilerClass.getMethod("compile", fileClass, stringClass, fileClass)
//
//   /* This method throws java.lang.reflect.InvocationTargetException
//    * if the compiler crashes.
//    * This exception is handled in the shouldCompile and shouldFailCompile
//    * methods of class CompileManager.
//    */
//   def compile(out: Option[File], files: List[File], kind: String, log: File): Boolean = {
//     val res = sepCompileMethod2.invoke(sepCompiler, out, files, kind, log).asInstanceOf[java.lang.Boolean]
//     res.booleanValue()
//   }
// }

class CompileManager(val fileManager: FileManager) {
  var compiler: SimpleCompiler = new /*ReflectiveCompiler*/ DirectCompiler(fileManager)

  var numSeparateCompilers = 1
  def createSeparateCompiler() = {
    numSeparateCompilers += 1
    compiler = new /*ReflectiveCompiler*/ DirectCompiler(fileManager)
  }

  /* This method returns true iff compilation succeeds.
   */
  def shouldCompile(files: List[File], kind: String, log: File): Boolean = {
    createSeparateCompiler()
    compiler.compile(None, files, kind, log)
  }

  /* This method returns true iff compilation succeeds.
   */
  def shouldCompile(out: File, files: List[File], kind: String, log: File): Boolean = {
    createSeparateCompiler()
    compiler.compile(Some(out), files, kind, log)
  }

  /* This method returns true iff compilation fails
   * _and_ the compiler does _not_ crash or loop.
   *
   * If the compiler crashes, this method returns false.
   */
  def shouldFailCompile(files: List[File], kind: String, log: File): Boolean = {
    createSeparateCompiler()
    !compiler.compile(None, files, kind, log)
  }

  /* This method returns true iff compilation fails
   * _and_ the compiler does _not_ crash or loop.
   *
   * If the compiler crashes, this method returns false.
   */
  def shouldFailCompile(out: File, files: List[File], kind: String, log: File): Boolean = {
    createSeparateCompiler()
    !compiler.compile(Some(out), files, kind, log)
  }
}
