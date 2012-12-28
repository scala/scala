/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import scala.tools.nsc.{ Global, Settings, CompilerCommand, FatalError, io }
import scala.tools.nsc.io.{ File => SFile }
import scala.tools.nsc.interactive.RangePositions
import scala.tools.nsc.reporters.{ Reporter, ConsoleReporter }
import scala.tools.nsc.util.{ ClassPath, FakePos }
import scala.tools.nsc.Properties.{ setProp, propOrEmpty }
import scala.tools.util.PathResolver
import io.Path
import java.io.{ File, BufferedReader, PrintWriter, FileReader, Writer, FileWriter, StringWriter }
import File.pathSeparator

sealed abstract class CompilationOutcome {
  def merge(other: CompilationOutcome): CompilationOutcome
  def isPositive = this eq CompileSuccess
  def isNegative = this eq CompileFailed
}
case object CompileSuccess extends CompilationOutcome {
  def merge(other: CompilationOutcome) = other
}
case object CompileFailed extends CompilationOutcome {
  def merge(other: CompilationOutcome) = if (other eq CompileSuccess) this else other
}
case object CompilerCrashed extends CompilationOutcome {
  def merge(other: CompilationOutcome) = this
}

class ExtConsoleReporter(settings: Settings, val writer: PrintWriter) extends ConsoleReporter(settings, Console.in, writer) {
  shortname = true
}

class TestSettings(cp: String, error: String => Unit) extends Settings(error) {
  def this(cp: String) = this(cp, _ => ())

  nowarnings.value  = false
  encoding.value    = "UTF-8"
  classpath.value   = cp
}

abstract class SimpleCompiler {
  def compile(out: Option[File], files: List[File], kind: String, log: File): CompilationOutcome
}

class DirectCompiler(val fileManager: FileManager) extends SimpleCompiler {
  def newGlobal(settings: Settings, reporter: Reporter): Global =
    if (settings.Yrangepos.value)
      new Global(settings, reporter) with RangePositions
    else
      new Global(settings, reporter)

  def newGlobal(settings: Settings, logWriter: FileWriter): Global =
    newGlobal(settings, new ExtConsoleReporter(settings, new PrintWriter(logWriter)))

  def newSettings(): TestSettings = new TestSettings(fileManager.LATEST_LIB)
  def newSettings(outdir: String): TestSettings = {
    val cp = ClassPath.join(fileManager.LATEST_LIB, outdir)
    val s = new TestSettings(cp)
    s.outdir.value = outdir

    s
  }

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

  def compile(out: Option[File], files: List[File], kind: String, log: File): CompilationOutcome = {
    val testSettings = out match {
      case Some(f)  => newSettings(f.getAbsolutePath)
      case _        => newSettings()
    }
    val logWriter = new FileWriter(log)

    // check whether there is a ".flags" file
    val logFile = basename(log.getName)
    val flagsFileName = "%s.flags" format (logFile.substring(0, logFile.lastIndexOf("-")))
    val argString = (io.File(log).parent / flagsFileName) ifFile (x => updatePluginPath(x.slurp())) getOrElse ""

    // slurp local flags (e.g., "A_1.flags")
    val fstFile = SFile(files(0))
    def isInGroup(num: Int) = fstFile.stripExtension endsWith ("_" + num)
    val inGroup = (1 to 9) flatMap (group => if (isInGroup(group)) List(group) else List())
    val localFlagsList = if (inGroup.nonEmpty) {
      val localArgString = (fstFile.parent / (fstFile.stripExtension + ".flags")) ifFile (x => updatePluginPath(x.slurp())) getOrElse ""
      localArgString.split(' ').toList.filter(_.length > 0)
    } else List()

    val allOpts = fileManager.SCALAC_OPTS.toList ::: argString.split(' ').toList.filter(_.length > 0) ::: localFlagsList
    val args = allOpts.toList

    NestUI.verbose("scalac options: "+allOpts)

    val command = new CompilerCommand(args, testSettings)
    val global = newGlobal(command.settings, logWriter)
    val testRep: ExtConsoleReporter = global.reporter.asInstanceOf[ExtConsoleReporter]

    val testFileFn: (File, FileManager) => TestFile = kind match {
      case "pos"          => PosTestFile.apply
      case "neg"          => NegTestFile.apply
      case "run"          => RunTestFile.apply
      case "jvm"          => JvmTestFile.apply
      case "shootout"     => ShootoutTestFile.apply
      case "scalap"       => ScalapTestFile.apply
      case "scalacheck"   => ScalaCheckTestFile.apply
      case "specialized"  => SpecializedTestFile.apply
      case "instrumented" => InstrumentedTestFile.apply
      case "presentation" => PresentationTestFile.apply
      case "ant"          => AntTestFile.apply
    }
    val test: TestFile = testFileFn(files.head, fileManager)
    if (!test.defineSettings(command.settings, out.isEmpty)) {
      testRep.error(FakePos("partest"), test.flags match {
        case Some(flags)  => "bad flags: " + flags
        case _            => "bad settings: " + command.settings
      })
    }

    val toCompile = files map (_.getPath)

    try {
      NestUI.verbose("compiling "+toCompile)
      NestUI.verbose("with classpath: "+global.classPath.toString)
      NestUI.verbose("and java classpath: "+ propOrEmpty("java.class.path"))
      try new global.Run compile toCompile
      catch {
        case FatalError(msg) =>
          testRep.error(null, "fatal error: " + msg)
          return CompilerCrashed
      }

      testRep.printSummary()
      testRep.writer.close()
    }
    finally logWriter.close()

    if (testRep.hasErrors) CompileFailed
    else CompileSuccess
  }
}

class CompileManager(val fileManager: FileManager) {
  private def newCompiler = new DirectCompiler(fileManager)
  def attemptCompile(outdir: Option[File], sources: List[File], kind: String, log: File): CompilationOutcome =
    newCompiler.compile(outdir, sources, kind, log)
}
