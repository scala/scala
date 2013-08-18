/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools.partest
package nest

import scala.tools.nsc.{ Global, Settings, CompilerCommand, FatalError }
import scala.tools.nsc.reporters.{ Reporter, ConsoleReporter }
import scala.tools.nsc.util.{ FakePos, stackTraceString }
import scala.tools.nsc.Properties.{ setProp, propOrEmpty }
import scala.reflect.io.AbstractFile
import scala.reflect.internal.util.Position
import java.io.{ BufferedReader, PrintWriter, FileReader, Writer, FileWriter }

class ExtConsoleReporter(settings: Settings, val writer: PrintWriter) extends ConsoleReporter(settings, Console.in, writer) {
  shortname = true
  // override def error(pos: Position, msg: String): Unit
}

class TestSettings(cp: String, error: String => Unit) extends Settings(error) {
  def this(cp: String) = this(cp, _ => ())

  nowarnings.value  = false
  encoding.value    = "UTF-8"
  classpath.value   = cp
}

class PartestGlobal(settings: Settings, reporter: Reporter) extends Global(settings, reporter) {
  // override def abort(msg: String): Nothing
  // override def globalError(msg: String): Unit
  // override def supplementErrorMessage(msg: String): String
}
class DirectCompiler(val runner: Runner) {
  def newGlobal(settings: Settings, reporter: Reporter): PartestGlobal =
    new PartestGlobal(settings, reporter)

  def newGlobal(settings: Settings, logWriter: FileWriter): Global =
    newGlobal(settings, new ExtConsoleReporter(settings, new PrintWriter(logWriter)))


  /** Massage args to merge plugins and fix paths.
   *  Plugin path can be relative to test root, or cwd is out.
   *  While we're at it, mix in the baseline options, too.
   *  That's how ant passes in the plugins dir.
   */
  private def updatePluginPath(args: List[String], out: AbstractFile, srcdir: AbstractFile): Seq[String] = {
    val dir = PathSettings.testRoot
    // The given path, or the output dir if ".", or a temp dir if output is virtual (since plugin loading doesn't like virtual)
    def pathOrCwd(p: String) =
      if (p == ".") {
        val plugxml = "scalac-plugin.xml"
        val pout = if (out.isVirtual) Directory.makeTemp() else Path(out.path)
        val srcpath = Path(srcdir.path)
        val pd = (srcpath / plugxml).toFile
        if (pd.exists) pd copyTo (pout / plugxml)
        pout.toAbsolute
      } else Path(p)
    def absolutize(path: String) = pathOrCwd(path) match {
      case x if x.isAbsolute => x.path
      case x                 => (dir / x).toAbsolute.path
    }

    val xprefix = "-Xplugin:"
    val (xplugs, others) = args partition (_ startsWith xprefix)
    val Xplugin = if (xplugs.isEmpty) Nil else List(xprefix +
      (xplugs map (_ stripPrefix xprefix) flatMap (_ split pathSeparator) map absolutize mkString pathSeparator)
    )
    runner.suiteRunner.scalacExtraArgs ++ PartestDefaults.scalacOpts.split(' ') ++ others ++ Xplugin
  }

  def compile(opts0: List[String], sources: List[File]): TestState = {
    import runner.{ sources => _, _ }
    import ClassPath.{join, split}

    // adding codelib.jar to the classpath
    // codelib provides the possibility to override standard reify
    // this shields the massive amount of reification tests from changes in the API
    val codeLib = PathSettings.srcCodeLib.fold[List[Path]](x => Nil, lib => List[Path](lib))
    // add the instrumented library version to classpath -- must come first
    val specializedOverride: List[Path] = if (kind == "specialized") List(PathSettings.srcSpecLib.fold(sys.error, identity)) else Nil

    val classPath: List[Path] = specializedOverride ++ codeLib ++ fileManager.testClassPath ++ List[Path](outDir)

    val testSettings = new TestSettings(FileManager.joinPaths(classPath))
    val logWriter    = new FileWriter(logFile)
    val srcDir       = if (testFile.isDirectory) testFile else Path(testFile).parent.jfile
    val opts         = updatePluginPath(opts0, AbstractFile getDirectory outDir, AbstractFile getDirectory srcDir)
    val command      = new CompilerCommand(opts.toList, testSettings)
    val global       = newGlobal(testSettings, logWriter)
    val reporter     = global.reporter.asInstanceOf[ExtConsoleReporter]
    def errorCount   = reporter.ERROR.count

    testSettings.outputDirs setSingleOutput outDir.getPath

    // check that option processing succeeded
    if (opts0.nonEmpty && !command.ok)
      reporter.error(null, opts0.mkString("bad options: ", space, ""))

    def ids = sources.map(_.testIdent) mkString space
    vlog(s"% scalac $ids")

    def execCompile() =
      if (command.shouldStopWithInfo) {
        logWriter append (command getInfoMessage global)
        runner genFail "compilation stopped with info"
      } else {
        new global.Run compile sources.map(_.getPath)
        if (!reporter.hasErrors) runner.genPass()
        else {
          reporter.printSummary()
          reporter.writer.close()
          runner.genFail(s"compilation failed with $errorCount errors")
        }
      }

    try     { execCompile() }
    catch   { case t: Throwable => reporter.error(null, t.getMessage) ; runner.genCrash(t) }
    finally { logWriter.close() }
  }
}
