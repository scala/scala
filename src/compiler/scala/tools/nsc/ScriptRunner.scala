/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.io.{
  InputStream, OutputStream,
  BufferedReader, FileInputStream, FileOutputStream,
  FileReader, InputStreamReader, PrintWriter, FileWriter,
  IOException
}
import java.io.{ File => JFile }
import io.{ Directory, File, Path, PlainFile }
import java.lang.reflect.InvocationTargetException
import java.net.URL
import java.util.jar.{ JarEntry, JarOutputStream }
import java.util.regex.Pattern

import scala.tools.util.PathResolver
import scala.tools.nsc.reporters.{Reporter,ConsoleReporter}
import scala.tools.nsc.util.{ClassPath, CompoundSourceFile, BatchSourceFile, SourceFile, SourceFileFragment}

/** An object that runs Scala code in script files.
 *
 *  <p>For example, here is a complete Scala script on Unix:</pre>
 *  <pre>
 *    #!/bin/sh
 *    exec scala "$0" "$@"
 *    !#
 *    Console.println("Hello, world!")
 *    argv.toList foreach Console.println
 *  </pre>
 *  <p>And here is a batch file example on Windows XP:</p>
 *  <pre>
 *    ::#!
 *    @echo off
 *    call scala %0 %*
 *    goto :eof
 *    ::!#
 *    Console.println("Hello, world!")
 *    argv.toList foreach Console.println
 *  </pre>
 *
 *  @author  Lex Spoon
 *  @version 1.0, 15/05/2006
 *  @todo    It would be better if error output went to stderr instead
 *           of stdout...
 */
object ScriptRunner
{
  /* While I'm chasing down the fsc and script bugs. */
  def DBG(msg: Any) {
    System.err.println(msg.toString)
    System.err.flush()
  }

  /** Default name to use for the wrapped script */
  val defaultScriptMain = "Main"

  private def addShutdownHook(body: => Unit) =
    Runtime.getRuntime addShutdownHook new Thread { override def run { body } }

  /** Pick a main object name from the specified settings */
  def scriptMain(settings: Settings) = settings.script.value match {
    case "" => defaultScriptMain
    case x  => x
  }

  /** Choose a jar filename to hold the compiled version of a script. */
  private def jarFileFor(scriptFile: String): File = {
    val name =
      if (scriptFile endsWith ".jar") scriptFile
      else scriptFile + ".jar"

    File(name)
  }

  def copyStreams(in: InputStream, out: OutputStream) = {
    val buf = new Array[Byte](10240)

    def loop: Unit = in.read(buf, 0, buf.length) match {
      case -1 => in.close()
      case n  => out.write(buf, 0, n) ; loop
    }

    loop
  }

  /** Try to create a jar file out of all the contents
   *  of the directory <code>sourcePath</code>.
   */
  private def tryMakeJar(jarFile: File, sourcePath: Directory) = {
    def addFromDir(jar: JarOutputStream, dir: Directory, prefix: String) {
      def addFileToJar(entry: File) = {
        jar putNextEntry new JarEntry(prefix + entry.name)
        copyStreams(entry.inputStream, jar)
        jar.closeEntry
      }

      dir.list foreach { entry =>
        if (entry.isFile) addFileToJar(entry.toFile)
        else addFromDir(jar, entry.toDirectory, prefix + entry.name + "/")
      }
    }

    try {
      val jar = new JarOutputStream(jarFile.outputStream())
      addFromDir(jar, sourcePath, "")
      jar.close
    }
    catch {
      case _: Error => jarFile.delete() // XXX what errors to catch?
    }
  }

  /** Read the entire contents of a file as a String. */
  private def contentsOfFile(filename: String) = File(filename).slurp()

  /** Find the length of the header in the specified file, if
    * there is one.  The header part starts with "#!" or "::#!"
    * and ends with a line that begins with "!#" or "::!#".
    */
  private def headerLength(filename: String): Int = {
    val headerPattern = Pattern.compile("""^(::)?!#.*(\r|\n|\r\n)""", Pattern.MULTILINE)
    val fileContents = contentsOfFile(filename)
    def isValid = List("#!", "::#!") exists (fileContents startsWith _)

    if (!isValid) 0 else {
      val matcher = headerPattern matcher fileContents
      if (matcher.find) matcher.end
      else throw new IOException("script file does not close its header with !# or ::!#")
    }
  }

  /** Split a fully qualified object name into a
   *  package and an unqualified object name */
  private def splitObjectName(fullname: String): (Option[String], String) =
    (fullname lastIndexOf '.') match {
      case -1   => (None, fullname)
      case idx  => (Some(fullname take idx), fullname drop (idx + 1))
    }

  /** Code that is added to the beginning of a script file to make
   *  it a complete Scala compilation unit.
   */
  protected def preambleCode(objectName: String): String = {
    val (maybePack, objName)  = splitObjectName(objectName)
    val packageDecl           = maybePack map ("package %s\n" format _) getOrElse ("")

    return """|
    |  object %s {
    |    def main(argv: Array[String]): Unit = {
    |      val args = argv
    |      new AnyRef {
    |""".stripMargin.format(objName)
  }

  /** Code that is added to the end of a script file to make
   *  it a complete Scala compilation unit.
   */
  val endCode = """
    |      }
    |    }
    |  }
    |""".stripMargin

  /** Wrap a script file into a runnable object named
   *  <code>scala.scripting.Main</code>.
   */
  def wrappedScript(
    objectName: String,
    filename: String,
    getSourceFile: PlainFile => BatchSourceFile): SourceFile =
  {
    val preamble = new BatchSourceFile("<script preamble>", preambleCode(objectName).toCharArray)
    val middle = {
      val bsf = getSourceFile(PlainFile fromPath filename)
      new SourceFileFragment(bsf, headerLength(filename), bsf.length)
    }
    val end = new BatchSourceFile("<script trailer>", endCode.toCharArray)

    new CompoundSourceFile(preamble, middle, end)
  }

  /** Compile a script using the fsc compilation deamon.
   *
   *  @param settings     ...
   *  @param scriptFileIn ...
   *  @return             ...
   */
  private def compileWithDaemon(
      settings: GenericRunnerSettings,
      scriptFileIn: String): Boolean =
  {
    val scriptFile        = Path(scriptFileIn).toAbsolute.path
    val compSettingNames  = new Settings(error).visibleSettings.toList map (_.name)
    val compSettings      = settings.visibleSettings.toList filter (compSettingNames contains _.name)
    val coreCompArgs      = compSettings flatMap (_.unparse)
    val compArgs          = coreCompArgs ::: List("-Xscript", scriptMain(settings), scriptFile)
    var compok            = true

    val socket = CompileSocket getOrCreateSocket "" getOrElse (return false)
    socket.applyReaderAndWriter { (in, out) =>
      out println (CompileSocket getPassword socket.getPort)
      out println (compArgs mkString "\0")

      for (fromServer <- (Iterator continually in.readLine()) takeWhile (_ != null)) {
        Console.err println fromServer
        if (CompileSocket.errorPattern matcher fromServer matches)
          compok = false
      }
      socket.close()
    }

    compok
  }

  protected def newGlobal(settings: Settings, reporter: Reporter) =
    new Global(settings, reporter)

  /** Compile a script and then run the specified closure with
    * a classpath for the compiled script.
    *
    * @return true if compilation and the handler succeeds, false otherwise.
    */
  private def withCompiledScript(
    settings: GenericRunnerSettings,
    scriptFile: String)
    (handler: String => Boolean): Boolean =
  {
    /** Compiles the script file, and returns the directory with the compiled
     *  class files, if the compilation succeeded.
     */
    def compile: Option[Directory] = {
      val compiledPath = Directory makeTemp "scalascript"

      // delete the directory after the user code has finished
      addShutdownHook(compiledPath.deleteRecursively())

      settings.outdir.value = compiledPath.path

      if (settings.nocompdaemon.value) {
        val reporter = new ConsoleReporter(settings)
        val compiler = newGlobal(settings, reporter)
        val cr = new compiler.Run
        val wrapped = wrappedScript(scriptMain(settings), scriptFile, compiler getSourceFile _)

        cr compileSources List(wrapped)
        if (reporter.hasErrors) None else Some(compiledPath)
      }
      else if (compileWithDaemon(settings, scriptFile)) Some(compiledPath)
      else None
    }

    if (settings.savecompiled.value) {
      val jarFile = jarFileFor(scriptFile)
      def jarOK   = jarFile.canRead && (jarFile isFresher File(scriptFile))

      def recompile() = {
        jarFile.delete()

        compile match {
          case Some(compiledPath) =>
            tryMakeJar(jarFile, compiledPath)
            if (jarOK) {
              compiledPath.deleteRecursively()
              handler(jarFile.toAbsolute.path)
            }
            // jar failed; run directly from the class files
            else handler(compiledPath.path)
          case _  => false
        }
      }

      if (jarOK) handler(jarFile.toAbsolute.path) // pre-compiled jar is current
      else recompile()                            // jar old - recompile the script.
    }
    // don't use a cache jar at all--just use the class files
    else compile map (cp => handler(cp.path)) getOrElse false
  }

  /** Run a script after it has been compiled
   *
   * @return true if execution succeeded, false otherwise
   */
  private def runCompiled(
    settings: GenericRunnerSettings,
		compiledLocation: String,
		scriptArgs: List[String]): Boolean =
	{
	  val pr = new PathResolver(settings)
	  val classpath = pr.asURLs :+ File(compiledLocation).toURL

    try {
      ObjectRunner.run(
        classpath,
        scriptMain(settings),
        scriptArgs)
      true
    }
    catch {
      case e @ (_: ClassNotFoundException | _: NoSuchMethodException) =>
        Console println e
        false
      case e: InvocationTargetException =>
        e.getCause.printStackTrace
        false
    }
  }

  /** Run a script file with the specified arguments and compilation
   *  settings.
   *
   * @return true if compilation and execution succeeded, false otherwise.
   */
  def runScript(
    settings: GenericRunnerSettings,
		scriptFile: String,
		scriptArgs: List[String]): Boolean =
	{
	  if (File(scriptFile).isFile)
	    withCompiledScript(settings, scriptFile) { runCompiled(settings, _, scriptArgs) }
	  else
	    throw new IOException("no such file: " + scriptFile)
  }

  /** Run a command
   *
   * @return true if compilation and execution succeeded, false otherwise.
   */
  def runCommand(
    settings: GenericRunnerSettings,
    command: String,
		scriptArgs: List[String]) : Boolean =
	{
    val scriptFile = File.makeTemp("scalacmd", ".scala")
    // save the command to the file
    scriptFile writeAll List(command)

    try withCompiledScript(settings, scriptFile.path) { runCompiled(settings, _, scriptArgs) }
    finally scriptFile.delete()  // in case there was a compilation error
  }
}
