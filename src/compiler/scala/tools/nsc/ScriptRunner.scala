/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
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
import scala.io.File
// import scala.io.arm.ManagedResource
import java.io.{ File => JFile }
import java.lang.reflect.InvocationTargetException
import java.net.URL
import java.util.jar.{ JarEntry, JarOutputStream }
import java.util.regex.Pattern

import scala.tools.nsc.io.PlainFile
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
  private def jarFileFor(scriptFile: String): JFile = {
    val name =
      if (scriptFile endsWith ".jar") scriptFile
      else scriptFile + ".jar"

    File(name).file
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
  private def tryMakeJar(jarFile: JFile, sourcePath: JFile) = {
    def addFromDir(jar: JarOutputStream, dir: JFile, prefix: String) {
      def addFileToJar(entry: JFile) = {
        jar putNextEntry new JarEntry(prefix + entry.getName)
        copyStreams(new FileInputStream(entry), jar)
        jar.closeEntry
      }

      dir.listFiles foreach { entry =>
        if (entry.isFile) addFileToJar(entry)
        else addFromDir(jar, entry, prefix + entry.getName + "/")
      }
    }

    try {
      val jar = new JarOutputStream(File(jarFile).outputStream())
      addFromDir(jar, sourcePath, "")
      jar.close
    }
    catch {
      case _: Error => jarFile.delete() // XXX what errors to catch?
    }
  }

  /** Read the entire contents of a file as a String. */
  private def contentsOfFile(filename: String) = File(filename).toSource().mkString

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
    val scriptFile = CompileClient absFileName scriptFileIn

    {
      import settings._
      for (setting <- List(classpath, sourcepath, bootclasspath, extdirs, outdir)) {
        // DBG("%s = %s".format(setting.name, setting.value))
        setting.value = CompileClient absFileName setting.value
      }
    }

    val compSettingNames  = new Settings(error).allSettings map (_.name)
    val compSettings      = settings.allSettings filter (compSettingNames contains _.name)
    val coreCompArgs      = compSettings flatMap (_.unparse)
    val compArgs          = coreCompArgs ::: List("-Xscript", scriptMain(settings), scriptFile)
    var compok            = true

    // XXX temporary as I started using ManagedResource not remembering it wasn't checked in.
    def ManagedResource[T](x: => T) = Some(x)

    for {
      socket <- ManagedResource(CompileSocket getOrCreateSocket "")
      val _ = if (socket == null) return false
      out <- ManagedResource(new PrintWriter(socket.getOutputStream(), true))
      in <- ManagedResource(new BufferedReader(new InputStreamReader(socket.getInputStream())))
    } {
      out println (CompileSocket getPassword socket.getPort)
      out println (compArgs mkString "\0")

      for (fromServer <- (Iterator continually in.readLine()) takeWhile (_ != null)) {
        Console.err println fromServer
        if (CompileSocket.errorPattern matcher fromServer matches)
          compok = false
      }
      // XXX temp until managed resource is available
      in.close() ; out.close() ; socket.close()
    }

    compok
  }

  protected def newGlobal(settings: Settings, reporter: Reporter) =
    new Global(settings, reporter)

  /** Compile a script and then run the specified closure with
    * a classpath for the compiled script.
    *
    * @returns true if compilation and the handler succeeds, false otherwise.
    */
  private def withCompiledScript(
    settings: GenericRunnerSettings,
    scriptFile: String)
    (handler: String => Boolean): Boolean =
  {
    import Interpreter.deleteRecursively

    /** Compiles the script file, and returns the directory with the compiled
     *  class files, if the compilation succeeded.
     */
    def compile: Option[JFile] = {
      val compiledPath = File tempdir "scalascript"

      // delete the directory after the user code has finished
      addShutdownHook(deleteRecursively(compiledPath.file))

      settings.outdir.value = compiledPath.path

      if (settings.nocompdaemon.value) {
        val reporter = new ConsoleReporter(settings)
        val compiler = newGlobal(settings, reporter)
        val cr = new compiler.Run
        val wrapped = wrappedScript(scriptMain(settings), scriptFile, compiler getSourceFile _)

        cr compileSources List(wrapped)
        if (reporter.hasErrors) None else Some(compiledPath.file)
      }
      else if (compileWithDaemon(settings, scriptFile)) Some(compiledPath.file)
      else None
    }

    if (settings.savecompiled.value) {
      val jarFile = File(jarFileFor(scriptFile))
      def jarOK   = jarFile.canRead && (jarFile isFresher File(scriptFile))

      def recompile() = {
        jarFile.delete

        compile match {
          case Some(compiledPath) =>
            tryMakeJar(jarFile.file, compiledPath)
            if (jarOK) {
              deleteRecursively(compiledPath)
              handler(jarFile.absolutePath)
            }
            // jar failed; run directly from the class files
            else handler(compiledPath.getPath)
          case _  => false
        }
      }

      if (jarOK) handler(jarFile.absolutePath)  // pre-compiled jar is current
      else recompile()                          // jar old - recompile the script.
    }
    // don't use a cache jar at all--just use the class files
    else compile map (cp => handler(cp.getPath)) getOrElse false
  }

  /** Run a script after it has been compiled
   *
   * @returns true if execution succeeded, false otherwise
   */
  private def runCompiled(
    settings: GenericRunnerSettings,
		compiledLocation: String,
		scriptArgs: List[String]): Boolean =
	{
    def fileToURL(f: JFile): Option[URL] =
      try Some(f.toURL) catch { case _: Exception => None }

    def paths(str: String, expandStar: Boolean): List[URL] =
      for {
        file <- ClassPath.expandPath(str, expandStar) map (new JFile(_))
        if file.exists
        url <- fileToURL(file)
      } yield url

    val classpath =
      (paths(settings.bootclasspath.value, true) :::
       paths(compiledLocation, false) :::
       paths(settings.classpath.value, true))

    try {
      ObjectRunner.run(
        classpath,
        scriptMain(settings),
        scriptArgs.toArray)
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
   * @returns true if compilation and execution succeeded, false otherwise.
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
   * @returns true if compilation and execution succeeded, false otherwise.
   */
  def runCommand(
    settings: GenericRunnerSettings,
    command: String,
		scriptArgs: List[String]) : Boolean =
	{
    val scriptFile = File.tempfile("scalacmd", ".scala")
    // save the command to the file
    scriptFile writeAll List(command)

    try withCompiledScript(settings, scriptFile.path) { runCompiled(settings, _, scriptArgs) }
    finally scriptFile.delete()  // in case there was a compilation error
  }
}
