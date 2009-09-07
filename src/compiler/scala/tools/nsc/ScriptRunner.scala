/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.io.{BufferedReader, File, FileInputStream, FileOutputStream,
                FileReader, InputStreamReader, PrintWriter,
	        FileWriter, IOException}
import java.lang.reflect.InvocationTargetException
import java.net.URL
import java.util.jar.{JarEntry, JarOutputStream}

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
object ScriptRunner {
  /** Default name to use for the wrapped script */
  val defaultScriptMain = "Main"

  /** Pick a main object name from the specified settings */
  def scriptMain(settings: Settings) =
    if (settings.script.value == "")
      defaultScriptMain
    else
      settings.script.value

  /** Choose a jar filename to hold the compiled version
   * of a script
   */
  private def jarFileFor(scriptFile: String): File = {
    val filename =
      if (scriptFile.matches(".*\\.[^.\\\\/]*"))
        scriptFile.replaceFirst("\\.[^.\\\\/]*$", ".jar")
      else
        scriptFile + ".jar"

    new File(filename)
  }

  /** Try to create a jar file out of all the contents
   *  of the directory <code>sourcePath</code>.
   */
  private def tryMakeJar(jarFile: File, sourcePath: File) = {
    try {
      val jarFileStream = new FileOutputStream(jarFile)
      val jar = new JarOutputStream(jarFileStream)
      val buf = new Array[Byte](10240)

      def addFromDir(dir: File, prefix: String) {
        for (entry <- dir.listFiles) {
          if (entry.isFile) {
            jar.putNextEntry(new JarEntry(prefix + entry.getName))

            val input = new FileInputStream(entry)
            var n = input.read(buf, 0, buf.length)
            while (n >= 0) {
              jar.write (buf, 0, n)
              n = input.read(buf, 0, buf.length)
            }
            jar.closeEntry
            input.close
          } else {
            addFromDir(entry, prefix + entry.getName + "/")
          }
        }
      }

      addFromDir(sourcePath, "")
      jar.close
    } catch {
      case _:Error => jarFile.delete // XXX what errors to catch?
    }
  }


  /** Read the entire contents of a file as a String. */
  private def contentsOfFile(filename: String): String = {
    val strbuf = new StringBuilder
    val reader = new FileReader(filename)
    val cbuf = new Array[Char](1024)
    while(true) {
      val n = reader.read(cbuf)
      if (n <= 0)
        return strbuf.toString
      strbuf.append(cbuf, 0, n)
    }
    throw new Error("impossible")
  }

  /** Find the length of the header in the specified file, if
    * there is one.  The header part starts with "#!" or "::#!"
    * and ends with a line that begins with "!#" or "::!#".
    */
  private def headerLength(filename: String): Int = {
    import java.util.regex._

    val fileContents = contentsOfFile(filename)

    if (!(fileContents.startsWith("#!") || fileContents.startsWith("::#!")))
      return 0

    val matcher =
      (Pattern.compile("^(::)?!#.*(\\r|\\n|\\r\\n)", Pattern.MULTILINE)
              .matcher(fileContents))
    if (!matcher.find)
      throw new IOException("script file does not close its header with !# or ::!#")
    return matcher.end
  }

  /** Split a fully qualified object name into a
   *  package and an unqualified object name */
  private def splitObjectName(fullname: String):
  (Option[String],String) =
  {
    val idx = fullname.lastIndexOf('.')
    if (idx < 0)
      (None, fullname)
    else
      (Some(fullname.substring(0,idx)), fullname.substring(idx+1))
  }

  /** Code that is added to the beginning of a script file to make
   *  it a complete Scala compilation unit.
   */
  protected def preambleCode(objectName: String) =  {
    val (maybePack, objName) = splitObjectName(objectName)

    val packageDecl =
      maybePack match {
	case Some(pack) => "package " + pack + "\n"
	case None => ""
      }

    (packageDecl +
     "object " + objName + " {\n" +
     "  def main(argv: Array[String]): Unit = {\n" +
     "  val args = argv;\n" +
     "  new AnyRef {\n")
  }

  /** Code that is added to the end of a script file to make
   *  it a complete Scala compilation unit.
   */
  val endCode = "\n} \n} }\n"


  /** Wrap a script file into a runnable object named
   *  <code>scala.scripting.Main</code>.
   */
  def wrappedScript(
    objectName: String,
    filename: String,
    getSourceFile: PlainFile => SourceFile): SourceFile =
  {
    val preamble =
      new BatchSourceFile("<script preamble>",
		     preambleCode(objectName).toCharArray)

    val middle = {
      val f = new File(filename)
      val bsf = getSourceFile(new PlainFile(f)).asInstanceOf[BatchSourceFile]
      new SourceFileFragment(
          bsf,
          headerLength(filename),
          bsf.length)
//          f.length.asInstanceOf[Int])
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
    val scriptFile = CompileClient.absFileName(scriptFileIn)
    for (setting <- List(
            settings.classpath,
            settings.sourcepath,
            settings.bootclasspath,
            settings.extdirs,
            settings.outdir))
      setting.value = CompileClient.absFileNames(setting.value)

    val compSettingNames =
      (new Settings(error)).allSettings.map(_.name)

    val compSettings =
      settings.allSettings.filter(stg =>
        compSettingNames.contains(stg.name))

    val coreCompArgs =
      compSettings.foldLeft[List[String]](Nil)((args, stg) =>
        stg.unparse ::: args)

    val compArgs =
      (coreCompArgs :::
        List("-Xscript", scriptMain(settings), scriptFile))

    val socket = CompileSocket.getOrCreateSocket("")
    if (socket eq null)
      return false

    val out = new PrintWriter(socket.getOutputStream(), true)
    val in = new BufferedReader(new InputStreamReader(socket.getInputStream()))

    out.println(CompileSocket.getPassword(socket.getPort))
    out.println(compArgs.mkString("", "\0", ""))

    var compok = true

    var fromServer = in.readLine()
    while (fromServer ne null) {
      Console.err.println(fromServer)
      if (CompileSocket.errorPattern.matcher(fromServer).matches)
        compok = false

      fromServer = in.readLine()
    }
    in.close()
    out.close()
    socket.close()

    compok
  }

  protected def newGlobal(settings: Settings, reporter: Reporter) =
    new Global(settings, reporter)

  /** Compile a script and then run the specified closure with
    * a classpath for the compiled script.
    *
    * @returns true if compilation and the handler succeeds, false otherwise.
    */
  private def withCompiledScript
        (settings: GenericRunnerSettings, scriptFile: String)
        (handler: String => Boolean)
        : Boolean = {
    import Interpreter.deleteRecursively

    /** Compiles the script file, and returns
      * the directory with the compiled class files,
      * if the compilation succeeded.
      */
    def compile: Option[File] = {
      val compiledPath = File.createTempFile("scalascript", "")
      compiledPath.delete  // the file is created as a file; make it a directory
      compiledPath.mkdirs

      // delete the directory after the user code has finished
      Runtime.getRuntime.addShutdownHook(new Thread {
	override def run { deleteRecursively(compiledPath) }})

      settings.outdir.value = compiledPath.getPath

      if (settings.nocompdaemon.value) {
        val reporter = new ConsoleReporter(settings)
        val compiler = newGlobal(settings, reporter)
        val cr = new compiler.Run
	  val wrapped =
	    wrappedScript(
	      scriptMain(settings),
	      scriptFile,
	      compiler.getSourceFile _)
          cr.compileSources(List(wrapped))
          if (!reporter.hasErrors)
	    Some(compiledPath)
	  else
	    None
      } else {
        if (compileWithDaemon(settings, scriptFile))
          Some(compiledPath)
	else
	  None
      }
    }

    if (settings.savecompiled.value) {
      val jarFile = jarFileFor(scriptFile)

      def jarOK = (jarFile.canRead &&
        (jarFile.lastModified > new File(scriptFile).lastModified))

      if (jarOK) {
        // pre-compiled jar is current
        handler(jarFile.getAbsolutePath)
      } else {
        // The pre-compiled jar is old.  Recompile the script.
        jarFile.delete

        compile match {
          case Some(compiledPath) =>
            tryMakeJar(jarFile, compiledPath)
            if (jarOK) {
	      deleteRecursively(compiledPath)  // may as well do it now
	      handler(jarFile.getAbsolutePath)
            } else {
	      // jar failed; run directly from the class files
	      handler(compiledPath.getPath)
            }
	  case None => false
        }
      }
    } else {
      // don't use a cache jar at all--just use the class files
      compile match {
	case Some(compiledPath) => handler(compiledPath.getPath)
	case None => false
      }
    }
  }


  /** Run a script after it has been compiled
   *
   * @returns true if execution succeeded, false otherwise
   */
  private def runCompiled(settings: GenericRunnerSettings,
			  compiledLocation: String,
			  scriptArgs: List[String]) : Boolean = {
    def fileToURL(f: File): Option[URL] =
      try { Some(f.toURL) }
    catch { case e => Console.err.println(e); None }

    def paths(str: String, expandStar: Boolean): List[URL] =
      for (
        file <- ClassPath.expandPath(str, expandStar) map (new File(_)) if file.exists;
        val url = fileToURL(file); if !url.isEmpty
      ) yield url.get

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
    } catch {
      case e: ClassNotFoundException =>
        Console.println(e)
        false
      case e: NoSuchMethodException =>
        Console.println(e)
        false
      case e:InvocationTargetException =>
        e.getCause.printStackTrace
        false
    }
  }


  /** Run a script file with the specified arguments and compilation
   *  settings.
   *
   * @returns true if compilation and execution succeeded, false otherwise.
   */
  def runScript(settings: GenericRunnerSettings,
		scriptFile: String,
		scriptArgs: List[String]) : Boolean = {
    val f = new File(scriptFile)
    if (!f.isFile) {
      throw new IOException("no such file: " + scriptFile)
    } else {
      try {
	withCompiledScript(settings, scriptFile){compiledLocation =>
	  runCompiled(settings, compiledLocation, scriptArgs)
	}
      } catch {
	case e => throw e
      }
    }
  }

  /** Run a command
   *
   * @returns true if compilation and execution succeeded, false otherwise.
   */
  def runCommand(settings: GenericRunnerSettings,
		 command: String,
		 scriptArgs: List[String]) : Boolean = {
    val scriptFile = File.createTempFile("scalacmd", ".scala")

    // save the command to the file
    {
      val str = new FileWriter(scriptFile)
      str.write(command)
      str.close()
    }

    try {
      withCompiledScript(settings, scriptFile.getPath){compiledLocation =>
        runCompiled(settings, compiledLocation, scriptArgs)
      }
    } catch {
      case e => throw e
    } finally scriptFile.delete()  // in case there was a compilation error
  }
}
