/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.io.{BufferedReader, File, FileInputStream, FileOutputStream,
                FileReader, InputStreamReader, PrintWriter}
import java.lang.reflect.InvocationTargetException
import java.net.URL
import java.util.jar.{JarEntry, JarOutputStream}

import scala.tools.nsc.io.PlainFile
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.util.{CompoundSourceFile, SourceFile, SourceFileFragment}

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
    if (! matcher.find)
      throw new Error("script file does not close its header with !# or ::!#")

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


  /** Wrap a script file into a runnable object named
   *  <code>scala.scripting.Main</code>.
   */
  def wrappedScript(
    objectName: String,
    filename: String,
    getSourceFile: PlainFile => SourceFile): SourceFile =
  {
    val (maybePack, objName) = splitObjectName(objectName)

    val packageDecl =
      maybePack match {
	case Some(pack) => "package " + pack + "\n"
	case None => ""
      }

    val preamble =
      new SourceFile("<script preamble>",
          (packageDecl +
          "object " + objName + " {\n" +
          "  def main(argv: Array[String]): Unit = {\n" +
          "  val args = argv;\n").toCharArray)

    val middle = {
      val f = new File(filename)
      new SourceFileFragment(
          getSourceFile(new PlainFile(f)),
          headerLength(filename),
          f.length.asInstanceOf[Int])
    }
    val end = new SourceFile("<script trailer>", "\n} }\n".toCharArray)

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
    for (setting:settings.StringSetting <- List(
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
        List("-script", scriptMain(settings), scriptFile))

    val socket = CompileSocket.getOrCreateSocket("")
    val out = new PrintWriter(socket.getOutputStream(), true)
    val in = new BufferedReader(new InputStreamReader(socket.getInputStream()))

    out.println(CompileSocket.getPassword(socket.getPort))
    out.println(compArgs.mkString("", "\0", ""))

    var compok = true

    var fromServer = in.readLine()
    while (fromServer ne null) {
      Console.println(fromServer)
      if (CompileSocket.errorPattern.matcher(fromServer).matches)
        compok = false

      fromServer = in.readLine()
    }
    in.close()
    out.close()
    socket.close()

    compok
  }

  /** Compile a script and then run the specified closure with
    * a classpath for the compiled script.
    */
  private def withCompiledScript
        (settings: GenericRunnerSettings, scriptFile: String)
        (handler: String => Unit)
        : Unit =
  {
    import Interpreter.deleteRecursively

    /** Compiles the script file, and returns two things:
      * the directory with the compiled class files,
      * and a flag for whether the compilation succeeded.
      */
    def compile: (File, Boolean) = {
      val compiledPath = File.createTempFile("scalascript", "")
      compiledPath.delete  // the file is created as a file; make it a directory
      compiledPath.mkdirs

      settings.outdir.value = compiledPath.getPath

      if (settings.nocompdaemon.value) {
        val reporter = new ConsoleReporter(settings)
        val compiler = new Global(settings, reporter)
        val cr = new compiler.Run
	val wrapped =
	  wrappedScript(
	    scriptMain(settings),
	    scriptFile,
	    compiler.getSourceFile _)
        cr.compileSources(List(wrapped))
        (compiledPath, !reporter.hasErrors)
      } else {
        val compok = compileWithDaemon(settings, scriptFile)
        (compiledPath, compok)
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
        val (compiledPath, compok) = compile
        try {
          if (compok) {
            tryMakeJar(jarFile, compiledPath)
            if (jarOK) {
              deleteRecursively(compiledPath)
              handler(jarFile.getAbsolutePath)
            } else {
              // run from the interpreter's temporary
              // directory
              handler(compiledPath.getPath)
            }
          }
        } finally {
          deleteRecursively(compiledPath)
        }
      }
    } else {
      // don't use the cache; just run from the interpreter's temporary directory
      val (compiledPath, compok) = compile
      try {
        if (compok)
          handler(compiledPath.getPath)
      } finally {
        deleteRecursively(compiledPath)
      }
    }
  }

  /** Run a script file with the specified arguments and compilation
   *  settings.
   *
   *  @param settings   ...
   *  @param scriptFile ...
   *  @param scriptArgs ...
   */
  def runScript(
      settings: GenericRunnerSettings,
      scriptFile: String,
      scriptArgs: List[String])
  {
    val f = new File(scriptFile)
    if (!f.isFile) {
      Console.println("no such file: " + scriptFile)
      return
    }

    withCompiledScript(settings, scriptFile)(compiledLocation => {
      def paths0(str: String): List[String] =
        str.split(File.pathSeparator).toList

      def fileToURL(f: File): Option[URL] =
        try { Some(f.toURL) }
        catch { case e => Console.println(e); None }

      def paths(str: String): List[URL] =
        for (
         file <- paths0(str) map (new File(_)) if file.exists;
          val url = fileToURL(file); if !url.isEmpty
        ) yield url.get

      val classpath: List[URL] =
        paths(settings.bootclasspath.value) :::
        paths(compiledLocation) :::
        paths(settings.classpath.value)

      try {
        ObjectRunner.run(
          classpath,
          scriptMain(settings),
          scriptArgs.toArray)
      } catch {
        case e:InvocationTargetException =>
          e.getCause.printStackTrace
      }
    })
  }
}
