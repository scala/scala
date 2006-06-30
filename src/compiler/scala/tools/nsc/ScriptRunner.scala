/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.io._
import java.util.jar._
import java.lang.reflect.InvocationTargetException
import scala.tools.nsc.util._
import scala.tools.nsc.io._

/** An object that runs Scala code in script files.
 *
 *  For example, here is a complete Scala script on Unix:
 *
 *    #!/bin/sh
 *    exec scala "$0" "$@"
 *    !#
 *    Console.println("Hello, world!")
 *    argv.toList foreach Console.println
 *
 * And here is a batch file example on Windows XP:
 *
 *    ::#!
 *    @echo off
 *    call scala %0 %*
 *    goto :eof
 *    ::!#
 *    Console.println("Hello, world!")
 *    argv.toList foreach Console.println
 *
 * TODO: It would be better if error output went to stderr instead
 * of stdout....
 */
object ScriptRunner {
  /** Choose a jar filename to hold the compiled version
    * of a script
    */
  private def jarFileFor(scriptFile: String): File = {
    val filename =
      if(scriptFile.matches(".*\\.[^.\\\\/]*"))
        scriptFile.replaceFirst("\\.[^.\\\\/]*$", ".jar")
      else
        scriptFile + ".jar"

    new File(filename)
  }

  /** Try to create a jar out of all the contents
    * of a directory.
    */
  private def tryMakeJar(jarFile: File, sourcePath: File) = {
    try {
      val jarFileStream = new FileOutputStream(jarFile)
      val jar = new JarOutputStream(jarFileStream)
      val buf = new Array[byte](10240)

      def addFromDir(dir: File, prefix: String): Unit = {
        for(val entry <- dir.listFiles) {
          if(entry.isFile) {
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
      case _ => jarFile.delete // XXX what errors to catch?
    }
  }

  /** Read the entire contents of a file as a String. */
  private def contentsOfFile(filename: String): String = {
    val strbuf = new StringBuffer
    val reader = new FileReader(filename)
    val cbuf = new Array[Char](1024)
    while(true) {
      val n = reader.read(cbuf)
      if(n <= 0)
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

    if(!(fileContents.startsWith("#!") || fileContents.startsWith("::#!")))
      return 0

    val matcher =
      (Pattern.compile("^(::)?!#.*(\\r|\\n|\\r\\n)", Pattern.MULTILINE)
              .matcher(fileContents))
    if(! matcher.find)
      throw new Error("script file does not close its header with !# or ::!#")

    return matcher.end
  }

  /** Wrap a script file into a runnable object named
    * scala.scripting.Main .
    */
  private def wrappedScript(filename: String): SourceFile = {
    val preamble =
      new SourceFile("<script preamble>",
          ("package scala.scripting\n" +
          "object Main {\n" +
          "  def main(argv: Array[String]): Unit = {\n" +
          "  val args = argv;\n").toCharArray)

    val middle =
      new SourceFileFragment(
          new SourceFile(new PlainFile(new File(filename))),
          headerLength(filename),
          new File(filename).length.asInstanceOf[Int])

    val end = new SourceFile("<script trailer>", "\n} }\n".toCharArray)

    new CompoundSourceFile(preamble, middle, end)
  }


  /** Compile a script and then run the specified closure with
    * a classpath for the compiled script.
    */
  private def withCompiledScript
        (settings: Settings, scriptFile: String)
        (handler: String=>Unit) =
  {
    val jarFile = jarFileFor(scriptFile)

    def jarOK = (jarFile.canRead &&
      (jarFile.lastModified > new File(scriptFile).lastModified))

    if(jarOK)
    {
      // pre-compiled jar is current
      handler(jarFile.getAbsolutePath)
    } else {
      // The pre-compiled jar is old.  Recompile the script.
      jarFile.delete
      val interpreter = new Interpreter(settings)
      interpreter.beQuiet
      if(interpreter.compileSources(List(wrappedScript(scriptFile)))) {
        tryMakeJar(jarFile, interpreter.classfilePath)
        if(jarOK) {
          // use the jar if possible, so that
          // the interpreter gets closed more reliably
          interpreter.close
          handler(jarFile.getAbsolutePath)
        } else {
          try {
            handler(interpreter.classfilePath.getAbsolutePath)
          } finally {
            interpreter.close
          }
        }
      }
    }
  }

  /** Run a script file with the specified arguments and compilation
    * settings.
    */
  def runScript(
      settings: Settings,
      scriptFile: String,
      scriptArgs: List[String]): Unit =
  {
    withCompiledScript(settings, scriptFile)(compiledLocation => {
      def pparts(path: String) = path.split(File.pathSeparator).toList

      val classpath =
        pparts(settings.bootclasspath.value) :::
        List(compiledLocation) :::
        pparts(settings.classpath.value)

      try {
        ObjectRunner.run(
          classpath,
          "scala.scripting.Main",
          scriptArgs.toArray)
      } catch {
        case e:InvocationTargetException =>
          e.getCause.printStackTrace
      }
    })
  }
}
