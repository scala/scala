/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.io.{BufferedReader, FileReader, File}
import scala.tools.nsc.util._
import scala.tools.nsc.io._

/** A main routine to support putting Scala code into scripts.
 *
 *  An shell script example on Unix would look like this:
 *
 *    #!/bin/sh
 *    exec scalascript "$0" "$@"
 *    !#
 *    Console.println("Hello, world!")
 *    argv.toList foreach Console.println
 *
 * A batch file example on Windows XP would look like this:
 *
 *    ::#!
 *    @echo off
 *    call scalascript %0 %*
 *    goto :eof
 *    ::!#
 *    Console.println("Hello, world!")
 *    argv.toList foreach Console.println
 *
 * TODO: It would be better if error output went to stderr instead
 * of stdout....
 */
object MainScript {
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
    * and ends with a line that begins with "!#" ar "::!#".
    */
  def headerLength(filename: String): Int = {
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

  /** Print a usage message and then exit. */
  def usageExit: Nothing = {
    Console.println(
        "scalascript [ compiler arguments... - ] scriptfile " +
        "[ script arguments... ]")
    Console.println
    Console.println(
        "Note that if you do specify compiler arguments, you \n" +
        "must put an explicit hyphen (\"-\") at the end of them.")
    System.exit(1).asInstanceOf[Nothing]
  }

  /** Parse an argument list into three portions:
    *
    *   Arguments to the compiler
    *   The filename of the script to run
    *   Arguments to pass to the script
    */
  def parseArgs(args: List[String])
      :Tuple3[List[String], String, List[String]] =
  {
    if (args.length == 0)
      usageExit

    if (args(0).startsWith("-")) {
      // the line includes compiler arguments
      val hyphenIndex = args.indexOf("-")
      if (hyphenIndex < 0)
        usageExit
      if (hyphenIndex == (args.length - 1))
        usageExit

      Tuple3(
        args.subseq(0, hyphenIndex).toList,
        args(hyphenIndex + 1),
        args.subseq(hyphenIndex + 2, args.length - hyphenIndex - 2).toList)
    } else {
      Tuple3(
        Nil,
        args(0),
        args.subseq(1, args.length-1).toList)
    }
  }


  def wrappedScript(filename: String): SourceFile = {
    val preamble =
      new SourceFile("<script preamble>",
          ("package scala.scripting\n" +
          "object Main {\n" +
          "  def main(argv: Array[String]): Unit = {\n").toCharArray)

    val middle =
      new SourceFileFragment(
          new SourceFile(new PlainFile(new File(filename))),
          headerLength(filename),
          new File(filename).length.asInstanceOf[Int])

    val end = new SourceFile("<script trailer>", "\n} }\n".toCharArray)

    new CompoundSourceFile(preamble, middle, end)
  }


  def main(args: Array[String]): Unit = {
    val parsedArgs = parseArgs(args.toList)
    val compilerArgs = parsedArgs._1
    val scriptFile = parsedArgs._2
    val scriptArgs = parsedArgs._3.toArray

    val command =
      new CompilerCommand(
        compilerArgs,
        Console.println,
        false)

    if (!command.ok || command.settings.help.value) {
      // either the command line is wrong, or the user
      // explicitly requested a help listing
      if (!command.ok)
        Console.println
      usageExit
    }


    val interpreter = new Interpreter(command.settings)
    interpreter.beQuiet

    if(!interpreter.compileSources(List(wrappedScript(scriptFile))))
      return () // compilation error
    interpreter.bind("argv", "Array[String]", scriptArgs)
    interpreter.interpret("scala.scripting.Main.main(argv)")
  }

}
