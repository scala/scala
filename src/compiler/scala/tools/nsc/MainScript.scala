package scala.tools.nsc
import java.io.{BufferedReader,FileReader}

/** A main routine to support putting Scala code into scripts.  An example
 *  script would look like this:
 *
 *    #!/bin/sh
 *    scalascript $0 "$@"
 *    !#
 *    Console.println("Hello, world!")
 *
 *
 * TODO: It would be better if error output went to stderr instead
 * of stdout....
 */
object MainScript {
  /** Read the contents of the specified file, skipping the header
    * part if there is one.  The header part starts with "#!"
    * and ends with a line that begins with "!#".
    */
  def readFileSkippingHeader(filename: String): String = {
    val file =
      new BufferedReader(
          new FileReader(filename))
    val contents = new StringBuffer

    // skip the header, if there is one
    val firstLine = file.readLine
    if(firstLine == null)
      return ""
    else if(firstLine.startsWith("#!")) {
      // skip until !# is seen
      def lp:Unit = {
        val line = file.readLine
        if(line == null)
          ()
        else if(!line.startsWith("!#"))
          lp
      }
      lp
    }
    else
      contents.append(firstLine)

    // now read the rest of the file
    def lp: Unit = {
      val line = file.readLine
      if(line == null)
        return ()
      contents.append(line)
      contents.append("\n")
      lp
    }
    lp

    contents.toString
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
    if(args.length == 0)
      usageExit

    if(args(0).startsWith("-")) {
      // the line includes compiler arguments
      val hyphenIndex = args.indexOf("-")
      if(hyphenIndex < 0)
        usageExit
      if(hyphenIndex == (args.length - 1))
        usageExit

      Tuple3(
        args.subseq(0, hyphenIndex).toList,
        args(hyphenIndex+1),
        args.subseq(hyphenIndex+2, args.length - hyphenIndex - 2).toList)
    } else {
      Tuple3(
        Nil,
        args(0),
        args.subseq(1, args.length-1).toList)
    }
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
      if(!command.ok)
        Console.println
      usageExit
    }

    val scriptContents = readFileSkippingHeader(scriptFile)
    val toRun =
      "package scala.scripting\n" +
      "object Main {\n" +
      "  def main(argv: Array[String]): Unit = {\n" +
      scriptContents +
      "\n} }\n"

    val interpreter = new Interpreter(command.settings)
    interpreter.beQuiet
    if(!interpreter.compileString(toRun))
      return () // compilation error
    interpreter.bind("argv", "Array[String]", scriptArgs)
    interpreter.interpret("scala.scripting.Main.main(argv)")
  }
}
