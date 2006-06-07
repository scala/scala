/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Stephane Micheloud
 */
//$Id: $

package man.man1

object scalascript extends Command {
  import ManPage._

  protected val cn = new Error().getStackTrace()(0).getClassName()

  val name = Section("NAME",

    MBold(command) & " " & NDash & " Script runner for the " &
    Link("Scala 2", "http://scala.epfl.ch/") & " language")

  val synopsis = Section("SYNOPSIS",

   CmdLine(" [ " & Argument("compiler args...") & " - ] " &
           Argument("scriptfile") & " [ " & Argument("script args...") & " ]"))

  val parameters = Section("PARAMETERS",

    DefinitionList(
      Definition(
        Mono(Argument("compiler args")),
        "Compiler arguments, exactly as for " & MBold("scalac") & ". " &
        "The compiler arguments, if present, must be terminated by a " &
        "bare hyphen."),
      Definition(
        Mono(Argument("scriptfile")),
        "One source file to be interpreted."),
      Definition(
        Mono(Argument("script args")),
        "Arguments to be passed to the script. They will be available " &
        "via the " & Mono("argv") & " variable.")))

  val description = Section("DESCRIPTION",

    "The " & MBold(command) & " tool supports writing script files " &
    "in Scala. To write a Scala script on Unix, start the file with the " &
    "following header:",

    CodeSample(
      "#!/bin/sh\n" +
      "exec scalascript \"$0\" \"$@\"\n" +
      "!#\n"),

    "To write a Scala script as a Microsoft Windows batch file, start " &
    "the " & Mono(".bat") & " file with the following header:",

    CodeSample(
      "::#!\n" +
      "@echo off\n" +
      "call scalascript %0 %*\n" +
      "goto :eof\n" +
      "::!#\n"))

  val examples = Section("EXAMPLES",

    "Here is a complete Scala script for Unix that prints out a " &
    "friendly greeting followed by all of the script's arguments:",

    CodeSample(
      "#!/bin/sh\n" +
      "exec scalascript \"$0\" \"$@\"\n" +
      "!#\n" +
      "Console.println(\"Hello, world!\")\n" +
      "argv.toList foreach Console.println\n"))

  override val authors = Section("AUTHOR",

    "Written by Lex Spoon.")

  val seeAlso = Section("SEE ALSO",

    Link(Bold("scala") & "(1)", "scala.html") & ", " &
    Link(Bold("scalac") & "(1)", "scalac.html") & ", " &
    Link(Bold("scaladoc") & "(1)", "scaladoc.html") & ", " &
    Link(Bold("scalaint") & "(1)", "scalaint.html"))

  def manpage = new Document {
    title = command
    date = "April 29, 2005"
    author = "Stephane Micheloud"
    version = "0.1"
    sections = List(
      name,
      synopsis,
      parameters,
      description,
      examples,
      authors,
      bugs,
      copyright,
      seeAlso)
  }
}
