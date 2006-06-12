/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Stephane Micheloud
 */
//$Id: $

package scala.man1

object scalaint extends Command {
  import _root_.scala.tools.docutil.ManPage._

  protected def cn = new Error().getStackTrace()(0).getClassName()

  val name = Section("NAME",

    MBold(command) & " " & NDash & " Interpreter for the " &
    Link("Scala 2", "http://scala.epfl.ch/") & " language")

  val synopsis = Section("SYNOPSIS",

    CmdLine(" [ " & Argument("source file") & " ]"))

  val parameters = Section("PARAMETERS",

    DefinitionList(
      Definition(
        Mono(Argument("source file")),
        "One source file to be interpreted (such as " &
        Mono("MyClass.scala") & ").")))

  val description = Section("DESCRIPTION",

    "The " & MBold(command) & " tool reads class and object definitions, " &
    "written in the Scala programming language, and interprets them in an " &
    "interactive shell environment.",

    "The shell environment provides the following internal commands:",

    CodeSample("This is an interpreter for Scala.\n" +
      "Type in expressions to have them evaluated.\n" +
      "Type :quit to exit the interpreter.\n" +
      "Type :compile followed by a filename to compile a complete Scala file.\n" +
      "Type :load followed by a filename to load a sequence of interpreter commands.\n" +
      "Type :replay to reset execution and replay all previous commands.\n" +
      "Type :help to repeat this message later.\n\n" +
      "scala>"))

  val examples = Section("EXAMPLES",

    DefinitionList(
      Definition(
        "Interpret a Scala program",
        CmdLine("HelloWorld"))))

  val seeAlso = Section("SEE ALSO",

    Link(Bold("sbaz") & "(1)", "sbaz.html") & ", " &
    Link(Bold("scala") & "(1)", "scala.html") & ", " &
    Link(Bold("scalac") & "(1)", "scalac.html") & ", " &
    Link(Bold("scaladoc") & "(1)", "scaladoc.html") & ", " &
    Link(Bold("scalap") & "(1)", "scalap.html") & ", " &
    Link(Bold("scalascript") & "(1)", "scalascript.html"))

  def manpage = new Document {
    title = command
    date = "June 8, 2006"
    author = "Stephane Micheloud"
    version = "0.2"
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
