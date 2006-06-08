/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Stephane Micheloud
 */
//$Id$

package man.man1

object scaladoc extends Command {
  import ManPage._

  protected val cn = new Error().getStackTrace()(0).getClassName()

  val name = Section("NAME",

    MBold(command) & " " & NDash & " Documentation generator for the " &
    Link("Scala 2", "http://scala.epfl.ch/") & " language")

  val synopsis = Section("SYNOPSIS",

    CmdLine(" [ " & Argument("options") & " ] " & Argument("source files")))

  val parameters = Section("PARAMETERS",

    DefinitionList(
      Definition(
        Mono(Argument("options")),
        "Command line options. See " & Link(Bold("OPTIONS"), "#options") &
        " below."),
      Definition(
        Mono(Argument("source files")),
        "One or more source files to be compiled (such as " &
        Mono("MyClass.scala") & ").")))

  val description = Section("DESCRIPTION",

    "The " & MBold(command) & " tool reads class and object definitions, " &
    "written in the Scala programming language, and generates their API as " &
    "HTML files.",

    "By default, the generator puts each HTML file in the same directory as " &
    "its source file. You can specify a separate destination directory with " &
    CmdOption("d") & "(see " & Link(Bold("OPTIONS"), "#options") & ", below).")

  val options = Section("OPTIONS",

    "The generator has a set of standard options that are supported on the " &
    "current development environment and will be supported in future releases.",

    Section("Standard Options",
      DefinitionList(
        Definition(
          CmdOption("d", Argument("directory")),
          "Specify where to place generated class files."),
        Definition(
          CmdOption("version"),
          "Print product version and exit."),
        Definition(
          CmdOption("?") & "| " & CmdOption("help"),
          "Print a synopsis of standard options."))))

  val examples = Section("EXAMPLES",

    DefinitionList(
      Definition(
        "Generate documentation for a Scala program",
        CmdLine("HelloWorld.scala")),
      Definition(
        "Generation documentation for a Scala program to the destination " &
        "directory " & Bold("classes"),
        CmdLine(CmdOption("d", "api") & "HelloWorld.scala")),
      Definition(
        "Generate documentation for all Scala files found in the source " &
        "directory " & Bold("src") & " to the destination directory " &
        Bold("api"),
        CmdLine(CmdOption("d", "api") & "src/*.scala"))))

  val exitStatus = Section("EXIT STATUS",

    MBold(command) & " returns a zero exist status if it succeeds to process " &
    "the specified input files. Non zero is returned in case of failure.")

  val seeAlso = Section("SEE ALSO",

    Link(Bold("sbaz") & "(1)", "sbaz.html") & ", " &
    Link(Bold("scala") & "(1)", "scala.html") & ", " &
    Link(Bold("scalac") & "(1)", "scalac.html") & ", " &
    Link(Bold("scalaint") & "(1)", "scalaint.html") & ", " &
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
      options,
      examples,
      exitStatus,
      authors,
      bugs,
      copyright,
      seeAlso)
  }
}
