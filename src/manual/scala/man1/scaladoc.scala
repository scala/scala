/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Stephane Micheloud
 */

package scala.man1

object scaladoc extends Command {
  import _root_.scala.tools.docutil.ManPage._

  protected def cn = new Error().getStackTrace()(0).getClassName()

  val scalaLink = Link("Scala 2", "http://scala-lang.org/")

  val name = Section("NAME",

    MBold(command) & " " & NDash & " Documentation generator for the " &
    scalaLink & " language")

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
    "written in the " & scalaLink & " programming language, and generates " &
    "their API as HTML files.",

    "By default, the generator puts each HTML file in the same directory as " &
    "its source file. You can specify a separate destination directory with " &
    CmdOption("d") & "(see " & Link(Bold("OPTIONS"), "#options") & ", below).",

    // tags are defined in class "scala.tools.nsc.doc.DocGenerator"
    "Supported tag comments are:",

    BulletList(
      Mono("@author"), Mono("@deprecated"),
      Mono("@exception") & " (two arguments)",
      Mono("@param") & " (two arguments)", Mono("@pre"),
      Mono("@return"), Mono("@see"), Mono("@since"),
      Mono("@throws") & " (two arguments)",
      Mono("@todo"), Mono("@version")),

    "See also online document \"" & Link("How to Write Doc Comments for the Javadoc Tool",
    "http://java.sun.com/j2se/javadoc/writingdoccomments/") & "\" from Sun.")

  val options = Section("OPTIONS",

    "The generator has a set of standard options that are supported on the " &
    "current development environment and will be supported in future releases.",

    Section("Standard Options",
      DefinitionList(
        Definition(
          CmdOption("d", Argument("directory")),
          "Specify where to place generated class files."),
        Definition(
          CmdOption("access:<access>"),
          "Show only public, protected/public (default) or all classes " &
          "and members (" & Mono("public") & ",protected,private)"),
        Definition(
          CmdOption("windowtitle", Argument("windowtitle")),
          "Specify window title of generated HTML documentation"),
        Definition(
          CmdOption("doctitle", Argument("doctitle")),
          "Include title for the overview page"),
        Definition(
          CmdOption("stylesheetfile", Argument("stylesheetfile")),
          "File to change style of the generated documentation"),
        Definition(
          CmdOption("header", Argument("pageheader")),
          "Include header text for each page"),
        Definition(
          CmdOption("footer", Argument("pagefooter")),
          "Include footer text for each page"),
        Definition(
          CmdOption("top", Argument("pagetop")),
          "Include top text for each page"),
        Definition(
          CmdOption("bottom", Argument("pagebottom")),
          "Include bottom text for each page"),
        Definition(
          CmdOption("version"),
          "Print product version and exit."),
        Definition(
          /*CmdOption("?") & "| " &*/ CmdOption("help"),
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

    Link(Bold("fsc") & "(1)", "fsc.html") & ", " &
    Link(Bold("sbaz") & "(1)", "sbaz.html") & ", " &
    Link(Bold("scala") & "(1)", "scala.html") & ", " &
    Link(Bold("scalac") & "(1)", "scalac.html") & ", " &
    Link(Bold("scalap") & "(1)", "scalap.html"))

  def manpage = new Document {
    title = command
    date = "May 1, 2007"
    author = "Stephane Micheloud"
    version = "0.4"
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
