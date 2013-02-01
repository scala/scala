/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Stephane Micheloud
 */

package scala.man1

/**
 *  @author Gilles Dubochet
 *  @version 1.0
 */
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
    "The recognised format of comments in source is described in the " & Link("online documentation",
    "https://wiki.scala-lang.org/display/SW/Scaladoc"))

  val options = Section("OPTIONS",

    Section("Standard Options",
      DefinitionList(
        Definition(
          CmdOption("d", Argument("directory")),
          "Specify where to generate documentation."),
        Definition(
          CmdOption("version"),
          "Print product version and exit."),
        Definition(
          /*CmdOption("?") & "| " &*/ CmdOption("help"),
          "Print a synopsis of available options."))),

    Section("Documentation Options",
      DefinitionList(
        Definition(
          CmdOption("doc-title", Argument("title")),
          "Define the overall title of the documentation, typically the name of the library being documented."),
        Definition(
          CmdOption("doc-version", Argument("version")),
          "Define the overall version number of the documentation, typically the version of the library being documented."),
        Definition(
          CmdOption("doc-source-url", Argument("url")),
          "Define a URL to be concatenated with source locations for link to source files."),
        Definition(
          CmdOption("doc-external-doc", Argument("external-doc")),
          "Define a comma-separated list of classpath_entry_path#doc_URL pairs describing external dependencies."))),

    Section("Compiler Options",
      DefinitionList(
        Definition(
          CmdOption("verbose"),
          "Output messages about what the compiler is doing"),
        Definition(
          CmdOption("deprecation"),
          SeqPara(
            "Indicate whether source should be compiled with deprecation " &
            "information; defaults to " & Mono("off") & " (" &
            "accepted values are: " & Mono("on") & ", " & Mono("off") &
            ", " & Mono("yes") & " and " & Mono("no") & ")",
            "Available since Scala version 2.2.1")),
        Definition(
          CmdOption("classpath", Argument("path")),
          SeqPara(
            "Specify where to find user class files (on Unix-based systems " &
            "a colon-separated list of paths, on Windows-based systems, a " &
            "semicolon-separate list of paths). This does not override the " &
            "built-in (" & Mono("\"boot\"") & ") search path.",
            "The default class path is the current directory. Setting the " &
            Mono("CLASSPATH") & " variable or using the " & Mono("-classpath") & " " &
            "command-line option overrides that default, so if you want to " &
            "include the current directory in the search path, you must " &
            "include " & Mono("\".\"") & " in the new settings.")),
        Definition(
          CmdOption("sourcepath", Argument("path")),
          "Specify where to find input source files."),
        Definition(
          CmdOption("bootclasspath", Argument("path")),
          "Override location of bootstrap class files (where to find the " &
          "standard built-in classes, such as \"" & Mono("scala.List") & "\")."),
        Definition(
          CmdOption("extdirs", Argument("dirs")),
          "Override location of installed extensions."),
        Definition(
          CmdOption("encoding", Argument("encoding")),
          SeqPara(
            "Specify character encoding used by source files.",
            "The default value is platform-specific (Linux: " & Mono("\"UTF8\"") &
            ", Windows: " & Mono("\"Cp1252\"") & "). Executing the following " &
            "code in the Scala interpreter will return the default value " &
            "on your system:",
            MBold("    scala> ") &
            Mono("new java.io.InputStreamReader(System.in).getEncoding"))))))

  val exitStatus = Section("EXIT STATUS",

    MBold(command) & " returns a zero exit status if it succeeds at processing " &
    "the specified input files. Non zero is returned in case of failure.")

  override val authors = Section("AUTHORS",

    "This version of Scaladoc was written by Gilles Dubochet with contributions by Pedro Furlanetto and Johannes Rudolph. " &
    "It is based on the original Scaladoc (Sean McDirmid, Geoffrey Washburn, Vincent Cremet and Stéphane Micheloud), " &
    "on vScaladoc (David Bernard), as well as on an unreleased version of Scaladoc 2 (Manohar Jonnalagedda).")

  val seeAlso = Section("SEE ALSO",

    Link(Bold("fsc") & "(1)", "fsc.html") & ", " &
    Link(Bold("scala") & "(1)", "scala.html") & ", " &
    Link(Bold("scalac") & "(1)", "scalac.html") & ", " &
    Link(Bold("scalap") & "(1)", "scalap.html"))

  def manpage = new Document {
    title = command
    date = "June 2010"
    author = "Gilles Dubochet"
    version = "2.0"
    sections = List(
      name,
      synopsis,
      parameters,
      description,
      options,
      exitStatus,
      authors,
      seeAlso)
  }
}
