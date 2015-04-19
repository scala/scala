package scala.man1

/**
 *  @author Raphael Jolly
 *  @version 1.0
 */
object jarlister extends Command {
  import _root_.scala.tools.docutil.ManPage._

  protected def cn = new Error().getStackTrace()(0).getClassName()

  val name = Section("NAME",

    MBold(command) & " " & NDash & " Jar file lister for the " &
    Link("Scala 2", "http://scala-lang.org/") & " language")

  val synopsis = Section("SYNOPSIS",

    CmdLine(" [ " & Argument("options") & " ] " & Argument("jar name")))

  val parameters = Section("PARAMETERS",

    DefinitionList(
      Definition(
        Mono(Argument("options")),
        "Command line options. See " & Link(Bold("OPTIONS"), "#options") &
        " below."),
      Definition(
        Mono(Argument("jar name")),
        "Name of a jar to be listed (such as " &
        Mono("thirdpartylib.jar") & ").")))

  val description = Section("DESCRIPTION",

    "The " & MBold(command) & " tool lists a jar file's classes " &
    "into its manifest.")

  val options = Section("OPTIONS",

    "The jar lister has the following options.",

    Section("Standard Options",
      DefinitionList(
        Definition(
          CmdOption("help"),
          "Display this usage message."),
        Definition(
          CmdOption("o"),
          "Specify output jar file."))))

  val examples = Section("EXAMPLES",

    DefinitionList(
      Definition(
        "List jar's classes into manifest",
        CmdLine("thirdpartylib.jar"))))

  val exitStatus = Section("EXIT STATUS",

    MBold(command) & " returns a zero exist status if it succeeds to process " &
    "the specified input file. Non zero is returned in case of failure.")

  override val authors = Section("AUTHOR",

    "Written by Raphael Jolly.")

  val seeAlso = Section("SEE ALSO",

    Link(Bold("fsc") & "(1)", "fsc.html") & ", " &
    Link(Bold("scala") & "(1)", "scala.html") & ", " &
    Link(Bold("scalac") & "(1)", "scalac.html") & ", " &
    Link(Bold("scaladoc") & "(1)", "scaladoc.html"))

  def manpage = new Document {
    title = command
    date = "February 2013"
    author = "Raphael Jolly"
    version = "1.0"
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
