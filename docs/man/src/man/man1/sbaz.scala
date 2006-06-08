/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Stephane Micheloud
 */
//$Id$

package man.man1

object sbaz extends Command {
  import ManPage._

  protected val cn = new Error().getStackTrace()(0).getClassName()

  val name = Section("NAME",

    MBold(command) & " " & NDash & " Scala package sharing tool for the " &
    Link("Scala 2", "http://scala.epfl.ch/") & " language")

  val synopsis = Section("SYNOPSIS",

    CmdLine(" [ " & Argument("global_options") & " ] " & Argument("command") &
            " [ " & Argument("command_options") & "]"))

  val parameters = Section("PARAMETERS",

    DefinitionList(
      Definition(
        Mono(Argument("global_options")),
        "Command line options. See " & Link(Bold("OPTIONS"), "#options") &
        " below."),
      Definition(
        Mono(Argument("command")),
        "Internal " & MBold(command) & " command."),
      Definition(
        Mono(Argument("command_options")),
        MBold(command) & " command options.")))

  val description = Section("DESCRIPTION",

    "The " & MBold(command) & " tool ...")

  val options = Section("OPTIONS",

    "The " & MBold(command) & " tool has a set of standard options that are supported " &
    "on the current development environment and will be supported in future releases.",

    Section("Global Options",
      DefinitionList(
        Definition(
          CmdOption("d", Argument("dir")),
          "Operate on dir as the local managed directory."),
        Definition(
          CmdOption("n"),
          "Do not actually do anything.  Only print out what " +
          "tool would normally do with the following arguments."))),

    Section("Available Commands",
      DefinitionList(
        Definition(
          MBold("available"),
          "List the available packages for installation."),
        Definition(
          MBold("compact"),
          "Clear the download cache to save space."),
        Definition(
          MBold("help"),
          "Display a help message."),
        Definition(
          MBold("install"),
          "Install a package."),
        Definition(
          MBold("installed"),
          "List the packages that are installed."),
        Definition(
          MBold("keycreate"),
          "Request that a new key be created."),
        Definition(
          MBold("keyforget"),
          "Forget the specified key."),
        Definition(
          MBold("keyknown"),
          "List all known keys."),
        Definition(
          MBold("keyremember"),
          "Remember the specified key for future use."),
        Definition(
          MBold("keyremoteknown"),
          "List all keys known to the bazaar server."),
        Definition(
          MBold("keyrevoke"),
          "Request that a specified key be revoked."),
        Definition(
          MBold("remove"),
          "Remove a package."),
        Definition(
          MBold("retract"),
          "Retract a previously shared package."),
        Definition(
          MBold("setuniverse"),
          "Set the universe for a directory."),
        Definition(
          MBold("setup"),
          "Initialize a directory to be managed."),
        Definition(
          MBold("share") & " filename",
          "Share a package advertisement on a bazaar."),
        Definition(
          MBold("share") & " " & CmdOption("i", Argument("descriptor")),
          "The package advertisement is usually specified in a file, " &
          "but it may also be specified on the command line with the " &
          CmdOption("i") & " option."),
        Definition(
          MBold("share") & " " & CmdOption("-template"),
          "If " & CmdOption("-template") & " is specified, then instead " &
          "of uploading a description, the command prints out a template " &
          "of a package advertisement."),
        Definition(
          MBold("show"),
          "Show information about one package."),
        Definition(
          MBold("update"),
          "Update the list of available packages."),
        Definition(
          MBold("upgrade"),
          "Upgrade all possible packages."))))

  val examples = Section("EXAMPLES",

    DefinitionList(
      Definition(
        "Update the list of available packages.",
        CmdLine(MBold("update"))),
      Definition(
        "Upload package description for " & Mono("scala-devel-2.1.5") &
        " to the universe",
        CmdLine(MBold("share") & " scala-devel-2.1.5.advert"))))

  val exitStatus = Section("EXIT STATUS",

    MBold(command) & " returns a zero exist status if it succeeds to process " &
    "the specified input files. Non zero is returned in case of failure.")

  override val authors = Section("AUTHOR",

    "Written by Lex Spoon.")

  val seeAlso = Section("SEE ALSO",

    Link("scala(1)", "scala.html") & ", " &
    Link("scalac(1)", "scalac.html") & ", " &
    Link("scaladoc(1)", "scaladoc.html") & ", " &
    Link("scalaint(1)", "scalaint.html") & ", " &
    Link("scalascript(1)", "scalascript.html"))

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
      options,
      examples,
      exitStatus,
      authors,
      bugs,
      copyright,
      seeAlso)
  }
}
