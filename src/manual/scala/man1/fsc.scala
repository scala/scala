/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Stephane Micheloud
 */
//$Id: $

package scala.man1

object fsc extends Command {
  import _root_.scala.tools.docutil.ManPage._

  protected def cn = new Error().getStackTrace()(0).getClassName()

  val name = Section("NAME",

    MBold(command) & " " & NDash & " Offline compiler for the " &
    Link("Scala 2", "http://scala.epfl.ch/") & " language")

  val synopsis = Section("SYNOPSIS",

    CmdLine(" [ " & Argument("options") & " ] " &
            Argument("source files")))

  val parameters = scalac.parameters

  val description = Section("DESCRIPTION",

    "The " & MBold(command) & " tool reads class and object definitions, " &
    "written in the Scala programming language, and compiles them into " &
    "bytecode class files.",

    "By default, the compiler puts each class file in the same directory " &
    "as its source file. You can specify a separate destination directory " &
    "with -d (see " & Link(Bold("OPTIONS"), "#options") & ", below).")

  val options =
    Section("Standard Options",
      DefinitionList(
        Definition(
          CmdOption("reset"),
          "Reset compile server caches"),
        Definition(
          CmdOption("shutdown"),
          "Shutdown compile server"),
        Definition(
          CmdOption("server", Argument("hostname:portnumber")),
          "Specify compile server host at port number.  Usually this option " &
          "is not needed.  Note that the hostname must be for a host that shares " &
          "the same filesystem."),
        Definition(
          CmdOption("J", Argument("flag")),
          "Pass <flag> directly to runtime system")
    ))

  val environment = Section("ENVIRONMENT",

    DefinitionList(
      Definition(
        MBold("JAVACMD"),
        "Specify the " & MBold("java") & " command to be used " &
        "for running the Scala commands")))

  val examples = Section("EXAMPLES",

    DefinitionList(
      Definition(
        "Compile a Scala program to the current directory",
        CmdLine("HelloWorld")),
      Definition(
        "Compile a Scala program to the destination directory " &
        MBold("classes"),
        CmdLine(CmdOption("d", "classes") & "HelloWorld.scala")),
     Definition(
        "Compile a Scala program using a user-defined " & MBold("java") & " " &
        "command",
        MBold("env JAVACMD") & Mono("=/usr/local/bin/cacao ") &
        CmdLine(CmdOption("d", "classes") & "HelloWorld.scala")),
      Definition(
        "Compile all Scala files found in the source directory " &
        MBold("src") & " to the destination directory " &
        MBold("classes"),
        CmdLine(CmdOption("d", "classes") & "src/*.scala"))))

  val exitStatus = Section("EXIT STATUS",

    MBold(command) & " returns a zero exist status if it succeeds to " &
    "compile the specified input files. Non zero is returned in case " &
    "of failure.")

  val seeAlso = Section("SEE ALSO",

    Link(Bold("sbaz") & "(1)", "sbaz.html") & ", " &
    Link(Bold("scala") & "(1)", "scala.html") & ", " &
    Link(Bold("scalac") & "(1)", "scalac.html") & ", " &
    Link(Bold("scaladoc") & "(1)", "scaladoc.html") & ", " &
    Link(Bold("scalap") & "(1)", "scalap.html"))

  def manpage = new Document {
    title = command
    date = lastModified // e.g. "June 8, 2006"
    author = "Stephane Micheloud"
    version = "0.1"
    sections = List(
      name,
      synopsis,
      parameters,
      options,
      environment,
      examples,
      exitStatus,
      authors,
      bugs,
      copyright,
      seeAlso)
  }
}
