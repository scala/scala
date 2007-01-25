/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author Stephane Micheloud
 */
//$Id$

package scala.man1

object fsc extends Command {
  import _root_.scala.tools.docutil.ManPage._

  protected def cn = new Error().getStackTrace()(0).getClassName()
  override def lastModified = "January 18, 2007"

  val name = Section("NAME",

    MBold(command) & " " & NDash & " Fast offline compiler for the " &
    Link("Scala 2", "http://scala-lang.org/") & " language")

  val synopsis = Section("SYNOPSIS",

    CmdLine(" [ " & Argument("options") & " ] " &
            Argument("source files")))

  val parameters = scalac.parameters

  val description = Section("DESCRIPTION",

    "The "&MBold("fsc")&" tool submits Scala compilation jobs to " &
    "a compilation daemon. "&
    "The first time it is executed, the daemon is started automatically. "&
    "On subsequent "&
    "runs, the same daemon can be reused, thus resulting in a faster compilation. "&
    "The tool is especially effective when repeatedly compiling with the same "&
    "class paths, because the compilation daemon can reuse a compiler instance.",

    "The compilation daemon is smart enough to flush its cached compiler "&
    "when the class path changes.  However, if the contents of the class path "&
    "change, for example due to upgrading a library, then the daemon "&
    "should be explicitly shut down with " & MBold("-shutdown") & ".",

    "Note that the "&Link(MBold("scala"),"scala.html")&" script runner "&
    "will also use "&
    "the offline compiler by default, with the sae advantages and caveats.")

  val options = Section("OPTIONS",

      "The offline compiler supports " &
      Link("all options of " & MBold("scalac"), "scalac.html#options") &
      " plus the following:",

      DefinitionList(
        Definition(
          CmdOption("reset"),
          "Reset compile server caches."),
        Definition(
          CmdOption("shutdown"),
          "Shut down the compilation daemon.  The daemon attempts to restart "&
          "itself as necessary, but sometimes an explicit shutdown is required. "&
          "A common example is if jars on the class path have changed."),
        Definition(
          CmdOption("server", Argument("hostname:portnumber")),
          "Specify compile server host at port number.  Usually this option " &
          "is not needed.  Note that the hostname must be for a host that shares " &
          "the same filesystem."),
        Definition(
          CmdOption("J", Argument("flag")),
          "Pass <flag> directly to the Java VM for the compilation daemon.")
    ))

  val example = Section("EXAMPLE",

      "The following session shows a typical speed up due to using the "&
      "offline compiler.",

      CodeSample(
      """> fsc -verbose -d /tmp test.scala
        |...
        |[Port number: 32834]
        |[Starting new Scala compile server instance]
        |[Classpath = ...]
        |[loaded directory path ... in 692ms]
        |...
        |[parsing test.scala]
        |...
        |[total in 943ms]
        |
        |> fsc -verbose -d /tmp test.scala
        |...
        |[Port number: 32834]
        |[parsing test.scala]
        |...
        |[total in 60ms]
        |
        |> fsc -verbose -d /tmp test.scala
        |...
        |[Port number: 32834]
        |[parsing test.scala]
        |...
        |[total in 42ms]
        |
        |> fsc -verbose -shutdown
        |[Scala compile server exited]
        |""".stripMargin))

  val environment = Section("ENVIRONMENT",

    DefinitionList(
      Definition(
        MBold("JAVACMD"),
        "Specify the " & MBold("java") & " command to be used " &
        "for running the Scala code.  Arguments may be specified " &
        "as part of the environment variable; spaces, quotation marks, " &
        "etc., will be passed directly to the shell for expansion.")))


  val exitStatus = Section("EXIT STATUS",

    MBold(command) & " returns a zero exit status if it succeeds to " &
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
    date = lastModified
    author = "Lex Spoon"
    version = "0.2"
    sections = List(
      name,
      synopsis,
      parameters,
      options,
      description,
      example,
      environment,
      exitStatus,
      authors,
      bugs,
      copyright,
      seeAlso)
  }
}
