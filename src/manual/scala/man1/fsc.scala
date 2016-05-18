/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Stephane Micheloud
 */

package scala.man1

/**
 *  @author Lex Spoon
 *  @version 1.0
 */
object fsc extends Command {
  import _root_.scala.tools.docutil.ManPage._

  protected def cn = new Error().getStackTrace()(0).getClassName()

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

    "Note that the " & Link(MBold("scala"), "scala.html") & " script runner " &
    "will also use " &
    "the offline compiler by default, with the same advantages and caveats.")

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
          CmdOptionBound("J", Argument("flag")),
          "Pass " & Mono(Argument("flag")) & " directly to the Java VM for the compilation daemon.")
    ))

  val example = Section("EXAMPLE",

      "The following session shows a typical speed up due to using the "&
      "offline compiler.",

      CodeSample(
      """> fsc -verbose -d /tmp test.scala
        |\&...
        |[Port number: 32834]
        |[Starting new Scala compile server instance]
        |[Classpath = ...]
        |[loaded directory path ... in 692ms]
        |\&...
        |[parsing test.scala]
        |\&...
        |[total in 943ms]
        |
        |> fsc -verbose -d /tmp test.scala
        |\&...
        |[Port number: 32834]
        |[parsing test.scala]
        |\&...
        |[total in 60ms]
        |
        |> fsc -verbose -d /tmp test.scala
        |\&...
        |[Port number: 32834]
        |[parsing test.scala]
        |\&...
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
        "etc., will be passed directly to the shell for expansion."),
      Definition(
        MBold("JAVA_HOME"),
        "Specify JDK/JRE home directory. This directory is used to locate " &
        "the " & MBold("java") & " command unless " & MBold("JAVACMD") & " variable set."),
      Definition(
        MBold("JAVA_OPTS"),
        SeqPara(
          "Specify the options to be passed to the " & MBold("java") &
          " command defined by " & MBold("JAVACMD") & ".",

          "With Java 1.5 (or newer) one may for example configure the " &
          "memory usage of the JVM as follows: " &
          Mono("JAVA_OPTS=\"-Xmx512M -Xms16M -Xss16M\"")
        ))))

  val exitStatus = Section("EXIT STATUS",

    MBold(command) & " returns a zero exit status if it succeeds to " &
    "compile the specified input files. Non zero is returned in case " &
    "of failure.")

  val seeAlso = Section("SEE ALSO",

    Link(Bold("scala") & "(1)", "scala.html") & ", " &
    Link(Bold("scalac") & "(1)", "scalac.html") & ", " &
    Link(Bold("scaladoc") & "(1)", "scaladoc.html") & ", " &
    Link(Bold("scalap") & "(1)", "scalap.html"))

  def manpage = new Document {
    title = command
    date = "March 2012"
    author = "Lex Spoon"
    version = "0.5"
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
