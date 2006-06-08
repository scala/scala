/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Stephane Micheloud
 */
//$Id$

package man.man1

object scala extends Command {
  import ManPage._

  protected val cn = new Error().getStackTrace()(0).getClassName()

  val name = Section("NAME",

    MBold(command) & " " & NDash & " Launcher for the " &
    Link("Scala 2", "http://scala.epfl.ch/") & " language")

  val synopsis = Section("SYNOPSIS",

    CmdLine(" [ " & Argument("options") & " ] " &
            Argument("class file") & " [ " & Argument("args") & " ]"))

  val parameters = Section("PARAMETERS",

    DefinitionList(
      Definition(
        Mono(Argument("options")),
        "Command line options. See " & Link(Bold("OPTIONS"), "#options") &
        " below."),
      Definition(
        Mono(Argument("class file")),
        "Name of the class to be invoked."),
      Definition(
        Mono(Argument("args")),
        "Program arguments passed to the main function.")))

  val description = Section("DESCRIPTION",

    "The " & MBold(command) & " utility launches a Scala application. " &
    "It does this by starting a Java runtime environment, loading a " &
    "specified class, and invoking that class's " & Bold("main") &
    " method. The method must have the following signature:",

    BlockQuote(Mono(Bold("def") & " main(args: Array[String]): Unit")),

    "The method must return a " & Bold("Unit") & " value, and it must " &
    "accept a " & Bold("String") & " array as a parameter. By default, " &
    "the first non-option argument is the name of the class to be invoked. "&
    "A fully-qualified class name should be used.",

    "The Scala runtime searches for the startup class, and other classes " &
    "used, in three sets of locations: the bootstrap class path, the " &
    "installed extensions, and the user class path.")

  val options = Section("OPTIONS",

    "The launcher has a set of standard options that are supported on the " &
    "current runtime environment and will be supported in future releases. " &
    "An additional set of non-standard options are specific to the current " &
    "virtual machine implementation and are subject to change in the future. " &
    "Non-standard options begin with " & CmdOption("X") & ".",

    Section("Standard Options",
      DefinitionList(
        Definition(
          CmdOption("cp") & "| " & CmdOption("classpath", Argument("path")),
          "Specify where to find user class files (on Unix-based systems " &
          "a colon-separated list of paths, on Windows-based systems, a " &
          "semicolon-separate list of paths)."),
        Definition(
          CmdOption("D", Argument("name") & "=" & Argument("value")),
          "Set a system property."),
        Definition(
          CmdOption("verbose", "[:class|gc|jni]"),
          "Enable verbose output."),
        Definition(
          CmdOption("showversion"),
          "Print product version and continue."),
        Definition(
          CmdOption("version"),
          "Print product version and exit."),
        Definition(
          CmdOption("help"),
          "Print this help message."))),

    Section("Non-Standard Options",
      "Same options as the " & MBold("java") & " command."))

  val environment = Section("ENVIRONMENT",

    DefinitionList(
      Definition(
        MBold("JAVACMD"),
        "Specify the " & MBold("java") & " command to be used " &
        "for running the Scala commands")))

  val examples = Section("EXAMPLES",

    DefinitionList(
      Definition(
        "Execute a Scala program generated in the current directory",
        CmdLine("hello.HelloWorld")),
      Definition(
        "Execute a Scala program generated in a user-defined " &
        "directory " & Bold("classes"),
        CmdLine(CmdOption("classpath", "classes") & "hello.HelloWorld")),
      Definition(
        "Execute a Scala program using a user-defined " & MBold("java") & " " &
        "command",
        MBold("env JAVACMD") & Mono("=/usr/local/bin/cacao ") &
        CmdLine(CmdOption("classpath", "classes") & "hello.HelloWorld"))))

  val exitStatus = Section("EXIT STATUS",

    MBold(command) & " returns a zero exit status if it succeeds. " &
    "Non zero is returned in case of failure.")

  val seeAlso = Section("SEE ALSO",

    Link(Bold("sbaz") & "(1)", "sbaz.html") & ", " &
    Link(Bold("scalac") & "(1)", "scalac.html") & ", " &
    Link(Bold("scaladoc") & "(1)", "scaladoc.html") & ", " &
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
      environment,
      examples,
      exitStatus,
      authors,
      bugs,
      copyright,
      seeAlso)
  }
}

