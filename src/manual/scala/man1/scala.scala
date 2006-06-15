/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Stephane Micheloud
 */
//$Id$

package scala.man1

object scala extends Command {
  import _root_.scala.tools.docutil.ManPage._

  protected def cn = new Error().getStackTrace()(0).getClassName()

  val name = Section("NAME",

    MBold(command) & " " & NDash & " Run code in the " &
    Link("Scala 2", "http://scala.epfl.ch/") &
    " language")

  val synopsis = Section("SYNOPSIS",

    CmdLine(
      " [ " & Argument("compiler-option") & " | " &
      Mono("-howtorun:") & Argument("how") & " ]... " &
      "[ " & Argument("torun") & " " & Argument("argument") &
      "... ]"))

  val parameters = Section("PARAMETERS",

    DefinitionList(
      Definition(
        Mono(Argument("compiler-option")),
        "Options for the compiler.  See " &
        Link(Bold("scalac") & "(1)", "scalac.html") & "."),

      Definition(
        Mono("-howtorun:") & Argument("how"),
        "How to execute " & Argument("torun") & ", if it is present. " &
        "Options for " & Argument("how") & " are " & Mono("guess") &
        " (the default), " & Mono("script") & ", and " & Mono("object") &
        "."),

      Definition(
        Mono(Argument("torun")),
        "A top-level object or a script file to run."),

      Definition(
        Mono(Argument("argument")),
        "An arguments to pass to " & Argument("torun") & ".")))

  val description = Section("DESCRIPTION",

    "The "&MBold(command)&" utility runs Scala code using a Java runtime "&
    "environment.  The Scala code to run is " &
    "specified in one of three ways:",

    NumberedList(
        "With no arguments specified, an interactive interpreter starts " &
        "and reads commands interactively.",

        "With " & Mono("-howtorun:object") & " specified, the fully " &
        "qualified name of a top-level " &
        "Scala object may be specified.  The object should previously have " &
        "been compiled using " & Link(Bold("scalac") & "(1)", "scalac.html") &
        ".",

        "With " & Mono("-howtorun:script") & " specified, a file " &
        "containing Scala code may be specified."
        ),

    "If " & Mono("-howtorun:") & " is left as the default (" & Mono("guess") &
    "), then the " & MBold(command) & " command " &
    "will check whether a file of the " &
    "specified name exists.  If it does, then it will treat it as a " &
    "script file; if it does not, then it will treat it as the name " &
    "of an object.",

    "In all three cases, arbitrary scalac options may be specified. "&
    "The most common option is to specify a classpath with " &
    Mono("-classpath") & ", but see the " &
    Link(Bold("scalac") & "(1)", "scalac.html") & " page for " &
    "full details.   ",


    "If an object is specified to run, then that object must be a top-level " &
    "Scala object with the specified name.  The object must define a method " &
    Bold("main") & " with the following signature:",

    BlockQuote(Mono(Bold("def") & " main(args: Array[String]): Unit")),

    "The method must return a " & Bold("Unit") & " value, and it must " &
    "accept a " & Bold("String") & " array as a parameter.  All arguments " &
    "specified on the command line will be passed as " &
    "arguments to the " & Bold("main") & " method.",

    "If a script file is specified to run, then the file is read and all " &
    "Scala statements and declarations in the file are processed in order. ",

    "Script files may have an optional header fthat is ignored if " &
    "present.  There are two ways to format the header: either beginning with " &
    Mono("#!") & " and ending with " & Mono("!#") & ", or beginning with " &
    Mono("::#!") & " and ending with " & Mono("::!#") & ".",

    "Such a header must have each header boundary start at the beginning of a " &
    "line.  Headers can be used to make stand-alone script files, as shown " &
    "in the examples below.")


  val options = Section("OPTIONS",

    "If any compiler options are specified, they must be first in the " &
    "command line and must be followed by a bare hypen (" & Quote("-") &
    ") character. " &
    "If no arguments are specified after the optional compiler arguments, " &
    "then an interactive interpreter is started.  Otherwise, either a " &
    "script file is run, or a pre-compiled Scala object is run.  It " &
    "is possible to distinguish the last two cases by using an explicit " &
    Mono("-object") & " or " & Mono("-script") & " flag, but usually the " &
    "program can guess correctly.")

  val environment = Section("ENVIRONMENT",

    DefinitionList(
      Definition(
        MBold("JAVACMD"),
        "Specify the " & MBold("java") & " command to be used " &
        "for running the Scala commands")))

  val examples = Section("EXAMPLES",

    "Here are some examples of running Scala code:",

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
        CmdLine(CmdOption("classpath", "classes") & "hello.HelloWorld"))),

    "Here is a complete Scala script for Unix: ",

    CodeSample(
      "#!/bin/sh\n" +
      "exec scalascript \"$0\" \"$@\"\n" +
      "!#\n" +
      "Console.println(\"Hello, world!\")\n" +
      "argv.toList foreach Console.println"),

    "Here is a complete Scala script for Unix: ",

    CodeSample(
      "::#!\n" +
      "@echo off\n" +
      "call scalascript %0 %*\n" +
      "goto :eof\n" +
      "::!#\n" +
      "Console.println(\"Hello, world!\")\n" +
      "argv.toList foreach Console.println"))


  val exitStatus = Section("EXIT STATUS",

    "The " & MBold(command) & " command " &
    "returns a zero exit status if it succeeds. " &
    "Non zero is returned in case of any error.  If a script or " &
    "top-level object is executed and returns a value, then that " &
    "return value is passed on to " & MBold(command) & ".")

  val seeAlso = Section("SEE ALSO",

    Link(Bold("sbaz") & "(1)", "sbaz.html") & ", " &
    Link(Bold("scalac") & "(1)", "scalac.html") & ", " &
    Link(Bold("scaladoc") & "(1)", "scaladoc.html") & ", " &
    Link(Bold("scalap") & "(1)", "scalap.html"))

  def manpage = new Document {
    title = command
    date = lastModified
    author = "LAMP"
    version = "0.3"
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

