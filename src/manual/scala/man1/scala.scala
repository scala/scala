/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Stephane Micheloud
 */

package scala.man1

/**
 *  @author Stephane Micheloud
 *  @version 1.0
 */
object scala extends Command {
  import _root_.scala.tools.docutil.ManPage._

  protected def cn = new Error().getStackTrace()(0).getClassName()

  val name = Section("NAME",

    MBold(command) & " " & NDash & " Run code in the " &
    Link("Scala 2", "http://scala-lang.org/") &
    " language")

  val synopsis = Section("SYNOPSIS",

    CmdLine(
      " [ " & Argument("option") & " ]... " &
      "[ " & Argument("torun") & " " & Argument("argument") &
      "... ]"))

  val parameters = Section("PARAMETERS",

    DefinitionList(
      Definition(
        Mono(Argument("compiler-option")),
        "Any scalac option.  See " &
        Link(Bold("scalac") & "(1)", "scalac.html") & "."),

      Definition(
        CmdOptionBound("howtorun:", Argument("how")),
        "How to execute " & Argument("torun") & ", if it is present. " &
        "Options for " & Argument("how") & " are " & Mono("guess") &
        " (the default), " & Mono("script") & ", " & Mono("jar") & ", and " & Mono("object") &
        "."),

      Definition(
        CmdOption("i", Argument("file")),
        "Requests that a file be pre-loaded.  It is only " &
        "meaningful for interactive shells."),

      Definition(
        CmdOption("e", Argument("string")),
        "Requests that its argument be executed as Scala code."),

      Definition(
        CmdOption("savecompiled"),
        "Save this compiled version of scripts in order to speed up " &
        "later executions of the same script.  When running a script, " &
        "save the compiled version in a file with the same name as the " &
        "script but with an extension of " & Mono(".jar") & ".  On subsequent " &
        "runs of the same script, the pre-compiled " & Mono(".jar") & " file " &
        "will be used if it is newer than the script file."),

      Definition(
        CmdOption("nocompdaemon"),
        "Do not use the " & MBold("fsc") & " offline compiler."),

      Definition(
        CmdOption("nc"),
        "Same as " & Mono("-nocompdaemon") & "."),

      Definition(
        CmdOptionBound("D", "property=value"),
        "Set a Java system property.  If no value is specified, " &
        "then the property is set to the empty string."),

      Definition(
        Mono(Argument("torun")),
        "A top-level object or a script file to run."),

      Definition(
        Mono(Argument("argument")),
        "An arguments to pass to " & Argument("torun") & ".")))

  val description = Section("DESCRIPTION",

    "The " & MBold(command) & " utility runs Scala code using a Java " &
    "runtime environment.  The Scala code to run is " &
    "specified in one of three ways:",

    NumberedList(
        "With no arguments specified, a Scala shell starts " &
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
    "Scala statements and declarations in the file are processed in order. " &
    "Any arguments specified will be available via the " & Mono("args") &
    "variable.",

    "Script files may have an optional header that is ignored if " &
    "present.  There are two ways to format the header: either beginning with " &
    Mono("#!") & " and ending with " & Mono("!#") & ", or beginning with " &
    Mono("::#!") & " and ending with " & Mono("::!#") & ".",

    "Such a header must have each header boundary start at the beginning of a " &
    "line.  Headers can be used to make stand-alone script files, as shown " &
    "in the examples below.",

    "When running a script or using " & Mono("-e") & ", an already running " &
    "compilation daemon (fsc) is used, or a new one started on demand.  The " &
    Mono("-nocompdaemon") & " or " & Mono("-nc") & " option can be used to " &
    "prevent this.",

    "If " & Mono("scala") & " is run from an sbaz(1) directory, " &
    "then it will add to its classpath any jars installed in the " &
    "lib directory of the sbaz directory.  Additionally, if no " &
    "-classpath option is specified, then " & Mono("scala") &
    " will add " & Quote(".") & ", the current directory, to the " &
    "end of the classpath.")

  val options = Section("OPTIONS",

    "If any compiler options are specified, they must be first in the " &
    "command line and must be followed by a bare hypen (" & Quote("-") &
    ") character. " &
    "If no arguments are specified after the optional compiler arguments, " &
    "then an interactive Scala shell is started.  Otherwise, either a " &
    "script file is run, or a pre-compiled Scala object is run.  It " &
    "is possible to distinguish the last two cases by using an explicit " &
    Mono("-object") & " or " & Mono("-script") & " flag, but usually the " &
    "program can guess correctly.")

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
        CmdLine(CmdOption("classpath", "classes") & "hello.HelloWorld")),

      Definition(
        "Execute a Scala program using JVM options",
        MBold("env JAVACMD") & Mono("=java ") &
        MBold("JAVA_OPTS") & Mono("=\"-Dmsg=hello -enableassertions\" ") &
        CmdLine(CmdOption("classpath", "classes") & "hello.HelloWorld"))),

    "Here is a complete Scala script for Unix: ",

    CodeSample(
      "#!/bin/sh\n" +
      "exec scala \"$0\" \"$@\"\n" +
      "!#\n" +
      "Console.println(\"Hello, world!\")\n" +
      "args.toList foreach Console.println"),

    "Here is a complete Scala script for MS Windows: ",

    CodeSample(
      "::#!\n" +
      "@echo off\n" +
      "call scala %0 %*\n" +
      "goto :eof\n" +
      "::!#\n" +
      "Console.println(\"Hello, world!\")\n" +
      "args.toList foreach Console.println"),

    "If you want to use the compilation cache to speed up multiple executions " +
    "of the script, then add " & Mono("-savecompiled") & " to the scala " +
    "command:",

    CodeSample(
      "#!/bin/sh\n" +
      "exec scala -savecompiled \"$0\" \"$@\"\n" +
      "!#\n" +
      "Console.println(\"Hello, world!\")\n" +
      "args.toList foreach Console.println"))

  val exitStatus = Section("EXIT STATUS",

    "The " & MBold(command) & " command " &
    "returns a zero exit status if it succeeds. " &
    "Non zero is returned in case of any error.  If a script or " &
    "top-level object is executed and returns a value, then that " &
    "return value is passed on to " & MBold(command) & ".")

  val seeAlso = Section("SEE ALSO",

    Link(Bold("fsc") & "(1)", "fsc.html") & ", " &
    Link(Bold("scalac") & "(1)", "scalac.html") & ", " &
    Link(Bold("scaladoc") & "(1)", "scaladoc.html") & ", " &
    Link(Bold("scalap") & "(1)", "scalap.html"))

  def manpage = new Document {
    title = command
    date = "April 2007"
    author = "Stephane Micheloud"
    version = "0.5"
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

