/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Stephane Micheloud
 */

package scala.man1

/**
 *  @author Stephane Micheloud
 */
object scalac extends Command {
  import _root_.scala.tools.docutil.ManPage._

  protected def cn = new Error().getStackTrace()(0).getClassName()

  val name = Section("NAME",

    MBold(command) & " " & NDash & " Compiler for the " &
    Link("Scala 2", "http://scala-lang.org/") & " language")

  val synopsis = Section("SYNOPSIS",

    CmdLine(" [ " & Argument("options") & " ] " &
            Argument("source files")))

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
    "written in the Scala programming language, and compiles them into " &
    "bytecode class files.",

    "By default, the compiler puts each class file in the same directory " &
    "as its source file. You can specify a separate destination directory " &
    "with -d (see " & Link(Bold("OPTIONS"), "#options") & ", below).")

  val options = Section("OPTIONS",

    "The compiler has a set of standard options that are supported on the " &
    "current development environment and will be supported in future " &
    "releases. An additional set of non-standard options are specific to " &
    "the current virtual machine implementation and are subject to change " &
    "in the future.  Non-standard options begin with " & Bold("-X") & ".",

    Section("Standard Options",
      DefinitionList(
        Definition(
          CmdOption("g:{none,source,line,vars,notc}"),
          SeqPara(
            Mono("\"none\"") & " generates no debugging info,",
            Mono("\"source\"") & " generates only the source file attribute,",
            Mono("\"line\"") & " generates source and line number information,",
            Mono("\"vars\"") & " generates source, line number and local " &
            "variable information,",
            Mono("\"notc\"") & " generates all of the above and " &
            Italic("will not") & " perform tail call optimization.")),
        Definition(
          CmdOption("nowarn"),
          "Generate no warnings"),
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
          CmdOption("unchecked"),
          SeqPara(
            "Enable detailed unchecked warnings",
            "Non variable type-arguments in type patterns are unchecked " &
            "since they are eliminated by erasure",
            "Available since Scala version 2.3.0")),
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
          CmdOption("d", Argument("directory")),
          "Specify where to place generated class files."),
        Definition(
          CmdOption("encoding", Argument("encoding")),
          SeqPara(
            "Specify character encoding used by source files.",
            "The default value is platform-specific (Linux: " & Mono("\"UTF8\"") &
            ", Windows: " & Mono("\"Cp1252\"") & "). Executing the following " &
            "code in the Scala interpreter will return the default value " &
            "on your system:",
            MBold("    scala> ") &
            Mono("new java.io.InputStreamReader(System.in).getEncoding"))),
        Definition(
          CmdOption("target:", Argument("target")),
          SeqPara(
            "Specify which backend to use (" & Mono("jvm-1.5," &
            "msil") & ").",
            "The default value is " & Mono("\"jvm-1.5\"") & " (was " &
            Mono("\"jvm-1.4\"") & " up to Scala version 2.6.1).")),
        Definition(
          CmdOption("print"),
          "Print program with all Scala-specific features removed"
        ),
        Definition(
          CmdOption("optimise"),
          "Generates faster bytecode by applying optimisations to the program"
        ),
        Definition(
          CmdOption("explaintypes"),
          "Explain type errors in more detail."),
        Definition(
          CmdOption("uniqid"),
          "Print identifiers with unique names (debugging option)."),
        Definition(
          CmdOption("version"),
          "Print product version and exit."),
        Definition(
          /*CmdOption("?") & "| " &*/ CmdOption("help"),
          "Print a synopsis of standard options."))),

    Section("Advanced Options",
      DefinitionList(
        Definition(
          CmdOption("Xassem", Argument("file")),
          "Name of the output assembly (only relevant with -target:msil)"),
        Definition(
          CmdOption("Xassem-path", Argument("path")),
          "List of assemblies referenced by the program (only relevant with -target:msil)"),
        Definition(
          CmdOption("Xcheck-null"),
          "Emit warning on selection of nullable reference"),
        Definition(
          CmdOption("Xdisable-assertions"),
          "Generate no assertions and assumptions"),
        Definition(
          CmdOption("Xexperimental"),
          "enable experimental extensions"),
        Definition(
          CmdOption("Xno-uescape"),
          "Disable handling of " & BSlash & "u unicode escapes"),
        Definition(
          CmdOption("Xplug-types"),
          "Parse but ignore annotations in more locations"),
        Definition(
          CmdOption("Xplugin:", Argument("file")),
          "Load a plugin from a file"),
        Definition(
          CmdOption("Xplugin-disable:", Argument("plugin")),
          "Disable a plugin"),
        Definition(
          CmdOption("Xplugin-list"),
          "Print a synopsis of loaded plugins"),
        Definition(
          CmdOption("Xplugin-opt:", Argument("plugin:opt")),
          "Pass an option to a plugin"),
        Definition(
          CmdOption("Xplugin-require:", Argument("plugin")),
          "Abort unless a plugin is available"),
        Definition(
          CmdOption("Xprint:", Argument("phases")),
          "Print out program after " & Argument("phases") & " (see below)."),
        Definition(
          CmdOption("Xprint-pos"),
          "Print tree positions (as offsets)"),
        Definition(
          CmdOption("Xprint-types"),
          "Print tree types (debugging option)."),
        Definition(
          CmdOption("Xprompt"),
          "Display a prompt after each error (debugging option)."),
        Definition(
          CmdOption("Xresident"),
          "Compiler stays resident, files to compile are read from standard " &
          "input."),
        Definition(
          CmdOption("Xshow-class", Argument("class")),
          "Show class info."),
        Definition(
          CmdOption("Xshow-object", Argument("object")),
          "Show object info."),
        Definition(
          CmdOption("Xshow-phases"),
          "Print a synopsis of compiler phases."),
        Definition(
          CmdOption("Xsource-reader", Argument("classname")),
          "Specify a custom method for reading source files."),
        Definition(
          CmdOption("Xscript", Argument("object")),
          "Compile as a script, wrapping the code into object.main().")
      )
    ),

    Section("Compilation Phases",
      DefinitionList(
        Definition(
          MItalic("initial"),
          "initializing compiler"),
        Definition(
          MItalic("parse"),
          "parse source files"),
        Definition(
          MItalic("namer"),
          "create symbols"),
        Definition(
          MItalic("analyze"),
          "name and type analysis"),
        Definition(
          MItalic("refcheck"),
          "reference checking"),
        Definition(
          MItalic("uncurry"),
          "uncurry function types and applications"),
        Definition(
          MItalic("transmatch"),
          "translate match expressions"),
        Definition(
          MItalic("lambdalift"),
          "lambda lifter"),
        Definition(
          MItalic("typesasvalues"),
          "represent types as values"),
        Definition(
          MItalic("addaccessors"),
          "add accessors for constructor arguments"),
        Definition(
          MItalic("explicitouterclasses"),
          "make links from inner classes to enclosing one explicit"),
        Definition(
          MItalic("addconstructors"),
          "add explicit constructor for each class"),
        Definition(
          MItalic("tailcall"),
          "add tail-calls"),
        Definition(
          MItalic("wholeprog"),
          "perform whole program analysis"),
        Definition(
          MItalic("addinterfaces"),
          "add one interface per class"),
        Definition(
          MItalic("expandmixins"),
          "expand mixins by code copying"),
        Definition(
          MItalic("boxing"),
          "makes boxing explicit"),
        Definition(
          MItalic("erasure"),
          "type eraser"),
        Definition(
          MItalic("icode"),
          "generate icode"),
        Definition(
          MItalic("codegen"),
          "enable code generation"),
        Definition(
          MItalic("terminal"),
          "compilation terminated"),
        Definition(
          MItalic("all"),
          "matches all phases"))))

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
          Mono("JAVA_OPTS=\"-Xmx512M -Xms16M -Xss16M\""),

          "With " & Link("GNU Java", "http://gcc.gnu.org/java/") & " one " &
          "may configure the memory usage of the GIJ as follows: " &
          Mono("JAVA_OPTS=\"--mx512m --ms16m\"")
        ))))

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

    Link(Bold("fsc") & "(1)", "fsc.html") & ", " &
    Link(Bold("sbaz") & "(1)", "sbaz.html") & ", " &
    Link(Bold("scala") & "(1)", "scala.html") & ", " &
    Link(Bold("scaladoc") & "(1)", "scaladoc.html") & ", " &
    Link(Bold("scalap") & "(1)", "scalap.html"))

  def manpage = new Document {
    title = command
    date = lastModified // e.g. "June 8, 2006"
    author = "Stephane Micheloud & LAMP"
    version = "0.4"
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
