/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Stephane Micheloud
 */
//$Id$

package man.man1

object scalac extends Command {
  import ManPage._

  protected val cn = new Error().getStackTrace()(0).getClassName()

  val name = Section("NAME",

    MBold(command) & " " & NDash & " Compiler for the " &
    Link("Scala 2", "http://scala.epfl.ch/") & " language")

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
          CmdOption("g"),
          "Generate debugging info"),
        Definition(
          CmdOption("nowarn"),
          "Generate no warnings"),
        Definition(
          CmdOption("verbose"),
          "Output messages about what the compiler is doing"),
        Definition(
          CmdOption("classpath", Argument("path")),
          "Specify where to find user class files (on Unix-based systems " &
          "a colon-separated list of paths, on Windows-based systems, a " &
          "semicolon-separate list of paths). This does not override the " &
          "built-in (" & Mono("\"boot\"") & ") search path."),
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
          "Specify character encoding used by source files."),
        Definition(
          CmdOption("target:", Argument("target")),
          "Specify which backend to use (" & Mono(Italic("jvm-1.5") & ", " &
          Italic("jvm-1.4") & ", " & Italic("msil") & ", " & Italic("cldc")) &
          ")."),
        Definition(
          CmdOption("migrate"),
          "Assist in migrating from Scala version 1.0."),
        Definition(
          CmdOption("statistics"),
          "Print compiler statistics."),
        Definition(
          CmdOption("resident"),
          "Compiler stays resident, files to compile are read from standard " &
          "input."),
        Definition(
          CmdOption("version"),
          "Print product version and exit."),
        Definition(
          CmdOption("?") & "| " & CmdOption("help"),
          "Print a synopsis of standard options."))),

    Section("Non-Standard Options",
      DefinitionList(
        Definition(
          CmdOption("Xinline"),
          "Perform inlining when possible."),
        Definition(
          CmdOption("Xcloselim"),
          "Perform closure elimination."),
        Definition(
          CmdOption("Xshowcls", Argument("class")),
          "Show class info."),
        Definition(
          CmdOption("Xshowobj", Argument("object")),
          "Show object info."),
        Definition(
          CmdOption("Xshowicode"),
          "Print the generated ICode."),
        Definition(
          CmdOption("Xgadt"),
          "Enable gadt for classes."),
        Definition(
          CmdOption("Xlinearizer", Argument("Xlinearizer")),
          "Linearizer to use (" & Mono("normal,dfs,rpo") & ")."),
        Definition(
          CmdOption("Xgenerics"),
          "Use generic Java types."))),

    Section("Debug Options",
      DefinitionList(
        Definition(
          CmdOption("debug"),
          "Output debugging messages."),
        Definition(
          CmdOption("explaintypes"),
          "Explain type errors in more detail."),
        Definition(
          CmdOption("uniqid"),
          "Print identifiers with unique names (debugging option)."),
        Definition(
          CmdOption("printtypes"),
          "Print tree types (debugging option)."),
        Definition(
          CmdOption("prompt"),
          "Display a prompt after each error (debugging option)."),
        Definition(
          CmdOption("noimports"),
          "Compile without any implicit imports."),
        Definition(
          CmdOption("nopredefs"),
          "Compile without any implicit predefined values."),
        Definition(
          CmdOption("skip:", Argument("phases")),
          "Skip " & Argument("phases") & " (see below)."),
        Definition(
          CmdOption("check:", Argument("phases")),
          "Check the tree after " & Argument("phases") & " (see below)."),
        Definition(
          CmdOption("print:", Argument("phases")),
          "Print out program after " & Argument("phases") & " (see below)."),
        Definition(
          CmdOption("printer:", Argument("printer")),
          "Printer to use."),
        Definition(
          CmdOption("print-file", Argument("file")),
          "Specify file in which to print trees."),
        Definition(
          CmdOption("graph:", Argument("phases")),
          "Graph the program after " & Argument("phases") & " (see below)."),
        Definition(
          CmdOption("stop:", Argument("phases")),
          "Stop after first phase in " & Argument("phases") & " (see below)."),
        Definition(
          CmdOption("log:", Argument("phases")),
          "Log operations in " & Argument("phases") & " (see below)."))),

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
