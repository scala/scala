/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Stephane Micheloud
 */

package scala.man1

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
    "in the future.  Non-standard options begin with " & MBold("-X") & ".",

    Section("Standard Options",
      DefinitionList(
        Definition(
          CmdOptionBound("D", "property=value"),
          "Pass " & CmdOptionBound("D", "property=value") & " directly to the runtime system."),
        Definition(
          CmdOptionBound("J", Argument("flag")),
          "Pass " & Mono(Argument("flag")) & " directly to the runtime system."),
        Definition(
          CmdOptionBound("P:", Argument("plugin:opt")),
          "Pass an option to a plugin"),
        Definition(
          CmdOption("X"),
          "Print a synopsis of advanced options."),
        Definition(
          CmdOption("bootclasspath", Argument("path")),
          "Override location of bootstrap class files (where to find the " &
          "standard built-in classes, such as \"" & Mono("scala.List") & "\")."),
        Definition(
          CmdOption("classpath", Argument("path")),
          SeqPara(
            "Specify where to find user class files (on Unix-based systems " &
            "a colon-separated list of paths, on Windows-based systems, a " &
            "semicolon-separated list of paths). This does not override the " &
            "built-in (" & Mono("\"boot\"") & ") search path.",
            "The default class path is the current directory. Setting the " &
            Mono("CLASSPATH") & " variable or using the " & Mono("-classpath") & " " &
            "command-line option overrides that default, so if you want to " &
            "include the current directory in the search path, you must " &
            "include " & Mono("\".\"") & " in the new settings.")),
        Definition(
          CmdOption("d", Argument("directory|jar")),
          "Specify where to place generated class files."),
        Definition(
          CmdOption("deprecation"),
          SeqPara(
            "Emit warning and location for usages of deprecated APIs.",
            "Available since Scala version 2.2.1")),
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
          CmdOption("explaintypes"),
          "Explain type errors in more detail."),
        Definition(
          CmdOption("extdirs", Argument("dirs")),
          "Override location of installed extensions."),
        Definition(
          CmdOption("feature"),
          "Emit warning and location for usages of features that should be imported explicitly."),
        Definition(
          CmdOptionBound("g:", "{none,source,line,vars,notailcalls}"),
          SeqPara(
            Mono("\"none\"") & " generates no debugging info,",
            Mono("\"source\"") & " generates only the source file attribute,",
            Mono("\"line\"") & " generates source and line number information,",
            Mono("\"vars\"") & " generates source, line number and local " &
            "variable information,",
            Mono("\"notailcalls\"") & " generates all of the above and " &
            Italic("will not") & " perform tail call optimization.")),
        Definition(
          CmdOption("help"),
          "Print a synopsis of standard options."),
        Definition(
          CmdOption("javabootclasspath", Argument("path")),
          "Override Java boot classpath."),
        Definition(
          CmdOption("javaextdirs", Argument("path")),
          "Override Java extdirs classpath."),
        Definition(
          CmdOptionBound("language:", Argument("feature")),
          "Enable one or more language features."),
        Definition(
          CmdOption("no-specialization"),
          "Ignore " & MItalic("@specialize") & " annotations."),
        Definition(
          CmdOption("nobootcp"),
          "Do not use the boot classpath for the Scala jar files."),
        Definition(
          CmdOption("nowarn"),
          "Generate no warnings"),
        Definition(
          CmdOption("optimise"),
          "Generates faster bytecode by applying optimisations to the program."),
        Definition(
          CmdOption("print"),
          "Print program with all Scala-specific features removed."),
        Definition(
          CmdOption("sourcepath", Argument("path")),
          "Specify location(s) of source files."),
        Definition(
          CmdOptionBound("target:", "{jvm-1.5,jvm-1.6,jvm-1.7,jvm-1.8}"),
          SeqPara(
            Mono("\"jvm-1.5\"") & " target JVM 1.5 (deprecated),",
            Mono("\"jvm-1.6\"") & " target JVM 1.6 (default),",
            Mono("\"jvm-1.7\"") & " target JVM 1.7,",
            Mono("\"jvm-1.8\"") & " target JVM 1.8,")),
        Definition(
          CmdOption("toolcp", Argument("path")),
          "Add to the runner classpath."),
        Definition(
          CmdOption("unchecked"),
          SeqPara(
            "Enable detailed unchecked (erasure) warnings",
            "Non variable type-arguments in type patterns are unchecked " &
            "since they are eliminated by erasure",
            "Available since Scala version 2.3.0")),
        Definition(
          CmdOption("uniqid"),
          "Uniquely tag all identifiers in debugging output."),
        Definition(
          CmdOption("usejavacp"),
          "Utilize the java.class.path in classpath resolution."),
        Definition(
          CmdOption("usemanifestcp"),
          "Utilize the manifest in classpath resolution."),
        Definition(
          CmdOption("verbose"),
          "Output messages about what the compiler is doing"),
        Definition(
          CmdOption("version"),
          "Print product version and exit."),
        Definition(
          Mono(Bold("@") & Argument("file")),
          "A text file containing compiler arguments (options and source files)")

        // TODO - Add macros an dsuch here.
      )
    ),

    Section("Advanced Options",
      DefinitionList(
        Definition(
          CmdOption("Xcheckinit"),
          "Wrap field accessors to throw an exception on uninitialized access."),
        Definition(
          CmdOption("Xdev"),
          "Enable warnings for developers working on the Scala compiler"),
        Definition(
          CmdOption("Xdisable-assertions"),
          "Generate no assertions and assumptions"),
        Definition(
          CmdOption("Xelide-below", Argument("n")),
          "Calls to " & MItalic("@elidable") &
          " methods are omitted if method priority is lower than argument."),
        Definition(
          CmdOption("Xexperimental"),
          "Enable experimental extensions"),
        Definition(
          CmdOption("Xfatal-warnings"),
          "Fail the compilation if there are any warnings."),
        Definition(
          CmdOption("Xfull-lubs"),
          "Retain pre 2.10 behavior of less aggressive truncation of least upper bounds."),
        Definition(
          CmdOption("Xfuture"),
          "Turn on future language features."),
        Definition(
          CmdOption("Xgenerate-phase-graph", Argument("file")),
          "Generate the phase graphs (outputs .dot files) to fileX.dot."),
        Definition(
          CmdOption("Xlint"),
          "Enable recommended additional warnings."),
        Definition(
          CmdOption("Xlog-free-terms"),
          "Print a message when reification creates a free term."),
        Definition(
          CmdOption("Xlog-free-types"),
          "Print a message when reification resorts to generating a free type."),
        Definition(
          CmdOption("Xlog-implicit-conversions"),
          "Print a message whenever an implicit conversion is inserted."),
        Definition(
          CmdOption("Xlog-implicits"),
          "Show more detail on why some implicits are not applicable."),
        Definition(
          CmdOption("Xlog-reflective-calls"),
          "Print a message when a reflective method call is generated."),
        Definition(
          CmdOptionBound("Xmacro-settings:", Argument("option")),
          "Custom settings for macros."),
        Definition(
          CmdOption("Xmain-class", Argument("path")),
          "Class for manifest's Main-Class entry (only useful with -d <jar>)."),
        Definition(
          CmdOption("Xmax-classfile-name", Argument("n")),
          "Maximum filename length for generated classes."),
        Definition(
          CmdOptionBound("Xmigration:", Argument("version")),
          "Warn about constructs whose behavior may have changed since" & Argument("version") & "."),
        Definition(
          CmdOption("Xno-forwarders"),
          "Do not generate static forwarders in mirror classes."),
        Definition(
          CmdOption("Xno-patmat-analysis"),
          "Don't perform exhaustivity/unreachability analysis. Also, ignore " & MItalic("@switch") & " annotation."),
        Definition(
          CmdOption("Xno-uescape"),
          "Disable handling of " & BSlash & "u unicode escapes"),
        Definition(
          CmdOption("Xnojline"),
          "Do not use JLine for editing."),
        Definition(
          CmdOptionBound("Xplugin:", Argument("paths")),
          "Load a plugin from each classpath."),
        Definition(
          CmdOptionBound("Xplugin-disable:", Argument("plugin")),
          "Disable plugins by name."),
        Definition(
          CmdOption("Xplugin-list"),
          "Print a synopsis of loaded plugins."),
        Definition(
          CmdOptionBound("Xplugin-require:", Argument("plugin")),
          "Abort if a named plugin is not loaded."),
        Definition(
          CmdOption("Xpluginsdir", Argument("path")),
          "Path to search for plugin archives."),
        Definition(
          CmdOptionBound("Xprint:", Argument("phases")),
          "Print out program after " & Argument("phases") & " (see below)."),
        Definition(
          CmdOptionBound("Xprint-icode", "[:" & Argument("phases") & "]"),
          "Log internal icode to *.icode files after" & Argument("phases") & " (default: icode)."),
        Definition(
          CmdOption("Xprint-pos"),
          "Print tree positions, as offsets."),
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
          CmdOption("Xscript", Argument("object")),
          "Treat the source file as a script and wrap it in a main method."),
        Definition(
          CmdOption("Xshow-class", Argument("class")),
          "Show internal representation of class."),
        Definition(
          CmdOption("Xshow-object", Argument("object")),
          "Show internal representation of object."),
        Definition(
          CmdOption("Xshow-phases"),
          "Print a synopsis of compiler phases."),
        Definition(
          CmdOptionBound("Xsource:", Argument("version")),
          "Treat compiler input as Scala source for the specified version, see SI-8126."),
        Definition(
          CmdOption("Xsource-reader", Argument("classname")),
          "Specify a custom method for reading source files."),
        Definition(
          CmdOption("Xstrict-inference"),
          "Don't infer known-unsound types."),
        Definition(
          CmdOption("Xverify"),
          "Verify generic signatures in generated bytecode (asm backend only)."),
        Definition(
          CmdOptionBound("Xxml:", "{coalescing}"),
          SeqPara(
            "Configure XML parsing.",
            Mono("\"coalescing\"") & " convert PCData to Text and coalesce sibling nodes (default in 2.11).")),
        Definition(
          CmdOption("Y"),
          "Print a synopsis of private options.")
      )
    ),

    Section("Compilation Phases",
      DefinitionList(
        Definition(
          MItalic("parser"),
          "parse source into ASTs, perform simple desugaring"),
        Definition(
          MItalic("namer"),
          "resolve names, attach symbols to named trees"),
        Definition(
          MItalic("packageobjects"),
          "load package objects"),
        Definition(
          MItalic("typer"),
          "the meat and potatoes: type the trees"),
        Definition(
          MItalic("patmat"),
          "translate match expressions"),
        Definition(
          MItalic("superaccessors"),
          "add super accessors in traits and nested classes"),
        Definition(
          MItalic("extmethods"),
          "add extension methods for inline classes"),
        Definition(
          MItalic("pickler"),
          "serialize symbol tables"),
        Definition(
          MItalic("refchecks"),
          "reference/override checking, translate nested objects"),
        Definition(
          MItalic("selectiveanf"),
          "ANF pre-transform for " & MItalic("@cps") & " (CPS plugin)"),
        Definition(
          MItalic("selectivecps"),
          MItalic("@cps") & "-driven transform of selectiveanf assignments (CPS plugin)"),
        Definition(
          MItalic("uncurry"),
          "uncurry, translate function values to anonymous classes"),
        Definition(
          MItalic("tailcalls"),
          "replace tail calls by jumps"),
        Definition(
          MItalic("specialize"),
          MItalic("@specialized") & "-driven class and method specialization"),
        Definition(
          MItalic("explicitouter"),
          "this refs to outer pointers, translate patterns"),
        Definition(
          MItalic("erasure"),
          "erase types, add interfaces for traits"),
        Definition(
          MItalic("posterasure"),
          "clean up erased inline classes"),
        Definition(
          MItalic("lazyvals"),
          "allocate bitmaps, translate lazy vals into lazified defs"),
        Definition(
          MItalic("lambdalift"),
          "move nested functions to top level"),
        Definition(
          MItalic("constructors"),
          "move field definitions into constructors"),
        Definition(
          MItalic("flatten"),
          "eliminate inner classes"),
        Definition(
          MItalic("mixin"),
          "mixin composition"),
        Definition(
          MItalic("cleanup"),
          "platform-specific cleanups, generate reflective calls"),
        Definition(
          MItalic("delambdafy"),
          "remove lambdas"),
        Definition(
          MItalic("icode"),
          "generate portable intermediate code"),
        Definition(
          MItalic("inliner"),
          "optimization: do inlining"),
        Definition(
          MItalic("inlineHandlers"),
          "optimization: inline exception handlers"),
        Definition(
          MItalic("closelim"),
          "optimization: eliminate uncalled closures"),
        Definition(
          MItalic("constopt"),
          "optimization: optimize null and other constants"),
        Definition(
          MItalic("dce"),
          "optimization: eliminate dead code"),
        Definition(
          MItalic("jvm"),
          "generate JVM bytecode"),
        Definition(
          MItalic("terminal"),
          "the last phase in the compiler chain"),
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
          Mono("JAVA_OPTS=\"-Xmx512M -Xms16M -Xss16M\"")
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
    Link(Bold("scala") & "(1)", "scala.html") & ", " &
    Link(Bold("scaladoc") & "(1)", "scaladoc.html") & ", " &
    Link(Bold("scalap") & "(1)", "scalap.html"))

  def manpage = new Document {
    title = command
    date = "March 2012"
    author = "Stephane Micheloud"
    version = "1.0"
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
