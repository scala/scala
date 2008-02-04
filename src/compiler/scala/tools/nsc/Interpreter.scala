/* NSC -- new Scala compiler
 * Copyright 2005-2008 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.io.{File, PrintWriter, StringWriter, Writer}
import java.lang.{Class, ClassLoader}
import java.net.{URL, URLClassLoader}

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, HashSet, ArrayBuffer}

//import ast.parser.SyntaxAnalyzer
import io.PlainFile
import reporters.{ConsoleReporter, Reporter}
import symtab.Flags
import util.{SourceFile,BatchSourceFile,ClassPath}
import nsc.{InterpreterResults=>IR}

/** <p>
 *    An interpreter for Scala code.
 *  </p>
 *  <p>
 *    The main public entry points are <code>compile()</code> and
 *    <code>interpret()</code>. The <code>compile()</code> method loads a
 *    complete Scala file.  The <code>interpret()</code> method executes one
 *    line of Scala code at the request of the user.
 *  </p>
 *  <p>
 *    The overall approach is based on compiling the requested code and then
 *    using a Java classloader and Java reflection to run the code
 *    and access its results.
 *  </p>
 *  <p>
 *    In more detail, a single compiler instance is used
 *    to accumulate all successfully compiled or interpreted Scala code.  To
 *    "interpret" a line of code, the compiler generates a fresh object that
 *    includes the line of code and which has public member(s) to export
 *    all variables defined by that code.  To extract the result of an
 *    interpreted line to show the user, a second "result object" is created
 *    which imports the variables exported by the above object and then
 *    exports a single member named "result".  To accomodate user expressions
 *    that read from variables or methods defined in previous statements, "import"
 *    statements are used.
 *  </p>
 *  <p>
 *    This interpreter shares the strengths and weaknesses of using the
 *    full compiler-to-Java.  The main strength is that interpreted code
 *    behaves exactly as does compiled code, including running at full speed.
 *    The main weakness is that redefining classes and methods is not handled
 *    properly, because rebinding at the Java level is technically difficult.
 *  </p>
 *
 * @author Moez A. Abdel-Gawad
 * @author Lex Spoon
 */
class Interpreter(val settings: Settings, out: PrintWriter) {
  import symtab.Names

  /* If the interpreter is running on pre-jvm-1.5 JVM,
     it is necessary to force the target setting to jvm-1.4 */
  private val major = System.getProperty("java.class.version").split("\\.")(0)
  if (major.toInt < 49) {
    this.settings.target.value = "jvm-1.4"
  }

  /** the compiler to compile expressions with */
  val compiler: scala.tools.nsc.Global = newCompiler(settings, reporter)

  import compiler.Traverser
  import compiler.{Tree, TermTree,
                   ValOrDefDef, ValDef, DefDef, Assign,
                   ClassDef, ModuleDef, Ident, Select, TypeDef,
                   Import, MemberDef}
  import compiler.CompilationUnit
  import compiler.{Symbol,Name,Type}
  import compiler.nme
  import Interpreter.string2code

  /** construct an interpreter that reports to Console */
  def this(settings: Settings) =
    this(settings,
         new NewLinePrintWriter(new ConsoleWriter, true))

  /** whether to print out result lines */
  private var printResults: Boolean = true

  /** Be quiet.  Do not print out the results of each
    * submitted command unless an exception is thrown.  */
  def beQuiet = { printResults = false }

  /** Temporarily be quiet */
  def beQuietDuring[T](operation: => T): T = {
    val wasPrinting = printResults
    try {
      printResults = false
      operation
    } finally {
      printResults = wasPrinting
    }
  }

  /** interpreter settings */
  val isettings = new InterpreterSettings

  /** directory to save .class files to */
  val classfilePath = File.createTempFile("scalaint", "")
  classfilePath.delete  // the file is created as a file; make it a directory
  classfilePath.mkdirs

  /* set up the compiler's output directory */
  settings.outdir.value = classfilePath.getPath

  object reporter extends ConsoleReporter(settings, null, out) {
    //override def printMessage(msg: String) { out.println(clean(msg)) }
    override def printMessage(msg: String) { out.print(clean(msg) + "\n"); out.flush() }
  }

  /** Instantiate a compiler.  Subclasses can override this to
   *  change the compiler class used by this interpreter. */
  protected def newCompiler(settings: Settings, reporter: Reporter) =
    new scala.tools.nsc.Global(settings, reporter)


  /** the compiler's classpath, as URL's */
  val compilerClasspath: List[URL] =
    ClassPath.expandPath(compiler.settings.classpath.value).
      map(s => new File(s).toURL)

  /** class loader used to load compiled code */
  /* A single class loader is used for all commands interpreted by this Interpreter.
     It would also be possible to create a new class loader for each command
     to interpret.  The advantages of the current approach are:

       - Expressions are only evaluated one time.  This is especially
         significant for I/O, e.g. "val x = Console.readLine"

     The main disadvantage is:

       - Objects, classes, and methods cannot be rebound.  Instead, definitions
         shadow the old ones, and old code objects refer to the old
         definitions.
  */
  private val classLoader =
    if (parentClassLoader eq null)
      new URLClassLoader((classfilePath.toURL :: compilerClasspath).toArray)
    else
      new URLClassLoader((classfilePath.toURL :: compilerClasspath).toArray,
                          parentClassLoader)

  /** Set the current Java "context" class loader to this
    * interpreter's class loader */
  def setContextClassLoader() {
    Thread.currentThread.setContextClassLoader(classLoader)
  }

  /** XXX Let's get rid of this.  I believe the Eclipse plugin is
    * the only user of it, so this should be doable.  */
  protected def parentClassLoader: ClassLoader = null

  /** the previous requests this interpreter has processed */
  private val prevRequests = new ArrayBuffer[Request]()

  /** next line number to use */
  private var nextLineNo = 0

  /** allocate a fresh line name */
  private def newLineName = {
    val num = nextLineNo
    nextLineNo += 1
    compiler.nme.INTERPRETER_LINE_PREFIX + num
  }

  /** next result variable number to use */
  private var nextVarNameNo = 0

  /** allocate a fresh variable name */
  private def newVarName() = {
    val num = nextVarNameNo
    nextVarNameNo += 1
    compiler.nme.INTERPRETER_VAR_PREFIX + num
  }


  /** generate a string using a routine that wants to write on a stream */
  private def stringFrom(writer: PrintWriter => Unit): String = {
    val stringWriter = new StringWriter()
    val stream = new NewLinePrintWriter(stringWriter)
    writer(stream)
    stream.close
    stringWriter.toString
  }

  /** Truncate a string if it is longer than settings.maxPrintString */
  private def truncPrintString(str: String): String = {
    val maxpr = isettings.maxPrintString

    if (maxpr <= 0)
      return str

    if (str.length <= maxpr)
      return str

    val trailer = "..."
    if (maxpr >= trailer.length+1)
      return str.substring(0, maxpr-3) + trailer

    str.substring(0, maxpr)
  }

  /** Clean up a string for output */
  private def clean(str: String) =
    truncPrintString(Interpreter.stripWrapperGunk(str))

  /** Indent some code by the width of the scala> prompt.
   *  This way, compiler error messages read beettr.
   */
  def indentCode(code: String) = {
    val spaces = "       "

    stringFrom(str =>
      for (line <- code.lines) {
        str.print(spaces)
        str.print(line + "\n")
        str.flush()
      })
  }

  implicit def name2string(name: Name) = name.toString

  /** Compute imports that allow definitions from previous
   *  requests to be visible in a new request.  Returns
   *  three pieces of related code:
   *
   *  1. An initial code fragment that should go before
   *  the code of the new request.
   *
   *  2. A code fragment that should go after the code
   *  of the new request.
   *
   *  3. An access path which can be traverested to access
   *  any bindings inside code wrapped by #1 and #2 .
   *
   * The argument is a set of Names that need to be imported.
   *
   * Limitations: This method is not as precise as it could be.
   * (1) It does not process wildcard imports to see what exactly
   * they import.
   * (2) If it imports any names from a request, it imports all
   * of them, which is not really necessary.
   * (3) It imports multiple same-named implicits, but only the
   * last one imported is actually usable.
   */
  private def importsCode(wanted: Set[Name]): (String, String, String) = {
    /** Narrow down the list of requests from which imports
     *  should be taken.  Removes requests which cannot contribute
     *  useful imports for the specified set of wanted names.
     */
    def reqsToUse: List[Request] = {
      /** Loop through the requests in reverse and select
       *  which ones to keep.  'wanted' is the set of
       *  names that need to be imported, and
       *  'shadowed' is the list of names useless to import
       *  because a later request will re-import it anyway.
       */
      def select(reqs: List[Request], wanted: Set[Name]): List[Request] = {
        reqs match {
          case Nil => Nil

          case req::rest =>
            val keepit = req.definesImplicit || (req match {
              case req:ImportReq =>
                req.importsWildcard ||
                req.importedNames.exists(wanted.contains)
              case _ =>
                req.boundNames.exists(wanted.contains)
            })

            val newWanted =
              if (keepit) {
                req match {
                  case req:ImportReq =>
                    wanted -- req.importedNames ++ req.usedNames

                  case _ => wanted -- req.boundNames
                }
              } else {
                wanted
              }

            val restToKeep = select(rest, newWanted)

            if(keepit)
              req :: restToKeep
            else
              restToKeep
        }
      }

      select(prevRequests.toList.reverse, wanted).reverse
    }

    val code = new StringBuffer
    val trailingBraces = new StringBuffer
    val accessPath = new StringBuffer
    val impname = compiler.nme.INTERPRETER_IMPORT_WRAPPER
    val currentImps = mutable.Set.empty[Name]

    // add code for a new object to hold some imports
    def addWrapper() {
      code.append("object " + impname + "{\n")
      trailingBraces.append("}\n")
      accessPath.append("." + impname)
      currentImps.clear
    }

    addWrapper()

    // loop through previous requests, adding imports
    // for each one
    for (req <- reqsToUse) {
      req match {
        case req:ImportReq =>
          // If the user entered an import, then just use it

          // add an import wrapping level if the import might
          // conflict with some other import
          if(req.importsWildcard ||
             currentImps.exists(req.importedNames.contains))
            if(!currentImps.isEmpty)
              addWrapper()

          code.append(req.line + ";\n")

          // give wildcard imports a import wrapper all to their own
          if(req.importsWildcard)
            addWrapper()
          else
            currentImps ++= req.importedNames

        case req =>
          // For other requests, import each bound variable.
          // import them explicitly instead of with _, so that
          // ambiguity errors will not be generated. Also, quote
 	  // the name of the variable, so that we don't need to
 	  // handle quoting keywords separately.
          for (imv <- req.boundNames) {
            if (currentImps.contains(imv))
              addWrapper()
            code.append("import ")
            code.append(req.objectName + req.accessPath + ".`" + imv + "`;\n")
            currentImps += imv
          }
      }
    }

    addWrapper() // Add one extra wrapper, to prevent warnings
                 // in the frequent case of redefining
                 // the value bound in the last interpreter
                 // request.

    (code.toString, trailingBraces.toString, accessPath.toString)
  }

  /** Parse a line into a sequence of trees. Returns None if the input
    * is incomplete. */
  private def parse(line: String): Option[List[Tree]] = {
    var justNeedsMore = false
    reporter.withIncompleteHandler((pos,msg) => {justNeedsMore = true}) {
      // simple parse: just parse it, nothing else
      def simpleParse(code: String): List[Tree] = {
        reporter.reset
        val unit =
          new CompilationUnit(
            new BatchSourceFile("<console>", code.toCharArray()))
        val scanner = new compiler.syntaxAnalyzer.UnitParser(unit);
        val xxx = scanner.templateStatSeq;
        (xxx._2)
      }
      val (trees) = simpleParse(line)
      if (reporter.hasErrors) {
        Some(Nil) // the result did not parse, so stop
      } else if (justNeedsMore) {
        None
      } else {
        Some(trees)
      }
    }
  }

  /** Compile an nsc SourceFile.  Returns true if there are
   *  no compilation errors, or false othrewise.
   */
  def compileSources(sources: List[SourceFile]): Boolean = {
    val cr = new compiler.Run
    reporter.reset
    cr.compileSources(sources)
    !reporter.hasErrors
  }

  /** Compile a string.  Returns true if there are no
   *  compilation errors, or false otherwise.
   */
  def compileString(code: String): Boolean =
    compileSources(List(new BatchSourceFile("<script>", code.toCharArray)))

  /** Build a request from the user. <code>trees</code> is <code>line</code>
   *  after being parsed.
   */
  private def buildRequest(trees: List[Tree], line: String, lineName: String): Request =
    trees match {
      /* This case for assignments is more specialized than desirable: it only
         handles assignments to an identifier.  It would be better to support
         arbitrary paths being assigned, but that is technically difficult
         because of the way objectSourceCode and resultObjectSourceCode are
         implemented in class Request. */
      case List(Assign(Ident(lhs), _)) =>
        new AssignReq(lhs, line, lineName)
      case _ if trees.forall(t => t.isInstanceOf[ValOrDefDef]) =>
        new DefReq(line, lineName)
      case List(_:TermTree) | List(_:Ident) | List(_:Select) =>
        new ExprReq(line, lineName)
      case List(_:ModuleDef) => new ModuleReq(line, lineName)
      case List(_:ClassDef) => new ClassReq(line, lineName)
      case List(t:TypeDef) if compiler.treeInfo.isAliasTypeDef(t) =>
        new TypeAliasReq(line, lineName)
      case List(_:Import) => new ImportReq(line, lineName)
      case _ =>
        reporter.error(null, "That kind of statement combination is not supported by the interpreter.")
        null
    }

  /** <p>
   *    Interpret one line of input.  All feedback, including parse errors
   *    and evaluation results, are printed via the supplied compiler's
   *    reporter.  Values defined are available for future interpreted
   *    strings.
   *  </p>
   *  <p>
   *    The return value is whether the line was interpreter successfully,
   *    e.g. that there were no parse errors.
   *  </p>
   *
   *  @param line ...
   *  @return     ...
   */
  def interpret(line: String): IR.Result = {
    if (prevRequests.isEmpty)
      new compiler.Run // initialize the compiler

    // parse
    val trees = parse(indentCode(line)) match {
      case None => return IR.Incomplete
      case (Some(Nil)) => return IR.Error // parse error or empty input
      case Some(trees) => trees
    }

    val lineName = newLineName

    // figure out what kind of request
    val req = buildRequest(trees, line, lineName)
    if (req eq null) return IR.Error  // a disallowed statement type

    if (!req.compile)
      return IR.Error  // an error happened during compilation, e.g. a type error

    val (interpreterResultString, succeeded) = req.loadAndRun

    if (printResults || !succeeded) {
      // print the result
      out.print(clean(interpreterResultString))

      // print out types of functions; they are not printed in the
      // request printout
      out.print(clean(req.defTypesSummary))
    }

    // book-keeping
    if (succeeded)
      prevRequests += req

    if (succeeded) IR.Success else IR.Error
  }

  /** A counter used for numbering objects created by <code>bind()</code>. */
  private var binderNum = 0

  /** Bind a specified name to a specified value.  The name may
   *  later be used by expressions passed to interpret.
   *
   *  @param name      the variable name to bind
   *  @param boundType the type of the variable, as a string
   *  @param value     the object value to bind to it
   *  @return          an indication of whether the binding succeeded
   */
  def bind(name: String, boundType: String, value: Any): IR.Result = {
    val binderName = "binder" + binderNum
    binderNum += 1

    compileString(
        "object " + binderName +
        "{ var value: " + boundType + " = _; " +
        " def set(x: Any) = value=x.asInstanceOf[" + boundType + "]; }")

    val binderObject =
      Class.forName(binderName, true, classLoader)
    val setterMethod =
      (binderObject
          .getDeclaredMethods
          .toList
          .find(meth => meth.getName == "set")
          .get)
    var argsHolder: Array[Any] = null // this roundabout approach is to try and
                                      // make sure the value is boxed
    argsHolder = List(value).toArray
    setterMethod.invoke(null, argsHolder.asInstanceOf[Array[AnyRef]])

    interpret("val " + name + " = " + binderName + ".value")
  }


  /** <p>
   *    This instance is no longer needed, so release any resources
   *    it is using.
   *  </p>
   *  <p>
   *    Specifically, this deletes the temporary directory used for holding
   *    class files for this instance.  This cannot safely be done after
   *    each command is executed because of Java's demand loading.
   *  </p>
   *  <p>
   *    Also, this flushes the reporter's output.
   *  </p>
   */
  def close() {
    Interpreter.deleteRecursively(classfilePath)
    reporter.flush()
  }

  /** A traverser that finds all mentioned identifiers, i.e. things
   *  that need to be imported.
   *  It might return extra names.
   */
  private class ImportVarsTraverser(definedVars: List[Name]) extends Traverser {
    val importVars = new HashSet[Name]()

    override def traverse(ast: Tree) {
      ast match {
        case Ident(name) => importVars += name
        case _ => super.traverse(ast)
      }
    }
  }


  /** One line of code submitted by the user for interpretation */
  private abstract class Request(val line: String, val lineName: String) {
    val trees = parse(line) match {
      case Some(ts) => ts
      case None => Nil
    }

    /** name to use for the object that will compute "line" */
    def objectName = lineName + compiler.nme.INTERPRETER_WRAPPER_SUFFIX

    /** name of the object that retrieves the result from the above object */
    def resultObjectName = "RequestResult$" + objectName

    /** whether the trees need a variable name, as opposed to standing
        alone */
    val needsVarName: Boolean = false

    /** A cache for the chosen variable name, if one has been calculated */
    var varNameCache: Option[String] = None

    /** A computed variable name, if one is needed */
    def varName = varNameCache match {
      case None =>
        varNameCache = Some(newVarName)
        varNameCache.get
      case Some(name) =>
        name
    }

    /** list of methods defined */
    val defNames =
      for (DefDef(mods, name, _, _, _, _) <- trees if mods.isPublic)
        yield name

    /** list of val's and var's defined */
    val valAndVarNames = {
      val baseNames =
        for (ValDef(mods, name, _, _) <- trees if mods.isPublic)
          yield name

      if (needsVarName)
        compiler.encode(varName) :: baseNames  // add a var name
      else
        baseNames
    }

    /** list of modules defined */
    val moduleNames = {
      val explicit =
        for (ModuleDef(mods, name, _) <- trees if mods.isPublic)
          yield name
      val caseClasses =
        for {val ClassDef(mods, name, _, _) <- trees
             mods.isPublic
             mods.hasFlag(Flags.CASE)}
        yield name.toTermName
      explicit ::: caseClasses
    }

    /** list of classes defined */
    val classNames =
      for (ClassDef(mods, name, _, _) <- trees if mods.isPublic)
        yield name

    /** list of type aliases defined */
    val typeNames =
      for (t @ TypeDef(mods, name, _, _) <- trees
           if mods.isPublic && compiler.treeInfo.isAliasTypeDef(t))
        yield name

    /** all (public) names defined by these statements */
    val boundNames =
      defNames ::: valAndVarNames ::: moduleNames ::: classNames ::: typeNames

    /** list of names used by this expression */
    val usedNames: List[Name] = {
      val ivt = new ImportVarsTraverser(boundNames)
      ivt.traverseTrees(trees)
      ivt.importVars.toList
    }

    /** Whether this request defines an implicit.  */
    def definesImplicit = trees.exists {
      case tree:MemberDef =>
        tree.mods.hasFlag(symtab.Flags.IMPLICIT)
      case _ => false
    }

    def myImportsCode = importsCode(Set.empty ++ usedNames)

    /** Code to append to objectName to access anything that
     *  the request binds.  */
    val accessPath = myImportsCode._3


    /** Code to access a variable with the specified name */
    def fullPath(vname: String): String =
      objectName + accessPath + "." + vname

    /** Code to access a variable with the specified name */
    def fullPath(vname: Name): String = fullPath(vname.toString)

    /** the line of code to compute */
    def toCompute = line

    /** generate the source code for the object that computes this request */
    def objectSourceCode: String =
      stringFrom(code => {
        // header for the wrapper object
        code.println("object " + objectName + " {")

        val (importsPreamble, importsTrailer, _) = myImportsCode

        code.print(importsPreamble)

        // the variable to compute, if any
        if (needsVarName)
          code.println("  val " + varName + " = ")

        code.println(indentCode(toCompute))

        code.println(importsTrailer)

        //end the wrapper object
        code.println(";}")
      })

    /** Types of variables defined by this request.  They are computed
        after compilation of the main object */
    var typeOf: Map[Name, String] = _

    /** generate source code for the object that retrieves the result
        from objectSourceCode */
    def resultObjectSourceCode: String =
      stringFrom(code => {
        code.println("object " + resultObjectName)
        code.println("{ val result: String = {")
        code.println(objectName + accessPath + ";")  // evaluate the object, to make sure its constructor is run
        code.print("\"\"")  // print an initial empty string, so later code can
                            // uniformly be: + morestuff
        resultExtractionCode(code)
        code.println("}")
        code.println(";}")
      })

    def resultExtractionCode(code: PrintWriter) {
      for (vname <- valAndVarNames) {
        code.print(" + \"" + vname + ": " +
		   string2code(typeOf(vname)) +
		   " = \" + " +
                   " (if(" +
		   fullPath(vname) +
                   ".asInstanceOf[AnyRef] != null) " +
                   " ((if(" +
		   fullPath(vname) +
		   ".toString.contains('\\n')) " +
                   " \"\\n\" else \"\") + " +
                   fullPath(vname) + ".toString + \"\\n\") else \"null\\n\") ")
      }
    }

    /** Compile the object file.  Returns whether the compilation succeeded.
     *  If all goes well, the "types" map is computed. */
    def compile(): Boolean = {
      reporter.reset  // without this, error counting is not correct,
                      // and the interpreter sometimes overlooks compile failures!

      // compile the main object
      val objRun = new compiler.Run()
      //println("source: "+objectSourceCode) //DEBUG
      objRun.compileSources(
        List(new BatchSourceFile("<console>", objectSourceCode.toCharArray))
      )
      if (reporter.hasErrors) return false


      // extract and remember types
      typeOf = findTypes(objRun)

      // compile the result-extraction object
      new compiler.Run().compileSources(
        List(new BatchSourceFile("<console>", resultObjectSourceCode.toCharArray))
      )

      // success
      !reporter.hasErrors
    }

    /** Dig the types of all bound variables out of the compiler run.
     *
     *  @param objRun ...
     *  @return       ...
     */
    def findTypes(objRun: compiler.Run): Map[Name, String] = {
      def getTypes(names: List[Name], nameMap: Name=>Name): Map[Name, String] = {
      /** the outermost wrapper object */
      val outerResObjSym: Symbol =
        compiler.definitions.getMember(compiler.definitions.EmptyPackage,
          compiler.newTermName(objectName))

      /** the innermost object inside the wrapper, found by
        * following accessPath into the outer one. */
      val resObjSym =
        (accessPath.split("\\.")).foldLeft(outerResObjSym)((sym,name) =>
          if(name == "") sym else
            compiler.atPhase(objRun.typerPhase.next) {
              sym.info.member(compiler.newTermName(name)) })

      names.foldLeft(Map.empty[Name,String])((map, name) => {
          val rawType =
            compiler.atPhase(objRun.typerPhase.next) {
              resObjSym.info.member(name).tpe
            }

          // the types are all =>T; remove the =>
          val cleanedType= rawType match {
            case compiler.PolyType(Nil, rt) => rt
            case rawType => rawType
          }

          map + (name -> compiler.atPhase(objRun.typerPhase.next) { cleanedType.toString })
        })
      }

      val names1 = getTypes(valAndVarNames, n => compiler.nme.getterToLocal(n))
      val names2 = getTypes(defNames, identity)
      names1 ++ names2
    }

    /** load and run the code using reflection */
    def loadAndRun: (String, Boolean) = {
      val interpreterResultObject: Class[_] =
        Class.forName(resultObjectName, true, classLoader)
      val resultValMethod: java.lang.reflect.Method =
        interpreterResultObject.getMethod("result", null)
      try {
        (resultValMethod.invoke(interpreterResultObject, null).toString(),
             true)
      } catch {
        case e =>
          def caus(e: Throwable): Throwable =
            if (e.getCause eq null) e else caus(e.getCause)
          val orig = caus(e)
          (stringFrom(str => orig.printStackTrace(str)), false)
      }
    }

    /** return a summary of the defined methods */
    def defTypesSummary: String =
      stringFrom(summ => {
        for (methname <- defNames)
          summ.println("" + methname + ": " +
		       string2code(typeOf(methname)))
      })
  }

  /** A sequence of definition's.  val's, var's, def's. */
  private class DefReq(line: String, lineName: String)
  extends Request(line, lineName)

  /** Assignment of a single variable: lhs = exp */
  private class AssignReq(val lhs: Name, line: String, lineName: String)
  extends Request(line, lineName) {
    override val needsVarName = true

    /** Perform the assignment, and then return the new value */
    override def toCompute = "{\n" + line + "\n;\n" + lhs + "\n}"

    /** Print out lhs instead of the generated varName */
    override def resultExtractionCode(code: PrintWriter) {
      code.print(" + \"" + lhs + ": " +
		 string2code(typeOf(compiler.encode(varName))) +
                 " = \" + " +
		 string2code(fullPath(varName))
		 + " + \"\\n\"")
//      override def resultExtractionCode(code: PrintWriter) {
//        {wrapperObj; lhs}
//    }
    }
  }

  /** A single expression */
  private class ExprReq(line: String, lineName: String)
  extends Request(line, lineName) {
    override val needsVarName = true

    /** Skip the printout if the expression has type Unit */
    override def resultExtractionCode(code: PrintWriter) {
      if (typeOf(compiler.encode(varName)) != "Unit")
	super.resultExtractionCode(code)
    }
  }

  /** A module definition */
  private class ModuleReq(line: String, lineName: String)
  extends Request(line, lineName) {
    def moduleName = trees match {
      case List(ModuleDef(_, name, _)) => name
    }
    override def resultExtractionCode(code: PrintWriter) {
      super.resultExtractionCode(code)
      code.println(" + \"defined module " +
		   string2code(moduleName)
		   + "\\n\"")
    }
  }

  /** A class definition */
  private class ClassReq(line: String, lineName: String)
  extends Request(line, lineName) {
    def newClassName = trees match {
      case List(ClassDef(_, name, _, _)) => name
    }

    def classdef = trees.head.asInstanceOf[ClassDef]

    // TODO: MemberDef.keyword does not include "trait";
    // otherwise it could be used here
    def keyword: String =
      if (classdef.mods.isTrait) "trait" else "class"

    override def resultExtractionCode(code: PrintWriter) {
      super.resultExtractionCode(code)
      code.print(
          " + \"defined " +
          keyword +
          " " +
          string2code(newClassName) +
          "\\n\"")
    }
  }

  /** a type alias */
  private class TypeAliasReq(line: String, lineName: String)
  extends Request(line, lineName) {
    def newTypeName = trees match {
      case List(TypeDef(_, name, _, _)) => name
    }

    override def resultExtractionCode(code: PrintWriter) {
      super.resultExtractionCode(code)
      code.println(" + \"defined type alias " + newTypeName + "\\n\"")
    }
  }

  /** an import */
  private class ImportReq(line: String, lineName: String)
  extends Request(line, lineName) {
    override val boundNames = Nil
    override def resultExtractionCode(code: PrintWriter) {
      code.println("+ \"" + trees.head.toString + "\\n\"")
    }

    /** Whether this import includes a wildcard import */
    def importsWildcard =
      trees.exists {
        case Import(_, selectors) =>
          selectors.map(_._1).contains(nme.USCOREkw)
        case _ => false
      }

    /** The individual names imported by this statement */
    def importedNames: Seq[Name] =
      for {
        val Import(_, selectors) <- trees
        val (_,sel) <- selectors
        sel != null
        sel != nme.USCOREkw
        val name <- List(sel.toTypeName, sel.toTermName)
      }
      yield name
  }
}

  class NewLinePrintWriter(out: Writer, autoFlush: Boolean)
  extends PrintWriter(out, autoFlush) {
    def this(out: Writer) = this(out, false)
    override def println() { print("\n"); flush() }
  }

/** Utility methods for the Interpreter. */
object Interpreter {

  /** Delete a directory tree recursively.  Use with care!
   */
  private[nsc] def deleteRecursively(path: File) {
    path match  {
      case _ if !path.exists =>
        ()
      case _ if path.isDirectory =>
        for (p <- path.listFiles)
          deleteRecursively(p)
        path.delete
      case _ =>
        path.delete
    }
  }

  /** Heuristically strip interpreter wrapper prefixes
   *  from an interpreter output string.
   */
  def stripWrapperGunk(str: String): String = {
    val wrapregex = "(line[0-9]+\\$object[$.])?(\\$iw[$.])*"
    str.replaceAll(wrapregex, "")
  }

  /** Convert a string into code that can recreate the string.
   *  This requires replacing all special characters by escape
   *  codes. It does not add the surrounding " marks.  */
  def string2code(str: String): String = {
    /** Convert a character to a backslash-u escape */
    def char2uescape(c: Char): String = {
      var rest = c.toInt
      val buf = new StringBuilder
      for (i <- 1 to 4) {
	buf ++= (rest % 16).toHexString
	rest = rest / 16
      }
      "\\" + "u" + buf.toString.reverse
    }


    val res = new StringBuilder
    for (c <- str) {
      if ("'\"\\" contains c) {
	res += '\\'
	res += c
      } else if (!c.isControl) {
	res += c
      } else {
	res ++= char2uescape(c)
      }
    }
    res.toString
  }
}
