/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.lang.{Class, ClassLoader}
import java.io.{File, PrintWriter, StringWriter}
import java.net.{URL, URLClassLoader}

import scala.collection.mutable.{ListBuffer, HashSet, ArrayBuffer}
import scala.collection.immutable.{Map, ListMap}

import ast.parser.SyntaxAnalyzer
import io.PlainFile
import reporters.{ConsoleReporter, Reporter}
import symtab.Flags
import util.SourceFile
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
 */
class Interpreter(val settings: Settings, reporter: Reporter, out: PrintWriter) {
  import symtab.Names
  import compiler.Traverser
  import compiler.{Tree, TermTree,
                   ValOrDefDef, ValDef, DefDef, Assign,
                   ClassDef, ModuleDef, Ident, Select, AliasTypeDef,
                   Import}
  import compiler.CompilationUnit
  import compiler.Symbol
  import compiler.Name

  /** construct an interpreter that reports to Console */
  def this(settings: Settings) =
    this(settings,
         new ConsoleReporter(settings),
         new PrintWriter(new ConsoleWriter, true))

  /** construct an interpreter that uses the specified in and out streams */
  def this(settings: Settings, out: PrintWriter) =
    this(settings, new ConsoleReporter(settings, null, out), out)

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

  /** directory to save .class files to */
  val classfilePath = File.createTempFile("scalaint", "")
  classfilePath.delete  // the file is created as a file; make it a directory
  classfilePath.mkdirs

  /* set up the compiler's output directory */
  settings.outdir.value = classfilePath.getPath

  /** the compiler to compile expressions with */
  val compiler: Global = new Global(settings, reporter)

  /** the compiler's classpath, as URL's */
  val compilerClasspath: List[URL] =
    compiler.settings.classpath.value.split(File.pathSeparator).toList.
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

  /** XXX Let's get rid of this.  I believe the Eclipse plugin is
    * the only user of it, so this should be doable.  */
  protected def parentClassLoader: ClassLoader = null

  /** the previous requests this interpreter has processed */
  private val prevRequests = new ArrayBuffer[Request]()

  /** look up the request that bound a specified term or type */
  private def reqBinding(vname: Name): Option[Request] =
    prevRequests.toList.reverse.find(lin => lin.boundNames.contains(vname))

  /** next line number to use */
  private var nextLineNo = 0

  /** allocate a fresh line name */
  private def newLineName = {
    val num = nextLineNo
    nextLineNo = nextLineNo + 1
    (compiler.nme.INTERPRETER_LINE_PREFIX +
      (if (num>=0) num.toString else "_neg" + (-num).toString))
  }

  /** import statements that should be used for submitted code */
  private def importLines: List[String] =
    for {
      val req <- prevRequests.toList
      req.isInstanceOf[ImportReq]
    }
    yield req.line

  //private var importLinesRev: List[String] = List("import scala.collection.immutable._")

  /** a string of import code corresponding to all of the current importLines */
  private def codeForImports: String = importLines.mkString("", ";\n", ";\n")

  /** generate a string using a routine that wants to write on a stream */
  private def stringFrom(writer: PrintWriter => Unit): String = {
    val stringWriter = new StringWriter()
    val stream = new PrintWriter(stringWriter)
    writer(stream)
    stream.close
    stringWriter.toString
  }

  /** Parse a line into a sequence of trees. Returns None if the input
    * is incomplete. */
  private def parse(line: String): Option[List[Tree]] = {
    var justNeedsMore = false
    reporter.withIncompleteHandler((pos,msg) => {justNeedsMore = true}) {
      // simple parse: just parse it, nothing else
      def simpleParse(code: String): List[Tree] = {
        val unit =
          new CompilationUnit(
            new SourceFile("<console>", code.toCharArray()))
        new compiler.syntaxAnalyzer.Parser(unit).templateStatSeq._2
      }

      // parse the main code along with the imports
      reporter.reset
      val trees = simpleParse(codeForImports + line)
      if (justNeedsMore)
        None
      else if (reporter.hasErrors)
        Some(Nil) // the result did not parse, so stop
      else {
        // parse the imports alone
        val importTrees = simpleParse(codeForImports)

        // return just the new trees, not the import trees
        Some(trees.drop(importTrees.length))
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
    compileSources(List(new SourceFile("<script>", code.toCharArray)))

  /** Build a request from the user. <code>trees</code> is <code>line</code>
   *  after being parsed.
   *
   *  @param trees    ..
   *  @param line     ..
   *  @param lineName ..
   *  @return         ..
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
      case List(_:AliasTypeDef) => new TypeAliasReq(line, lineName)
      case List(_:Import) => new ImportReq(line, lineName)
      case _ => {
        reporter.error(null, "That kind of statement combination is not supported by the interpreter.")
        null
      }
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
    // parse
    val trees = parse(line) match {
      case None => return IR.Incomplete
      case Some(Nil) => return IR.Error // parse error or empty input
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
      out.print(interpreterResultString)

      // print out types of functions; they are not printed in the
      // request printout
      out.print(req.defTypesSummary)
    }

    // book-keeping
    if (succeeded)
      prevRequests += req

    if (succeeded) IR.Success else IR.Error
  }

  /** A counter used for numbering objects created by bind() */
  private var binderNum = 0

  /** Bind a specified name to a specified value.  The name may
   *  later be used by expressions passed to interpret.
   *
   *  @param name      ...
   *  @param boundType ...
   *  @param value     ...
   *  @return          ...
   */
  def bind(name: String, boundType: String, value: Any) = {
    val binderName = "binder" + binderNum
    binderNum = binderNum + 1

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


  /** Make a dry run in order to fill caches.  This is useful
    * for interactive interpreters so that the interpreter responds
    * quickly to the first user-supplied query.
    */
  def prime: Unit =
    if(prevRequests.isEmpty)
      beQuietDuring {
        nextLineNo = -1  // cosmetic: make the first user-requested line
                         // be number 0
        interpret("0")
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
   */
  def close: Unit =
    Interpreter.deleteRecursively(classfilePath)

  /** A traverser that finds all mentioned identifiers, i.e. things
   *  that need to be imported. It might return extra names.
   */
  private class ImportVarsTraverser(definedVars: List[Name]) extends Traverser {
    val importVars = new HashSet[Name]()

    override def traverse(ast: Tree): unit = ast match {
      case Ident(name) => importVars += name
      case _ => super.traverse(ast)
    }
  }

  /** One line of code submitted by the user for interpretation */
  private abstract class Request(val line: String, val lineName: String) {
    val Some(trees) = parse(line)

    /** name to use for the object that will compute "line" */
    def objectName = lineName + compiler.nme.INTERPRETER_WRAPPER_SUFFIX  // make it unlikely to clash with user variables

    /** name of the object that retrieves the result from the above object */
    def resultObjectName = "RequestResult$" + objectName

    /** whether the trees need a variable name, as opposed to standing
        alone */
    def needsVarName: Boolean = false

    /** list of methods defined */
    val defNames =
      for {
        val DefDef(mods, name, _, _, _, _) <- trees
        mods.isPublic
      } yield name

    /** list of val's and var's defined */
    val valAndVarNames = {
      val baseNames =
        for {
          val ValDef(mods, name, _, _) <- trees
          mods.isPublic
        } yield name

      if (needsVarName)
        compiler.encode(lineName) :: baseNames  // add a var name
      else
        baseNames
    }

    /** list of modules defined */
    val moduleNames = {
      val explicit =
        for(val ModuleDef(mods, name, _) <- trees; mods.isPublic)
          yield name
      val caseClasses =
        for {val ClassDef(mods, name, _, _, _) <- trees
             mods.isPublic
             mods.hasFlag(Flags.CASE)}
        yield name.toTermName
      explicit ::: caseClasses
    }

    /** list of classes defined */
    val classNames =
      for (val ClassDef(mods, name, _, _, _) <- trees; mods.isPublic)
        yield name

    /** list of type aliases defined */
    val typeNames =
      for (val AliasTypeDef(mods, name, _, _) <- trees; mods.isPublic)
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

    /** names to print out to the user after evaluation */
    def namesToPrintForUser = valAndVarNames

    /** generate the source code for the object that computes this request */
    def objectSourceCode: String =
      stringFrom(code => {
        // add the user-specified imports first
        code.println(codeForImports)

        // object header
        code.println("object " + objectName + " {")

        // Write an import for each imported variable.
        // Note that the imports are inside the object wrapper; otherwise,
        // the names defined at the package level will override these
        // imported values.
        for {val imv <- usedNames
             val lastDefiner <- reqBinding(imv).toList } {
          code.println("import " + lastDefiner.objectName + "." + imv)
        }


        // the line of code to compute
        if (needsVarName)
          code.println("  val " + lineName + " = " + line)
        else
          code.println("  " + line)

        //end
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
        code.println(objectName + ";")  // evaluate the object, to make sure its constructor is run
        code.print("\"\"")  // print an initial empty string, so later code can
                            // uniformly be: + morestuff
        resultExtractionCode(code)
        code.println("}")
        code.println(";}")
      })

    def resultExtractionCode(code: PrintWriter): Unit =
      for (val vname <- namesToPrintForUser) {
        code.print(" + \"" + vname + ": " + typeOf(vname) +
                   " = \" + " + objectName + "." + vname + " + \"\\n\"")
      }

    /** Compile the object file.  Returns whether the compilation succeeded.
        If all goes well, types is computed and set */
    def compile: Boolean = {
      reporter.reset  // without this, error counting is not correct,
                      // and the interpreter sometimes overlooks compile failures!

      // compile the main object
      val objRun = new compiler.Run()
      //Console.println("source: "+objectSourceCode) //DEBUG
      objRun.compileSources(
        List(new SourceFile("<console>", objectSourceCode.toCharArray))
      )
      if (reporter.hasErrors) return false

      // extract and remember types
      typeOf = findTypes(objRun)

      // compile the result-extraction object
      new compiler.Run().compileSources(
        List(new SourceFile("<console>", resultObjectSourceCode.toCharArray))
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
        names.foldLeft[Map[Name,String]](new ListMap[Name, String]())((map, name) => {
          val resObjSym: Symbol =
            compiler.definitions.getMember(compiler.definitions.EmptyPackage,
              compiler.newTermName(objectName))

          val typeString =
            compiler.atPhase(objRun.typerPhase.next) {
              resObjSym.info.decls.toList.find(s =>
                s.name == nameMap(name)).get.tpe.toString()
            }

          map + name -> typeString
        })
      }

      val names1 = getTypes(valAndVarNames, n=>compiler.nme.getterToLocal(n))
      val names2 = getTypes(defNames, identity)
      names1.incl(names2)
    }

    /** load and run the code using reflection */
    def loadAndRun: (String, Boolean) = {
      val interpreterResultObject: Class =
        Class.forName(resultObjectName, true, classLoader)
      val resultValMethod: java.lang.reflect.Method =
        interpreterResultObject.getMethod("result", null)
      try {
        (resultValMethod.invoke(interpreterResultObject, null).toString(),
             true)
      } catch {
        case e => {
          def caus(e: Throwable): Throwable =
            if (e.getCause eq null) e else caus(e.getCause)
            val orig = caus(e)
            (stringFrom(str => orig.printStackTrace(str)),
                 false)
        }
      }
    }

    /** return a summary of the defined methods */
    def defTypesSummary: String =
      stringFrom(summ => {
        for (val methname <- defNames)
          summ.println("" + methname + ": " + typeOf(methname))
      })
  }

  /** A sequence of definition's.  val's, var's, def's. */
  private class DefReq(line: String, lineName: String)
  extends Request(line, lineName)

  /** Assignment of a single variable: lhs = exp */
  private class AssignReq(val lhs: Name, line: String, lineName: String)
  extends Request(line, lineName) {
    override def resultExtractionCode(code: PrintWriter): Unit = {
      super.resultExtractionCode(code)
      val bindReq = reqBinding(lhs).get
      code.println(" + \"" + lhs + " = \" + " + bindReq.objectName + "." + lhs)
    }
    override def namesToPrintForUser = Nil
  }

  /** A single expression */
  private class ExprReq(line: String, lineName: String)
  extends Request(line, lineName) {
    override val needsVarName = true
  }

  /** A module definition */
  private class ModuleReq(line: String, lineName: String)
  extends Request(line, lineName) {
    def moduleName = trees match {
      case List(ModuleDef(_, name, _)) => name
    }
    override def resultExtractionCode(code: PrintWriter): Unit = {
      super.resultExtractionCode(code)
      code.println(" + \"defined module " + moduleName + "\\n\"")
    }
  }

  /** A class definition */
  private class ClassReq(line: String, lineName: String)
  extends Request(line, lineName) {
    def newClassName = trees match {
      case List(ClassDef(_, name, _, _, _)) => name
    }

    def keyword = trees.head.asInstanceOf[ClassDef].keyword

    override def resultExtractionCode(code: PrintWriter): Unit = {
      super.resultExtractionCode(code)
      code.print(
          " + \"defined " +
          keyword +
          " " +
          newClassName +
          "\\n\"")
    }
  }

  /** a type alias */
  private class TypeAliasReq(line: String, lineName: String)
  extends Request(line, lineName) {
    def newTypeName = trees match {
      case List(AliasTypeDef(_, name, _, _)) => name
    }

    override def resultExtractionCode(code: PrintWriter): Unit = {
      super.resultExtractionCode(code)
      code.println(" + \"defined type alias " + newTypeName + "\\n\"")
    }
  }

  /** an import */
  private class ImportReq(line: String, lineName: String)
  extends Request(line, lineName) {
    override val boundNames = Nil
    override val usedNames = Nil
    override def resultExtractionCode(code: PrintWriter): Unit = {
      code.println("+ \"" + line + "\"")
    }
  }
}

/** The object <code>Interpreter</code> ...
 */
object Interpreter {

  /** Delete a directory tree recursively.  Use with care!
   *
   *  @param path ...
   */
  def deleteRecursively(path: File): Unit = {
    path match  {
      case _ if !path.exists => {}
      case _ if path.isDirectory =>
        for (val p <- path.listFiles)
          deleteRecursively(p)
        path.delete
      case _ => path.delete
    }
  }

}
