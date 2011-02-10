/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package interpreter

import Predef.{ println => _, _ }
import java.io.{ PrintWriter }
import java.lang.reflect
import java.net.URL
import util.{ Set => _, _ }
import io.VirtualDirectory
import reporters.{ ConsoleReporter, Reporter }
import symtab.{ Flags, Names }
import scala.tools.nsc.interpreter.{ Results => IR }
import scala.tools.util.PathResolver
import scala.tools.nsc.util.{ ScalaClassLoader, Exceptional }
import ScalaClassLoader.URLClassLoader
import Exceptional.unwrap
import scala.collection.{ mutable, immutable }
import scala.PartialFunction.{ cond, condOpt }
import scala.util.control.Exception.{ ultimately }
import scala.reflect.NameTransformer
import IMain._

/** An interpreter for Scala code.
 *
 *  The main public entry points are compile(), interpret(), and bind().
 *  The compile() method loads a complete Scala file.  The interpret() method
 *  executes one line of Scala code at the request of the user.  The bind()
 *  method binds an object to a variable that can then be used by later
 *  interpreted code.
 *
 *  The overall approach is based on compiling the requested code and then
 *  using a Java classloader and Java reflection to run the code
 *  and access its results.
 *
 *  In more detail, a single compiler instance is used
 *  to accumulate all successfully compiled or interpreted Scala code.  To
 *  "interpret" a line of code, the compiler generates a fresh object that
 *  includes the line of code and which has public member(s) to export
 *  all variables defined by that code.  To extract the result of an
 *  interpreted line to show the user, a second "result object" is created
 *  which imports the variables exported by the above object and then
 *  exports a single member named "$export".  To accomodate user expressions
 *  that read from variables or methods defined in previous statements, "import"
 *  statements are used.
 *
 *  This interpreter shares the strengths and weaknesses of using the
 *  full compiler-to-Java.  The main strength is that interpreted code
 *  behaves exactly as does compiled code, including running at full speed.
 *  The main weakness is that redefining classes and methods is not handled
 *  properly, because rebinding at the Java level is technically difficult.
 *
 *  @author Moez A. Abdel-Gawad
 *  @author Lex Spoon
 */
class IMain(val settings: Settings, protected val out: PrintWriter) {
  imain =>

  /** construct an interpreter that reports to Console */
  def this(settings: Settings) = this(settings, new NewLinePrintWriter(new ConsoleWriter, true))
  def this() = this(new Settings())

  /** whether to print out result lines */
  private[nsc] var printResults: Boolean = true

  /** whether to print errors */
  private[nsc] var totalSilence: Boolean = false

  private val RESULT_OBJECT_PREFIX = "RequestResult$"

  lazy val formatting: Formatting = new Formatting {
    val prompt = Properties.shellPromptString
  }
  import formatting._

  /** directory to save .class files to */
  val virtualDirectory = new VirtualDirectory("(memory)", None)

  /** reporter */
  lazy val reporter: ConsoleReporter = new IMain.ReplReporter(this)
  import reporter.{ printMessage, withoutTruncating }
  // not sure if we have some motivation to print directly to console
  private def echo(msg: String) { Console println msg }

  /** We're going to go to some trouble to initialize the compiler asynchronously.
   *  It's critical that nothing call into it until it's been initialized or we will
   *  run into unrecoverable issues, but the perceived repl startup time goes
   *  through the roof if we wait for it.  So we initialize it with a future and
   *  use a lazy val to ensure that any attempt to use the compiler object waits
   *  on the future.
   */
  private val _compiler: Global = newCompiler(settings, reporter)
  private var _initializeComplete = false
  def isInitializeComplete = _initializeComplete

  private def _initialize(): Boolean = {
    val source = """
      |// this is assembled to force the loading of approximately the
      |// classes which will be loaded on the first expression anyway.
      |class $repl_$init {
      |  val x = "abc".reverse.length + (5 max 5)
      |  scala.runtime.ScalaRunTime.stringOf(x)
      |}
      |""".stripMargin

    val result = try {
      new _compiler.Run() compileSources List(new BatchSourceFile("<init>", source))
      if (isReplDebug || settings.debug.value) {
        // Can't use printMessage here, it deadlocks
        Console.println("Repl compiler initialized.")
      }
      true
    }
    catch {
      case x: AbstractMethodError =>
        printMessage("""
          |Failed to initialize compiler: abstract method error.
          |This is most often remedied by a full clean and recompile.
          |""".stripMargin
        )
        x.printStackTrace()
        false
      case x: MissingRequirementError => printMessage("""
        |Failed to initialize compiler: %s not found.
        |** Note that as of 2.8 scala does not assume use of the java classpath.
        |** For the old behavior pass -usejavacp to scala, or if using a Settings
        |** object programatically, settings.usejavacp.value = true.""".stripMargin.format(x.req)
      )
      false
    }

    try result
    finally _initializeComplete = result
  }

  // set up initialization future
  private var _isInitialized: () => Boolean = null
  def initialize() = synchronized {
    if (_isInitialized == null)
      _isInitialized = scala.concurrent.ops future _initialize()
  }

  /** the public, go through the future compiler */
  lazy val global: Global = {
    initialize()

    // blocks until it is ; false means catastrophic failure
    if (_isInitialized()) _compiler
    else null
  }
  @deprecated("Use `global` for access to the compiler instance.")
  lazy val compiler = global

  import global._
  import definitions.{ ScalaPackage, JavaLangPackage, PredefModule, RootClass }
  import nme.{ INTERPRETER_IMPORT_WRAPPER }

  object naming extends {
    val global: imain.global.type = imain.global
  } with Naming {
    // make sure we don't overwrite their unwisely named res3 etc.
    override def freshUserVarName(): String = {
      val name = super.freshUserVarName()
      if (definedNameMap contains name) freshUserVarName()
      else name
    }
  }
  import naming._

  // object dossiers extends {
  //   val intp: imain.type = imain
  // } with Dossiers { }
  // import dossiers._

  lazy val memberHandlers = new {
    val intp: imain.type = imain
  } with MemberHandlers
  import memberHandlers._

  def atPickler[T](op: => T): T = atPhase(currentRun.picklerPhase)(op)
  def afterTyper[T](op: => T): T = atPhase(currentRun.typerPhase.next)(op)

  /** Temporarily be quiet */
  def beQuietDuring[T](operation: => T): T = {
    val wasPrinting = printResults
    ultimately(printResults = wasPrinting) {
      if (isReplDebug) echo(">> beQuietDuring")
      else printResults = false

      operation
    }
  }
  def beSilentDuring[T](operation: => T): T = {
    val saved = totalSilence
    totalSilence = true
    try operation
    finally totalSilence = saved
  }

  def quietRun[T](code: String) = beQuietDuring(interpret(code))

  /** whether to bind the lastException variable */
  private var bindLastException = true

  /** A string representing code to be wrapped around all lines. */
  private var _executionWrapper: String = ""
  def executionWrapper = _executionWrapper
  def setExecutionWrapper(code: String) = _executionWrapper = code
  def clearExecutionWrapper() = _executionWrapper = ""

  /** Temporarily stop binding lastException */
  def withoutBindingLastException[T](operation: => T): T = {
    val wasBinding = bindLastException
    ultimately(bindLastException = wasBinding) {
      bindLastException = false
      operation
    }
  }

  protected def createLineManager(): Line.Manager = new Line.Manager
  lazy val lineManager = createLineManager()

  /** interpreter settings */
  lazy val isettings = new ISettings(this)

  /** Instantiate a compiler.  Subclasses can override this to
   *  change the compiler class used by this interpreter. */
  protected def newCompiler(settings: Settings, reporter: Reporter) = {
    settings.outputDirs setSingleOutput virtualDirectory
    settings.exposeEmptyPackage.value = true
    new Global(settings, reporter)
  }

  /** the compiler's classpath, as URL's */
  lazy val compilerClasspath: List[URL] = new PathResolver(settings) asURLs

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
  private var _classLoader: AbstractFileClassLoader = null
  def resetClassLoader() = _classLoader = makeClassLoader()
  def classLoader: AbstractFileClassLoader = {
    if (_classLoader == null)
      resetClassLoader()

    _classLoader
  }
  private def makeClassLoader(): AbstractFileClassLoader = {
    val parent =
      if (parentClassLoader == null)  ScalaClassLoader fromURLs compilerClasspath
      else                            new URLClassLoader(compilerClasspath, parentClassLoader)

    new AbstractFileClassLoader(virtualDirectory, parent)
  }
  private def loadByName(s: String): Class[_] =
    (classLoader tryToInitializeClass s) getOrElse sys.error("Failed to load expected class: '" + s + "'")

  protected def parentClassLoader: ClassLoader =
    settings.explicitParentLoader.getOrElse( this.getClass.getClassLoader() )

  def getInterpreterClassLoader() = classLoader

  // Set the current Java "context" class loader to this interpreter's class loader
  def setContextClassLoader() = classLoader.setAsContext()

  /** Given a simple repl-defined name, returns the real name of
   *  the class representing it, e.g. for "Bippy" it may return
   *
   *    $line19.$read$$iw$$iw$$iw$$iw$$iw$$iw$$iw$$iw$Bippy
   */
  def pathToFlatName(id: String): String = {
    requestForIdent(id) match {
      case Some(req)    => req fullFlatName id
      case _            => id
    }
  }

  def allDefinedNames = definedNameMap.keys.toList sortBy (_.toString)
  def pathToType(id: String): String = pathToName(newTypeName(id))
  def pathToTerm(id: String): String = pathToName(newTermName(id))
  def pathToName(name: Name): String = {
    if (definedNameMap contains name)
      definedNameMap(name) fullPath name
    else name.toString
  }

  /** Most recent tree handled which wasn't wholly synthetic. */
  private def mostRecentlyHandledTree: Option[Tree] = {
    prevRequests.reverse foreach { req =>
      req.handlers.reverse foreach {
        case x: MemberDefHandler if x.definesValue && !isInternalVarName(x.name)  => return Some(x.member)
        case _ => ()
      }
    }
    None
  }

  /** Stubs for work in progress. */
  def handleTypeRedefinition(name: TypeName, old: Request, req: Request) = {
    for (t1 <- old.simpleNameOfType(name) ; t2 <- req.simpleNameOfType(name)) {
      DBG("Redefining type '%s'\n  %s -> %s".format(name, t1, t2))
    }
  }

  def handleTermRedefinition(name: TermName, old: Request, req: Request) = {
    for (t1 <- old.compilerTypeOf get name ; t2 <- req.compilerTypeOf get name) {
      // Printing the types here has a tendency to cause assertion errors, like
      //   assertion failed: fatal: <refinement> has owner value x, but a class owner is required
      // so DBG is by-name now to keep it in the family.  (It also traps the assertion error,
      // but we don't want to unnecessarily risk hosing the compiler's internal state.)
      DBG("Redefining term '%s'\n  %s -> %s".format(name, t1, t2))
    }
  }
  def recordRequest(req: Request) {
    if (req == null || referencedNameMap == null)
      return

    prevRequests += req
    req.referencedNames foreach (x => referencedNameMap(x) = req)

    req.definedNames foreach { name =>
      if (definedNameMap contains name) {
        if (name.isTypeName) handleTypeRedefinition(name.toTypeName, definedNameMap(name), req)
        else handleTermRedefinition(name.toTermName, definedNameMap(name), req)
      }
      definedNameMap(name) = req
    }
  }

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
  private case class ComputedImports(prepend: String, append: String, access: String)
  private def importsCode(wanted: Set[Name]): ComputedImports = {
    /** Narrow down the list of requests from which imports
     *  should be taken.  Removes requests which cannot contribute
     *  useful imports for the specified set of wanted names.
     */
    case class ReqAndHandler(req: Request, handler: MemberHandler) { }

    def reqsToUse: List[ReqAndHandler] = {
      /** Loop through a list of MemberHandlers and select which ones to keep.
        * 'wanted' is the set of names that need to be imported.
       */
      def select(reqs: List[ReqAndHandler], wanted: Set[Name]): List[ReqAndHandler] = {
        val isWanted = wanted contains _
        // Single symbol imports might be implicits! See bug #1752.  Rather than
        // try to finesse this, we will mimic all imports for now.
        def keepHandler(handler: MemberHandler) = handler match {
          case _: ImportHandler => true
          case x                => x.definesImplicit || (x.definedNames exists isWanted)
        }

        reqs match {
          case Nil                                    => Nil
          case rh :: rest if !keepHandler(rh.handler) => select(rest, wanted)
          case rh :: rest                             =>
            import rh.handler._
            val newWanted = wanted ++ referencedNames -- definedNames -- importedNames
            rh :: select(rest, newWanted)
        }
      }

      /** Flatten the handlers out and pair each with the original request */
      select(allReqAndHandlers reverseMap { case (r, h) => ReqAndHandler(r, h) }, wanted).reverse
    }

    val code, trailingBraces, accessPath = new StringBuilder
    val currentImps = mutable.HashSet[Name]()

    // add code for a new object to hold some imports
    def addWrapper() {
      val impname = INTERPRETER_IMPORT_WRAPPER
      code append "object %s {\n".format(impname)
      trailingBraces append "}\n"
      accessPath append ("." + impname)

      currentImps.clear
    }

    addWrapper()

    // loop through previous requests, adding imports for each one
    for (ReqAndHandler(req, handler) <- reqsToUse) {
      handler match {
        // If the user entered an import, then just use it; add an import wrapping
        // level if the import might conflict with some other import
        case x: ImportHandler =>
          if (x.importsWildcard || (currentImps exists (x.importedNames contains _)))
            addWrapper()

          code append (x.member + "\n")

          // give wildcard imports a import wrapper all to their own
          if (x.importsWildcard) addWrapper()
          else currentImps ++= x.importedNames

        // For other requests, import each defined name.
        // import them explicitly instead of with _, so that
        // ambiguity errors will not be generated. Also, quote
        // the name of the variable, so that we don't need to
        // handle quoting keywords separately.
        case x =>
          for (imv <- x.definedNames) {
            if (currentImps contains imv) addWrapper()

            code append ("import %s\n" format (req fullPath imv))
            currentImps += imv
          }
      }
    }
    // add one extra wrapper, to prevent warnings in the common case of
    // redefining the value bound in the last interpreter request.
    addWrapper()
    ComputedImports(code.toString, trailingBraces.toString, accessPath.toString)
  }

  /** Parse a line into a sequence of trees. Returns None if the input is incomplete. */
  def parse(line: String): Option[List[Tree]] = {
    var justNeedsMore = false
    reporter.withIncompleteHandler((pos,msg) => {justNeedsMore = true}) {
      // simple parse: just parse it, nothing else
      def simpleParse(code: String): List[Tree] = {
        reporter.reset()
        val unit = new CompilationUnit(new BatchSourceFile("<console>", code))
        val scanner = new syntaxAnalyzer.UnitParser(unit)

        scanner.templateStatSeq(false)._2
      }
      val trees = simpleParse(line)

      if (reporter.hasErrors)   Some(Nil)  // the result did not parse, so stop
      else if (justNeedsMore)   None
      else                      Some(trees)
    }
  }

  def isParseable(line: String): Boolean = {
    beSilentDuring {
      parse(line) match {
        case Some(xs) => xs.nonEmpty  // parses as-is
        case None     => true         // incomplete
      }
    }
  }

  /** Compile an nsc SourceFile.  Returns true if there are
   *  no compilation errors, or false otherwise.
   */
  def compileSources(sources: SourceFile*): Boolean = {
    reporter.reset()
    new Run() compileSources sources.toList
    !reporter.hasErrors
  }

  /** Compile a string.  Returns true if there are no
   *  compilation errors, or false otherwise.
   */
  def compileString(code: String): Boolean =
    compileSources(new BatchSourceFile("<script>", code))


  /** Build a request from the user. `trees` is `line` after being parsed.
   */
  private def buildRequest(line: String, trees: List[Tree]): Request = new Request(line, trees)

  private def requestFromLine(line: String, synthetic: Boolean): Either[IR.Result, Request] = {
    val trees = parse(indentCode(line)) match {
      case None         => return Left(IR.Incomplete)
      case Some(Nil)    => return Left(IR.Error) // parse error or empty input
      case Some(trees)  => trees
    }

    // use synthetic vars to avoid filling up the resXX slots
    def varName = if (synthetic) freshInternalVarName() else freshUserVarName()

    // Treat a single bare expression specially. This is necessary due to it being hard to
    // modify code at a textual level, and it being hard to submit an AST to the compiler.
    if (trees.size == 1) trees.head match {
      case _:Assign                         => // we don't want to include assignments
      case _:TermTree | _:Ident | _:Select  => // ... but do want these as valdefs.
        requestFromLine("val %s =\n%s".format(varName, line), synthetic) match {
          case Right(req) => return Right(req withOriginalLine line)
          case x          => return x
        }
      case _                                =>
    }

    // figure out what kind of request
    Right(buildRequest(line, trees))
  }

  /**
   *    Interpret one line of input.  All feedback, including parse errors
   *    and evaluation results, are printed via the supplied compiler's
   *    reporter.  Values defined are available for future interpreted
   *    strings.
   *
   *
   *    The return value is whether the line was interpreter successfully,
   *    e.g. that there were no parse errors.
   *
   *
   *  @param line ...
   *  @return     ...
   */
  def interpret(line: String): IR.Result = interpret(line, false)
  def interpret(line: String, synthetic: Boolean): IR.Result = {
    def loadAndRunReq(req: Request) = {
      val (result, succeeded) = req.loadAndRun
      /** To our displeasure, ConsoleReporter offers only printMessage,
       *  which tacks a newline on the end.  Since that breaks all the
       *  output checking, we have to take one off to balance.
       */
      def show() = {
        if (result == "") ()
        else printMessage(result stripSuffix "\n")
      }

      if (succeeded) {
        if (printResults)
          show()
        if (!synthetic)  // book-keeping
          recordRequest(req)

        IR.Success
      }
      else {
        // don't truncate stack traces
        withoutTruncating(show())
        IR.Error
      }
    }

    if (global == null) IR.Error
    else requestFromLine(line, synthetic) match {
      case Left(result) => result
      case Right(req)   =>
        // null indicates a disallowed statement type; otherwise compile and
        // fail if false (implying e.g. a type error)
        if (req == null || !req.compile) IR.Error
        else loadAndRunReq(req)
    }
  }

  /** Bind a specified name to a specified value.  The name may
   *  later be used by expressions passed to interpret.
   *
   *  @param name      the variable name to bind
   *  @param boundType the type of the variable, as a string
   *  @param value     the object value to bind to it
   *  @return          an indication of whether the binding succeeded
   */
  def bind(name: String, boundType: String, value: Any): IR.Result = {
    val bindRep = new ReadEvalPrint()
    val run = bindRep.compile("""
        |object %s {
        |  var value: %s = _
        |  def set(x: Any) = value = x.asInstanceOf[%s]
        |}
      """.stripMargin.format(bindRep.evalName, boundType, boundType)
      )
    bindRep.callOpt("set", value) match {
      case Some(_)  => interpret("val %s = %s.value".format(name, bindRep.evalPath))
      case _        => DBG("Set failed in bind(%s, %s, %s)".format(name, boundType, value)) ; IR.Error
    }
  }
  def rebind(p: NamedParam): IR.Result = {
    val name     = p.name
    val oldType  = typeOfTerm(name) getOrElse { return IR.Error }
    val newType  = p.tpe
    val tempName = freshInternalVarName()

    quietRun("val %s = %s".format(tempName, name))
    quietRun("val %s = %s.asInstanceOf[%s]".format(name, tempName, newType))
  }

  def quietBind(p: NamedParam): IR.Result                  = beQuietDuring(bind(p))
  def bind(p: NamedParam): IR.Result                       = bind(p.name, p.tpe, p.value)
  def bind[T: Manifest](name: String, value: T): IR.Result = bind((name, value))
  def bindValue(x: Any): IR.Result                         = bind(freshUserVarName(), TypeStrings.fromValue(x), x)

  /** Reset this interpreter, forgetting all user-specified requests. */
  def reset() {
    virtualDirectory.clear()
    resetClassLoader()
    resetAllCreators()
    prevRequests.clear
  }

  /** This instance is no longer needed, so release any resources
   *  it is using.  The reporter's output gets flushed.
   */
  def close() {
    reporter.flush
  }

  /** Here is where we:
   *
   *  1) Read some source code, and put it in the "read" object.
   *  2) Evaluate the read object, and put the result in the "eval" object.
   *  3) Create a String for human consumption, and put it in the "print" object.
   *
   *  Read! Eval! Print! Some of that not yet centralized here.
   */
  class ReadEvalPrint(lineId: Int) {
    def this() = this(freshLineId())

    val packageName = "$line" + lineId
    val readName    = "$read"
    val evalName    = "$eval"
    val printName   = "$print"
    val valueMethod = "$result"   // no-args method giving result

    // TODO: split it out into a package object and a regular
    // object and we can do that much less wrapping.
    def packageDecl = "package " + packageName

    def pathTo(name: String)   = packageName + "." + name
    def packaged(code: String) = packageDecl + "\n\n" + code

    def readPath  = pathTo(readName)
    def evalPath  = pathTo(evalName)
    def printPath = pathTo(printName)

    def call(name: String, args: Any*): AnyRef =
      evalMethod(name).invoke(evalClass, args.map(_.asInstanceOf[AnyRef]): _*)

    def callOpt(name: String, args: Any*): Option[AnyRef] =
      try Some(call(name, args: _*))
      catch { case ex: Exception =>
        quietBind("lastException", ex)
        None
      }

    lazy val evalClass = loadByName(evalPath)
    lazy val evalValue = callOpt(valueMethod)

    def compile(source: String): Boolean = compileAndSaveRun("<console>", source)
    def lineAfterTyper[T](op: => T): T = {
      assert(lastRun != null, "Internal error: trying to use atPhase, but Run is null." + this)
      atPhase(lastRun.typerPhase.next)(op)
    }

    /** The innermost object inside the wrapper, found by
      * following accessPath into the outer one.
      */
    def resolvePathToSymbol(accessPath: String): Symbol = {
      val readRoot  = definitions.getModule(readPath)   // the outermost wrapper
      (accessPath split '.').foldLeft(readRoot) { (sym, name) =>
        if (name == "") sym else
        lineAfterTyper(sym.info member newTermName(name))
      }
    }

    // def compileAndTypeExpr(expr: String): Option[Typer] = {
    //   class TyperRun extends Run {
    //     override def stopPhase(name: String) = name == "superaccessors"
    //   }
    // }
    private var lastRun: Run = _
    private def evalMethod(name: String) = {
      val methods = evalClass.getMethods filter (_.getName == name)
      assert(methods.size == 1, "Internal error - eval object method " + name + " is overloaded: " + methods)
      methods.head
    }
    private def compileAndSaveRun(label: String, code: String) = {
      showCodeIfDebugging(code)
      reporter.reset()
      lastRun = new Run()
      lastRun.compileSources(List(new BatchSourceFile(label, packaged(code))))
      !reporter.hasErrors
    }
  }

  /** One line of code submitted by the user for interpretation */
  // private
  class Request(val line: String, val trees: List[Tree]) {
    val lineRep     = new ReadEvalPrint()
    import lineRep.lineAfterTyper

    private var _originalLine: String = null
    def withOriginalLine(s: String): this.type = { _originalLine = s ; this }
    def originalLine = if (_originalLine == null) line else _originalLine

    /** handlers for each tree in this request */
    val handlers: List[MemberHandler] = trees map (memberHandlers chooseHandler _)

    /** all (public) names defined by these statements */
    val definedNames = handlers flatMap (_.definedNames)

    /** list of names used by this expression */
    val referencedNames: List[Name] = handlers flatMap (_.referencedNames)

    /** def and val names */
    def termNames = handlers flatMap (_.definesTerm)
    def typeNames = handlers flatMap (_.definesType)

    /** Code to import bound names from previous lines - accessPath is code to
      * append to objectName to access anything bound by request.
      */
    val ComputedImports(importsPreamble, importsTrailer, accessPath) =
      importsCode(referencedNames.toSet)

    /** Code to access a variable with the specified name */
    def fullPath(vname: String) = (
      lineRep.readPath + accessPath + ".`%s`".format(vname)
    )
    /** Same as fullpath, but after it has been flattened, so:
     *  $line5.$iw.$iw.$iw.Bippy      // fullPath
     *  $line5.$iw$$iw$$iw$Bippy      // fullFlatName
     */
    def fullFlatName(name: String) =
      lineRep.readPath + accessPath.replace('.', '$') + "$" + name

    /** Code to access a variable with the specified name */
    def fullPath(vname: Name): String = fullPath(vname.toString)

    /** the line of code to compute */
    def toCompute = line

    /** generate the source code for the object that computes this request */
    private object ObjectSourceCode extends CodeAssembler[MemberHandler] {
      val preamble = """
        |object %s {
        |  %s%s
      """.stripMargin.format(lineRep.readName, importsPreamble, indentCode(toCompute))
      val postamble = importsTrailer + "\n}"
      val generate = (m: MemberHandler) => m extraCodeToEvaluate Request.this
    }

    private object ResultObjectSourceCode extends CodeAssembler[MemberHandler] {
      /** We only want to generate this code when the result
       *  is a value which can be referred to as-is.
       */
      val evalResult =
        if (!handlers.last.definesValue) ""
        else handlers.last.definesTerm match {
          case Some(vname) if typeOf contains vname =>
            """
            |lazy val $result = {
            |  $export
            |  %s
            |}""".stripMargin.format(fullPath(vname))
          case _  => ""
        }
      // first line evaluates object to make sure constructor is run
      // initial "" so later code can uniformly be: + etc
      val preamble = """
      |object %s {
      |  %s
      |  val $export: String = %s {
      |    %s
      |    (""
      """.stripMargin.format(
        lineRep.evalName, evalResult, executionWrapper, lineRep.readName + accessPath
      )

      val postamble = """
      |    )
      |  }
      |}
      """.stripMargin
      val generate = (m: MemberHandler) => m resultExtractionCode Request.this
    }

    // get it
    def getEvalTyped[T] : Option[T] = getEval map (_.asInstanceOf[T])
    def getEval: Option[AnyRef] = {
      // ensure it has been compiled
      compile
      // try to load it and call the value method
      lineRep.evalValue filterNot (_ == null)
    }

    /** Compile the object file.  Returns whether the compilation succeeded.
     *  If all goes well, the "types" map is computed. */
    lazy val compile: Boolean = {
      // error counting is wrong, hence interpreter may overlook failure - so we reset
      reporter.reset()

      // compile the object containing the user's code
      lineRep.compile(ObjectSourceCode(handlers)) && {
        // extract and remember types
        typeOf
        typesOfDefinedTerms

        // compile the result-extraction object
        lineRep compile ResultObjectSourceCode(handlers)
      }
    }

    lazy val resultSymbol = lineRep.resolvePathToSymbol(accessPath)
    def applyToResultMember[T](name: Name, f: Symbol => T) = lineAfterTyper(f(resultSymbol.info.nonPrivateDecl(name)))

    /* typeOf lookup with encoding */
    def lookupTypeOf(name: Name) = typeOf.getOrElse(name, typeOf(global.encode(name.toString)))
    def simpleNameOfType(name: TypeName) = (compilerTypeOf get name) map (_.typeSymbol.simpleName)

    private def typeMap[T](f: Type => T): Map[Name, T] = {
      def toType(name: Name): T = {
        // the types are all =>T; remove the =>
        val tp1 = lineAfterTyper(resultSymbol.info.nonPrivateDecl(name).tpe match {
          case NullaryMethodType(tp)  => tp
          case tp                 => tp
        })
        // normalize non-public types so we don't see protected aliases like Self
        lineAfterTyper(tp1 match {
          case TypeRef(_, sym, _) if !sym.isPublic  => f(tp1.normalize)
          case tp                                   => f(tp)
        })
      }
      termNames ++ typeNames map (x => x -> toType(x)) toMap
    }
    /** Types of variables defined by this request. */
    lazy val compilerTypeOf = typeMap[Type](x => x)
    /** String representations of same. */
    lazy val typeOf         = typeMap[String](_.toString)

    // lazy val definedTypes: Map[Name, Type] = {
    //   typeNames map (x => x -> afterTyper(resultSymbol.info.nonPrivateDecl(x).tpe)) toMap
    // }
    lazy val definedSymbols: Map[Name, Symbol] =
      termNames map (x => x -> applyToResultMember(x, x => x)) toMap

    lazy val typesOfDefinedTerms: Map[Name, Type] =
      termNames map (x => x -> applyToResultMember(x, _.tpe)) toMap

    private def bindExceptionally(t: Throwable) = {
      val ex: Exceptional =
        if (isettings.showInternalStackTraces) Exceptional(t)
        else new Exceptional(t) {
          override def spanFn(frame: JavaStackFrame) = !(frame.className startsWith lineRep.evalPath)
          override def contextPrelude = super.contextPrelude + "/* The repl internal portion of the stack trace is elided. */\n"
        }

      quietBind("lastException", ex)
      ex.contextHead + "\n(access lastException for the full trace)"
    }
    private def bindUnexceptionally(t: Throwable) = {
      quietBind("lastException", t)
      stackTraceString(t)
    }

    /** load and run the code using reflection */
    def loadAndRun: (String, Boolean) = {
      import interpreter.Line._

      def handleException(t: Throwable) = {
        /** We turn off the binding to accomodate ticket #2817 */
        withoutBindingLastException {
          val message =
            if (opt.richExes) bindExceptionally(unwrap(t))
            else bindUnexceptionally(unwrap(t))

          (message, false)
        }
      }

      try {
        val execution = lineManager.set(originalLine)(lineRep call "$export")
        execution.await()

        execution.state match {
          case Done       => ("" + execution.get(), true)
          case Threw      => if (bindLastException) handleException(execution.caught()) else throw execution.caught()
          case Cancelled  => ("Execution interrupted by signal.\n", false)
          case Running    => ("Execution still running! Seems impossible.", false)
        }
      }
      finally lineManager.clear()
    }

    override def toString = "Request(line=%s, %s trees)".format(line, trees.size)
  }

  /** Returns the name of the most recent interpreter result.
   *  Mostly this exists so you can conveniently invoke methods on
   *  the previous result.
   */
  def mostRecentVar: String =
    if (mostRecentlyHandledTree.isEmpty) ""
    else "" + (mostRecentlyHandledTree.get match {
      case x: ValOrDefDef           => x.name
      case Assign(Ident(name), _)   => name
      case ModuleDef(_, name, _)    => name
      case _                        => naming.mostRecentVar
    })

  private def requestForName(name: Name): Option[Request] =
    definedNameMap get name

  private def requestForIdent(line: String): Option[Request] =
    requestForName(newTermName(line)) orElse requestForName(newTypeName(line))

  def safeClass(name: String): Option[Symbol] = {
    try Some(definitions.getClass(newTypeName(name)))
    catch { case _: MissingRequirementError => None }
  }
  def safeModule(name: String): Option[Symbol] = {
    try Some(definitions.getModule(newTermName(name)))
    catch { case _: MissingRequirementError => None }
  }

  def definitionForName(name: Name): Option[MemberHandler] =
    requestForName(name) flatMap { req =>
      req.handlers find (_.definedNames contains name)
    }
  //

  def valueOfTerm(id: String): Option[AnyRef] =
    requestForIdent(id) flatMap (_.getEval)

  def classOfTerm(id: String): Option[Class[_]] =
    valueOfTerm(id) map (_.getClass)

  def typeOfTerm(id: String): Option[Type] = newTermName(id) match {
    case nme.ROOTPKG  => Some(RootClass.tpe)
    case name         => requestForName(name) flatMap (_.compilerTypeOf get name)
  }
  def symbolOfTerm(id: String): Symbol =
    requestForIdent(id) flatMap (_.definedSymbols get newTermName(id)) getOrElse NoSymbol

  def runtimeClassAndTypeOfTerm(id: String): Option[(Class[_], Type)] = {
    for {
      clazz <- classOfTerm(id)
      tpe <- runtimeTypeOfTerm(id)
      nonAnon <- clazz.supers find (!_.isScalaAnonymous)
    } yield {
      (nonAnon, tpe)
    }
  }

  def runtimeTypeOfTerm(id: String): Option[Type] = {
    for {
      tpe <- typeOfTerm(id)
      clazz <- classOfTerm(id)
      val staticSym = tpe.typeSymbol
      runtimeSym <- safeClass(clazz.getName)
      if runtimeSym != staticSym
      if runtimeSym isSubClass staticSym
    } yield {
      runtimeSym.info
    }
  }

  // XXX literals.
  // 1) Identifiers defined in the repl.
  // 2) A path loadable via getModule.
  // 3) Try interpreting it as an expression.
  private var typeOfExpressionDepth = 0
  def typeOfExpression(expr: String): Option[Type] = {
    if (typeOfExpressionDepth > 2) {
      DBG("Terminating typeOfExpression recursion for expression: " + expr)
      return None
    }

    def asModule = safeModule(expr) map (_.tpe)
    def asExpr = beSilentDuring {
      val lhs = freshInternalVarName()
      interpret("val " + lhs + " = { " + expr + " } ") match {
        case IR.Success => typeOfExpression(lhs)
        case _          => None
      }
    }

    typeOfExpressionDepth += 1
    try typeOfTerm(expr) orElse asModule orElse asExpr
    finally typeOfExpressionDepth -= 1
  }
  // def compileAndTypeExpr(expr: String): Option[Typer] = {
  //   class TyperRun extends Run {
  //     override def stopPhase(name: String) = name == "superaccessors"
  //   }
  // }

  private def onlyTerms(xs: List[Name]) = xs collect { case x: TermName => x }
  private def onlyTypes(xs: List[Name]) = xs collect { case x: TypeName => x }

  def importHandlers = allHandlers collect { case x: ImportHandler => x }
  def definedTerms   = onlyTerms(allDefinedNames) filterNot isInternalVarName
  def definedTypes   = onlyTypes(allDefinedNames)
  def importedTerms  = onlyTerms(importHandlers flatMap (_.importedNames))
  def importedTypes  = onlyTypes(importHandlers flatMap (_.importedNames))

  /** the previous requests this interpreter has processed */
  private lazy val prevRequests      = mutable.ArrayBuffer[Request]()
  private lazy val referencedNameMap = mutable.Map[Name, Request]()
  private lazy val definedNameMap    = mutable.Map[Name, Request]()
  private def allHandlers            = prevRequests.toList flatMap (_.handlers)
  private def allReqAndHandlers      = prevRequests.toList flatMap (req => req.handlers map (req -> _))
  def allSeenTypes                   = prevRequests.toList flatMap (_.typeOf.values.toList) distinct
  def allImplicits                   = allHandlers filter (_.definesImplicit) flatMap (_.definedNames)

  private def membersAtPickler(sym: Symbol): List[Symbol] =
    atPickler(sym.info.nonPrivateMembers)

  /** Symbols whose contents are language-defined to be imported. */
  def languageWildcardSyms: List[Symbol] = List(JavaLangPackage, ScalaPackage, PredefModule)
  def languageWildcards: List[Type] = languageWildcardSyms map (_.tpe)

  /** Types which have been wildcard imported, such as:
   *    val x = "abc" ; import x._  // type java.lang.String
   *    import java.lang.String._   // object java.lang.String
   *
   *  Used by tab completion.
   *
   *  XXX right now this gets import x._ and import java.lang.String._,
   *  but doesn't figure out import String._.  There's a lot of ad hoc
   *  scope twiddling which should be swept away in favor of digging
   *  into the compiler scopes.
   */
  def sessionWildcards: List[Type] = {
    importHandlers flatMap {
      case x if x.importsWildcard => x.targetType
      case _                      => None
    } distinct
  }
  def wildcardTypes = languageWildcards ++ sessionWildcards

  def languageSymbols = languageWildcardSyms flatMap membersAtPickler
  def sessionSymbols  = importHandlers flatMap (_.importedSymbols)
  def importedSymbols = languageSymbols ++ sessionSymbols
  def implicitSymbols = importedSymbols filter (_.isImplicit)

  /** Tuples of (source, imported symbols) in the order they were imported.
   */
  def importedSymbolsBySource: List[(Symbol, List[Symbol])] = {
    val lang    = languageWildcardSyms map (sym => (sym, membersAtPickler(sym)))
    val session = importHandlers filter (_.targetType.isDefined) map { mh =>
      (mh.targetType.get.typeSymbol, mh.importedSymbols)
    }

    lang ++ session
  }
  def implicitSymbolsBySource: List[(Symbol, List[Symbol])] = {
    importedSymbolsBySource map {
      case (k, vs) => (k, vs filter (_.isImplicit))
    } filterNot (_._2.isEmpty)
  }

  def visibleTermNames: List[Name] = definedTerms ++ importedTerms distinct

  /** Another entry point for tab-completion, ids in scope */
  def unqualifiedIds = visibleTermNames map (_.toString) filterNot (_ contains "$") sorted

  /** Parse the ScalaSig to find type aliases */
  def aliasForType(path: String) = ByteCode.aliasForType(path)

  def withoutUnwrapping(op: => Unit): Unit = {
    val saved = isettings.unwrapStrings
    isettings.unwrapStrings = false
    try op
    finally isettings.unwrapStrings = saved
  }

  def showCodeIfDebugging(code: String) {
    /** Secret bookcase entrance for repl debuggers: end the line
     *  with "// show" and see what's going on.
     */
    if (code.lines exists (_.trim endsWith "// show")) {
      echo(code)
      parse(code) foreach (ts => ts foreach (t => withoutUnwrapping(DBG(asCompactString(t)))))
    }
  }
  // debugging
  def debugging[T](msg: String)(res: T) = {
    DBG(msg + " " + res)
    res
  }
  def DBG(s: => String) = if (isReplDebug) {
    try repldbg(s)
    catch { case x: AssertionError => repldbg("Assertion error printing debug string:\n  " + x) }
  }
}

/** Utility methods for the Interpreter. */
object IMain {
  trait CodeAssembler[T] {
    def preamble: String
    def generate: T => String
    def postamble: String

    def apply(contributors: List[T]): String = stringFromWriter { code =>
      code println preamble
      contributors map generate foreach (code print _)
      code println postamble
    }
  }

  trait StrippingWriter {
    def isStripping: Boolean
    def stripImpl(str: String): String
    def strip(str: String): String = if (isStripping) stripImpl(str) else str
  }
  trait TruncatingWriter {
    def maxStringLength: Int
    def isTruncating: Boolean
    def truncate(str: String): String = {
      if (isTruncating && str.length > maxStringLength)
        (str take maxStringLength - 3) + "..."
      else str
    }
  }
  abstract class StrippingTruncatingWriter(out: PrintWriter)
          extends PrintWriter(out)
             with StrippingWriter
             with TruncatingWriter {
    self =>

    def clean(str: String): String = truncate(strip(str))
    override def write(str: String) = super.write(clean(str))
  }
  class ReplStrippingWriter(intp: IMain) extends StrippingTruncatingWriter(intp.out) {
    import intp._
    def maxStringLength    = isettings.maxPrintString
    def isStripping        = isettings.unwrapStrings
    def isTruncating       = reporter.truncationOK

    def stripImpl(str: String): String = {
      val cleaned = removeIWPackages(removeLineWrapper(str))
      var ctrlChars = 0
      cleaned map { ch =>
        if (ch.isControl && !ch.isWhitespace) {
          ctrlChars += 1
          if (ctrlChars > 5) return "[line elided for control chars: possibly a scala signature]"
          else '?'
        }
        else ch
      }
    }

    // The two name forms this is catching are the two sides of this assignment:
    //
    // $line3.$read.$iw.$iw.Bippy =
    //   $line3.$read$$iw$$iw$Bippy@4a6a00ca
    private def removeLineWrapper(s: String) = s.replaceAll("""\$line\d+[./]\$(read|eval|print)[$.]""", "")
    private def removeIWPackages(s: String) = s.replaceAll("""\$(iw|read|eval|print)[$.]""", "")
  }

  class ReplReporter(intp: IMain) extends ConsoleReporter(intp.settings, null, new ReplStrippingWriter(intp)) {
    override def printMessage(msg: String) {
      // Avoiding deadlock when the compiler starts logging before
      // the lazy val is done.
      if (intp.isInitializeComplete) {
        if (intp.totalSilence) ()
        else super.printMessage(msg)
      }
      else Console.println(msg)
    }
  }
}
