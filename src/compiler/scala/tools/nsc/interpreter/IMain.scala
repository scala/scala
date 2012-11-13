/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package interpreter

import Predef.{ println => _, _ }
import util.stringFromWriter
import scala.reflect.internal.util._
import java.net.URL
import scala.sys.BooleanProp
import scala.tools.nsc.io.AbstractFile
import reporters._
import scala.tools.util.PathResolver
import scala.tools.nsc.util.ScalaClassLoader
import ScalaClassLoader.URLClassLoader
import scala.tools.nsc.util.Exceptional.unwrap
import scala.collection.{ mutable, immutable }
import IMain._
import java.util.concurrent.Future
import scala.reflect.runtime.{ universe => ru }
import scala.reflect.{ ClassTag, classTag }
import scala.tools.reflect.StdRuntimeTags._

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
 *  exports members called "$eval" and "$print". To accomodate user expressions
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
class IMain(initialSettings: Settings, protected val out: JPrintWriter) extends Imports {
  imain =>

  object replOutput extends ReplOutput(settings.Yreploutdir) { }

  @deprecated("Use replOutput.dir instead", "2.11.0")
  def virtualDirectory = replOutput.dir
  def showDirectory() = replOutput.show(out)

  private[nsc] var printResults               = true      // whether to print result lines
  private[nsc] var totalSilence               = false     // whether to print anything
  private var _initializeComplete             = false     // compiler is initialized
  private var _isInitialized: Future[Boolean] = null      // set up initialization future
  private var bindExceptions                  = true      // whether to bind the lastException variable
  private var _executionWrapper               = ""        // code to be wrapped around all lines

  /** We're going to go to some trouble to initialize the compiler asynchronously.
   *  It's critical that nothing call into it until it's been initialized or we will
   *  run into unrecoverable issues, but the perceived repl startup time goes
   *  through the roof if we wait for it.  So we initialize it with a future and
   *  use a lazy val to ensure that any attempt to use the compiler object waits
   *  on the future.
   */
  private var _classLoader: AbstractFileClassLoader = null                              // active classloader
  private val _compiler: Global                     = newCompiler(settings, reporter)   // our private compiler

  private val nextReqId = {
    var counter = 0
    () => { counter += 1 ; counter }
  }

  def compilerClasspath: Seq[URL] = (
    if (isInitializeComplete) global.classPath.asURLs
    else new PathResolver(settings).result.asURLs  // the compiler's classpath
  )
  def settings = initialSettings
  def mostRecentLine = prevRequestList match {
    case Nil      => ""
    case req :: _ => req.originalLine
  }
  // Run the code body with the given boolean settings flipped to true.
  def withoutWarnings[T](body: => T): T = beQuietDuring {
    val saved = settings.nowarn.value
    if (!saved)
      settings.nowarn.value = true

    try body
    finally if (!saved) settings.nowarn.value = false
  }

  /** construct an interpreter that reports to Console */
  def this(settings: Settings) = this(settings, new NewLinePrintWriter(new ConsoleWriter, true))
  def this() = this(new Settings())

  lazy val repllog: Logger = new Logger {
    val out: JPrintWriter = imain.out
    val isInfo: Boolean  = BooleanProp keyExists "scala.repl.info"
    val isDebug: Boolean = BooleanProp keyExists "scala.repl.debug"
    val isTrace: Boolean = BooleanProp keyExists "scala.repl.trace"
  }
  lazy val formatting: Formatting = new Formatting {
    val prompt = Properties.shellPromptString
  }
  lazy val reporter: ReplReporter = new ReplReporter(this)

  import formatting._
  import reporter.{ printMessage, withoutTruncating }

  // This exists mostly because using the reporter too early leads to deadlock.
  private def echo(msg: String) { Console println msg }
  private def _initSources = List(new BatchSourceFile("<init>", "class $repl_$init { }"))
  private def _initialize() = {
    try {
      // todo. if this crashes, REPL will hang
      new _compiler.Run() compileSources _initSources
      _initializeComplete = true
      true
    }
    catch AbstractOrMissingHandler()
  }
  private def tquoted(s: String) = "\"\"\"" + s + "\"\"\""
  private val logScope = scala.sys.props contains "scala.repl.scope"
  private def scopelog(msg: String) = if (logScope) Console.err.println(msg)

  // argument is a thunk to execute after init is done
  def initialize(postInitSignal: => Unit) {
    synchronized {
      if (_isInitialized == null) {
        _isInitialized = io.spawn {
          try _initialize()
          finally postInitSignal
        }
      }
    }
  }
  def initializeSynchronous(): Unit = {
    if (!isInitializeComplete) {
      _initialize()
      assert(global != null, global)
    }
  }
  def isInitializeComplete = _initializeComplete

  /** the public, go through the future compiler */
  lazy val global: Global = {
    if (isInitializeComplete) _compiler
    else {
      // If init hasn't been called yet you're on your own.
      if (_isInitialized == null) {
        repldbg("Warning: compiler accessed before init set up.  Assuming no postInit code.")
        initialize(())
      }
      // blocks until it is ; false means catastrophic failure
      if (_isInitialized.get()) _compiler
      else null
    }
  }

  import global._
  import definitions.{ ObjectClass, termMember, typeMember, dropNullaryMethod}

  lazy val runtimeMirror = ru.runtimeMirror(classLoader)

  private def noFatal(body: => Symbol): Symbol = try body catch { case _: FatalError => NoSymbol }

  def getClassIfDefined(path: String)  = (
           noFatal(runtimeMirror staticClass path)
    orElse noFatal(rootMirror staticClass path)
  )
  def getModuleIfDefined(path: String) = (
           noFatal(runtimeMirror staticModule path)
    orElse noFatal(rootMirror staticModule path)
  )
  def getPathIfDefined(path: String) = (
    if (path endsWith "$") getModuleIfDefined(path.init)
    else getClassIfDefined(path)
  )

  implicit class ReplTypeOps(tp: Type) {
    def orElse(other: => Type): Type    = if (tp ne NoType) tp else other
    def andAlso(fn: Type => Type): Type = if (tp eq NoType) tp else fn(tp)
  }

  // TODO: If we try to make naming a lazy val, we run into big time
  // scalac unhappiness with what look like cycles.  It has not been easy to
  // reduce, but name resolution clearly takes different paths.
  object naming extends {
    val global: imain.global.type = imain.global
  } with Naming {
    // make sure we don't overwrite their unwisely named res3 etc.
    def freshUserTermName(): TermName = {
      val name = newTermName(freshUserVarName())
      if (replScope containsName name) freshUserTermName()
      else name
    }
    def isUserTermName(name: Name) = isUserVarName("" + name)
    def isInternalTermName(name: Name) = isInternalVarName("" + name)
  }
  import naming._

  object deconstruct extends {
    val global: imain.global.type = imain.global
  } with StructuredTypeStrings

  lazy val memberHandlers = new {
    val intp: imain.type = imain
  } with MemberHandlers
  import memberHandlers._

  /** Temporarily be quiet */
  def beQuietDuring[T](body: => T): T = {
    val saved = printResults
    printResults = false
    try body
    finally printResults = saved
  }
  def beSilentDuring[T](operation: => T): T = {
    val saved = totalSilence
    totalSilence = true
    try operation
    finally totalSilence = saved
  }

  def quietRun[T](code: String) = beQuietDuring(interpret(code))

  /** takes AnyRef because it may be binding a Throwable or an Exceptional */
  private def withLastExceptionLock[T](body: => T, alt: => T): T = {
    assert(bindExceptions, "withLastExceptionLock called incorrectly.")
    bindExceptions = false

    try     beQuietDuring(body)
    catch   logAndDiscard("withLastExceptionLock", alt)
    finally bindExceptions = true
  }

  def executionWrapper = _executionWrapper
  def setExecutionWrapper(code: String) = _executionWrapper = code
  def clearExecutionWrapper() = _executionWrapper = ""

  /** interpreter settings */
  lazy val isettings = new ISettings(this)

  /** Instantiate a compiler.  Overridable. */
  protected def newCompiler(settings: Settings, reporter: Reporter): ReplGlobal = {
    settings.outputDirs setSingleOutput replOutput.dir
    settings.exposeEmptyPackage.value = true
    if (settings.Yrangepos.value)
      new Global(settings, reporter) with ReplGlobal with interactive.RangePositions
    else
      new Global(settings, reporter) with ReplGlobal
  }

  /** Parent classloader.  Overridable. */
  protected def parentClassLoader: ClassLoader =
    settings.explicitParentLoader.getOrElse( this.getClass.getClassLoader() )

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
  def resetClassLoader() = {
    repldbg("Setting new classloader: was " + _classLoader)
    _classLoader = null
    ensureClassLoader()
  }
  final def ensureClassLoader() {
    if (_classLoader == null)
      _classLoader = makeClassLoader()
  }
  def classLoader: AbstractFileClassLoader = {
    ensureClassLoader()
    _classLoader
  }

  def backticked(s: String): String = (
    (s split '.').toList map {
      case "_"                               => "_"
      case s if nme.keywords(newTermName(s)) => s"`$s`"
      case s                                 => s
    } mkString "."
  )

  abstract class PhaseDependentOps {
    def shift[T](op: => T): T

    def lookup(name: Name): Symbol = shift(replScope lookup name)
    def path(name: => Name): String = shift(path(symbolOfName(name)))
    def path(sym: Symbol): String = backticked(shift(sym.fullName))
    def name(sym: Symbol): Name   = shift(sym.name)
    def info(sym: Symbol): Type   = shift(sym.info)
    def sig(sym: Symbol): String  = shift(sym.defString)
  }
  object typerOp extends PhaseDependentOps {
    def shift[T](op: => T): T = exitingTyper(op)
  }
  object flatOp extends PhaseDependentOps {
    def shift[T](op: => T): T = exitingFlatten(op)
  }

  def originalPath(name: String): String = originalPath(name: TermName)
  def originalPath(name: Name): String   = typerOp path name
  def originalPath(sym: Symbol): String  = typerOp path sym
  def flatPath(sym: Symbol): String      = flatOp shift sym.javaClassName
  // def translatePath(path: String) = symbolOfPath(path).fold(Option.empty[String])(flatPath)
  def translatePath(path: String) = {
    val sym = if (path endsWith "$") symbolOfTerm(path.init) else symbolOfIdent(path)
    sym match {
      case NoSymbol => None
      case _        => Some(flatPath(sym))
    }
  }

  private class TranslatingClassLoader(parent: ClassLoader) extends AbstractFileClassLoader(replOutput.dir, parent) {
    /** Overridden here to try translating a simple name to the generated
     *  class name if the original attempt fails.  This method is used by
     *  getResourceAsStream as well as findClass.
     */
    override protected def findAbstractFile(name: String): AbstractFile =
      super.findAbstractFile(name) match {
        case null => translatePath(name) map (super.findAbstractFile(_)) orNull
        case file => file
      }
  }
  private def makeClassLoader(): AbstractFileClassLoader =
    new TranslatingClassLoader(parentClassLoader match {
      case null   => ScalaClassLoader fromURLs compilerClasspath
      case p      => new URLClassLoader(compilerClasspath, p)
    })

  def getInterpreterClassLoader() = classLoader

  // Set the current Java "context" class loader to this interpreter's class loader
  def setContextClassLoader() = classLoader.setAsContext()

  def allDefinedNames: List[Name]  = exitingTyper(replScope.toList.map(_.name).sorted)
  def unqualifiedIds: List[String] = allDefinedNames map (_.decode) sorted

  /** Most recent tree handled which wasn't wholly synthetic. */
  private def mostRecentlyHandledTree: Option[Tree] = {
    prevRequests.reverse foreach { req =>
      req.handlers.reverse foreach {
        case x: MemberDefHandler if x.definesValue && !isInternalTermName(x.name) => return Some(x.member)
        case _ => ()
      }
    }
    None
  }

  private def updateReplScope(sym: Symbol, isDefined: Boolean) {
    def log(what: String) {
      val mark = if (sym.isType) "t " else "v "
      val name = exitingTyper(sym.nameString)
      val info = cleanTypeAfterTyper(sym)
      val defn = sym defStringSeenAs info

      scopelog(f"[$mark$what%6s] $name%-25s $defn%s")
    }
    if (ObjectClass isSubClass sym.owner) return
    // unlink previous
    replScope lookupAll sym.name foreach { sym =>
      log("unlink")
      replScope unlink sym
    }
    val what = if (isDefined) "define" else "import"
    log(what)
    replScope enter sym
  }

  def recordRequest(req: Request) {
    if (req == null)
      return

    prevRequests += req

    // warning about serially defining companions.  It'd be easy
    // enough to just redefine them together but that may not always
    // be what people want so I'm waiting until I can do it better.
    exitingTyper {
      req.defines filterNot (s => req.defines contains s.companionSymbol) foreach { newSym =>
        val companion = newSym.name.companionName
        val found = replScope lookup companion
        replScope lookup companion andAlso { oldSym =>
          replwarn(s"warning: previously defined $oldSym is not a companion to $newSym.")
          replwarn("Companions must be defined together; you may wish to use :paste mode for this.")
        }
      }
    }
    exitingTyper {
      req.imports foreach (sym => updateReplScope(sym, isDefined = false))
      req.defines foreach (sym => updateReplScope(sym, isDefined = true))
    }
  }

  private[nsc] def replwarn(msg: => String) {
    if (!settings.nowarnings.value)
      printMessage(msg)
  }

  def isParseable(line: String): Boolean = {
    beSilentDuring {
      try parse(line) match {
        case Some(xs) => xs.nonEmpty  // parses as-is
        case None     => true         // incomplete
      }
      catch { case x: Exception =>    // crashed the compiler
        replwarn("Exception in isParseable(\"" + line + "\"): " + x)
        false
      }
    }
  }

  def compileSourcesKeepingRun(sources: SourceFile*) = {
    val run = new Run()
    reporter.reset()
    run compileSources sources.toList
    (!reporter.hasErrors, run)
  }

  /** Compile an nsc SourceFile.  Returns true if there are
   *  no compilation errors, or false otherwise.
   */
  def compileSources(sources: SourceFile*): Boolean =
    compileSourcesKeepingRun(sources: _*)._1

  /** Compile a string.  Returns true if there are no
   *  compilation errors, or false otherwise.
   */
  def compileString(code: String): Boolean =
    compileSources(new BatchSourceFile("<script>", code))

  /** Build a request from the user. `trees` is `line` after being parsed.
   */
  private def buildRequest(line: String, trees: List[Tree]): Request = {
    executingRequest = new Request(line, trees)
    executingRequest
  }

  private def safePos(t: Tree, alt: Int): Int =
    try t.pos.startOrPoint
    catch { case _: UnsupportedOperationException => alt }

  // Given an expression like 10 * 10 * 10 we receive the parent tree positioned
  // at a '*'.  So look at each subtree and find the earliest of all positions.
  private def earliestPosition(tree: Tree): Int = {
    var pos = Int.MaxValue
    tree foreach { t =>
      pos = math.min(pos, safePos(t, Int.MaxValue))
    }
    pos
  }

  private def requestFromLine(line: String, synthetic: Boolean): Either[IR.Result, Request] = {
    val content = indentCode(line)
    val trees = parse(content) match {
      case None         => return Left(IR.Incomplete)
      case Some(Nil)    => return Left(IR.Error) // parse error or empty input
      case Some(trees)  => trees
    }
    repltrace(
      trees map (t => {
        // [Eugene to Paul] previously it just said `t map ...`
        // because there was an implicit conversion from Tree to a list of Trees
        // however Martin and I have removed the conversion
        // (it was conflicting with the new reflection API),
        // so I had to rewrite this a bit
        val subs = t collect { case sub => sub }
        subs map (t0 =>
          "  " + safePos(t0, -1) + ": " + t0.shortClass + "\n"
        ) mkString ""
      }) mkString "\n"
    )
    // If the last tree is a bare expression, pinpoint where it begins using the
    // AST node position and snap the line off there.  Rewrite the code embodied
    // by the last tree as a ValDef instead, so we can access the value.
    trees.last match {
      case _:Assign                        => // we don't want to include assignments
      case _:TermTree | _:Ident | _:Select => // ... but do want other unnamed terms.
        val varName  = if (synthetic) freshInternalVarName() else freshUserVarName()
        val rewrittenLine = (
          // In theory this would come out the same without the 1-specific test, but
          // it's a cushion against any more sneaky parse-tree position vs. code mismatches:
          // this way such issues will only arise on multiple-statement repl input lines,
          // which most people don't use.
          if (trees.size == 1) "val " + varName + " =\n" + content
          else {
            // The position of the last tree
            val lastpos0 = earliestPosition(trees.last)
            // Oh boy, the parser throws away parens so "(2+2)" is mispositioned,
            // with increasingly hard to decipher positions as we move on to "() => 5",
            // (x: Int) => x + 1, and more.  So I abandon attempts to finesse and just
            // look for semicolons and newlines, which I'm sure is also buggy.
            val (raw1, raw2) = content splitAt lastpos0
            repldbg("[raw] " + raw1 + "   <--->   " + raw2)

            val adjustment = (raw1.reverse takeWhile (ch => (ch != ';') && (ch != '\n'))).size
            val lastpos = lastpos0 - adjustment

            // the source code split at the laboriously determined position.
            val (l1, l2) = content splitAt lastpos
            repldbg("[adj] " + l1 + "   <--->   " + l2)

            val prefix   = if (l1.trim == "") "" else l1 + ";\n"
            // Note to self: val source needs to have this precise structure so that
            // error messages print the user-submitted part without the "val res0 = " part.
            val combined   = prefix + "val " + varName + " =\n" + l2

            repldbg(List(
              "    line" -> line,
              " content" -> content,
              "     was" -> l2,
              "combined" -> combined) map {
                case (label, s) => label + ": '" + s + "'"
              } mkString "\n"
            )
            combined
          }
        )
        // Rewriting    "foo ; bar ; 123"
        // to           "foo ; bar ; val resXX = 123"
        requestFromLine(rewrittenLine, synthetic) match {
          case Right(req) => return Right(req withOriginalLine line)
          case x          => return x
        }
      case _ =>
    }
    Right(buildRequest(line, trees))
  }

  // normalize non-public types so we don't see protected aliases like Self
  def normalizeNonPublic(tp: Type) = tp match {
    case TypeRef(_, sym, _) if sym.isAliasType && !sym.isPublic => tp.normalize
    case _                                                      => tp
  }

  /**
   *  Interpret one line of input. All feedback, including parse errors
   *  and evaluation results, are printed via the supplied compiler's
   *  reporter. Values defined are available for future interpreted strings.
   *
   *  The return value is whether the line was interpreter successfully,
   *  e.g. that there were no parse errors.
   */
  def interpret(line: String): IR.Result = interpret(line, false)
  def interpretSynthetic(line: String): IR.Result = interpret(line, true)
  def interpret(line: String, synthetic: Boolean): IR.Result = {
    def loadAndRunReq(req: Request) = {
      classLoader.setAsContext()
      val (result, succeeded) = req.loadAndRun

      /** To our displeasure, ConsoleReporter offers only printMessage,
       *  which tacks a newline on the end.  Since that breaks all the
       *  output checking, we have to take one off to balance.
       */
      if (succeeded) {
        if (printResults && result != "")
          printMessage(result stripSuffix "\n")
        else if (isReplDebug) // show quiet-mode activity
          printMessage(result.trim.lines map ("[quiet] " + _) mkString "\n")

        // Book-keeping.  Have to record synthetic requests too,
        // as they may have been issued for information, e.g. :type
        recordRequest(req)
        IR.Success
      }
      else {
        // don't truncate stack traces
        withoutTruncating(printMessage(result))
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
  def bind(name: String, boundType: String, value: Any, modifiers: List[String] = Nil): IR.Result = {
    val bindRep = new ReadEvalPrint()
    bindRep.compile("""
        |object %s {
        |  var value: %s = _
        |  def set(x: Any) = value = x.asInstanceOf[%s]
        |}
      """.stripMargin.format(bindRep.evalName, boundType, boundType)
      )
    bindRep.callEither("set", value) match {
      case Left(ex) =>
        repldbg("Set failed in bind(%s, %s, %s)".format(name, boundType, value))
        repldbg(util.stackTraceString(ex))
        IR.Error

      case Right(_) =>
        val line = "%sval %s = %s.value".format(modifiers map (_ + " ") mkString, name, bindRep.evalPath)
        repldbg("Interpreting: " + line)
        interpret(line)
    }
  }
  def directBind(name: String, boundType: String, value: Any): IR.Result = {
    val result = bind(name, boundType, value)
    if (result == IR.Success)
      directlyBoundNames += newTermName(name)
    result
  }
  def directBind(p: NamedParam): IR.Result                                    = directBind(p.name, p.tpe, p.value)
  def directBind[T: ru.TypeTag : ClassTag](name: String, value: T): IR.Result = directBind((name, value))

  def rebind(p: NamedParam): IR.Result = {
    val name     = p.name
    val newType  = p.tpe
    val tempName = freshInternalVarName()

    quietRun("val %s = %s".format(tempName, name))
    quietRun("val %s = %s.asInstanceOf[%s]".format(name, tempName, newType))
  }
  def quietImport(ids: String*): IR.Result = beQuietDuring(addImports(ids: _*))
  def addImports(ids: String*): IR.Result =
    if (ids.isEmpty) IR.Success
    else interpret("import " + ids.mkString(", "))

  def quietBind(p: NamedParam): IR.Result                               = beQuietDuring(bind(p))
  def bind(p: NamedParam): IR.Result                                    = bind(p.name, p.tpe, p.value)
  def bind[T: ru.TypeTag : ClassTag](name: String, value: T): IR.Result = bind((name, value))
  def bindSyntheticValue(x: Any): IR.Result                             = bindValue(freshInternalVarName(), x)
  def bindValue(x: Any): IR.Result                                      = bindValue(freshUserVarName(), x)
  def bindValue(name: String, x: Any): IR.Result                        = bind(name, TypeStrings.fromValue(x), x)

  /** Reset this interpreter, forgetting all user-specified requests. */
  def reset() {
    clearExecutionWrapper()
    resetClassLoader()
    resetAllCreators()
    prevRequests.clear()
    resetReplScope()
    replOutput.dir.clear()
  }

  /** This instance is no longer needed, so release any resources
   *  it is using.  The reporter's output gets flushed.
   */
  def close() {
    reporter.flush()
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

    val packageName = sessionNames.line + lineId
    val readName    = sessionNames.read
    val evalName    = sessionNames.eval
    val printName   = sessionNames.print
    val resultName  = sessionNames.result

    def bindError(t: Throwable) = {
      if (!bindExceptions) // avoid looping if already binding
        throw t

      val unwrapped = unwrap(t)
      withLastExceptionLock[String]({
        directBind[Throwable]("lastException", unwrapped)(tagOfThrowable, classTag[Throwable])
        util.stackTraceString(unwrapped)
      }, util.stackTraceString(unwrapped))
    }

    // TODO: split it out into a package object and a regular
    // object and we can do that much less wrapping.
    def packageDecl = "package " + packageName

    def pathTo(name: String)   = packageName + "." + name
    def packaged(code: String) = packageDecl + "\n\n" + code

    def readPath  = pathTo(readName)
    def evalPath  = pathTo(evalName)
    def printPath = pathTo(printName)

    def call(name: String, args: Any*): AnyRef = {
      val m = evalMethod(name)
      repldbg("Invoking: " + m)
      if (args.nonEmpty)
        repldbg("  with args: " + args.mkString(", "))

      m.invoke(evalClass, args.map(_.asInstanceOf[AnyRef]): _*)
    }

    def callEither(name: String, args: Any*): Either[Throwable, AnyRef] =
      try Right(call(name, args: _*))
      catch { case ex: Throwable => Left(ex) }

    def callOpt(name: String, args: Any*): Option[AnyRef] =
      try Some(call(name, args: _*))
      catch { case ex: Throwable => bindError(ex) ; None }

    class EvalException(msg: String, cause: Throwable) extends RuntimeException(msg, cause) { }

    private def evalError(path: String, ex: Throwable) =
      throw new EvalException("Failed to load '" + path + "': " + ex.getMessage, ex)

    private def load(path: String): Class[_] = {
      try Class.forName(path, true, classLoader)
      catch { case ex: Throwable => evalError(path, unwrap(ex)) }
    }

    lazy val evalClass = load(evalPath)
    lazy val evalValue = callOpt(resultName)

    def compile(source: String): Boolean = compileAndSaveRun("<console>", source)

    /** The innermost object inside the wrapper, found by
      * following accessPath into the outer one.
      */
    def resolvePathToSymbol(accessPath: String): Symbol = {
      val readRoot  = getModuleIfDefined(readPath)   // the outermost wrapper
      (accessPath split '.').foldLeft(readRoot: Symbol) {
        case (sym, "")    => sym
        case (sym, name)  => exitingTyper(termMember(sym, name))
      }
    }
    /** We get a bunch of repeated warnings for reasons I haven't
     *  entirely figured out yet.  For now, squash.
     */
    private def updateRecentWarnings(run: Run) {
      def loop(xs: List[(Position, String)]): List[(Position, String)] = xs match {
        case Nil                  => Nil
        case ((pos, msg)) :: rest =>
          val filtered = rest filter { case (pos0, msg0) =>
            (msg != msg0) || (pos.lineContent.trim != pos0.lineContent.trim) || {
              // same messages and same line content after whitespace removal
              // but we want to let through multiple warnings on the same line
              // from the same run.  The untrimmed line will be the same since
              // there's no whitespace indenting blowing it.
              (pos.lineContent == pos0.lineContent)
            }
          }
          ((pos, msg)) :: loop(filtered)
      }
      val warnings = loop(run.allConditionalWarnings flatMap (_.warnings))
      if (warnings.nonEmpty)
        mostRecentWarnings = warnings
    }
    private def evalMethod(name: String) = evalClass.getMethods filter (_.getName == name) match {
      case Array(method) => method
      case xs            => sys.error("Internal error: eval object " + evalClass + ", " + xs.mkString("\n", "\n", ""))
    }
    private def compileAndSaveRun(label: String, code: String) = {
      showCodeIfDebugging(code)
      val (success, run) = compileSourcesKeepingRun(new BatchSourceFile(label, packaged(code)))
      updateRecentWarnings(run)
      success
    }
  }

  /** One line of code submitted by the user for interpretation */
  // private
  class Request(val line: String, val trees: List[Tree]) {
    def defines    = defHandlers flatMap (_.definedSymbols)
    def imports    = importedSymbols
    def references = referencedNames map symbolOfName
    def value      = Some(handlers.last) filter (h => h.definesValue) map (h => definedSymbols(h.definesTerm.get)) getOrElse NoSymbol

    val reqId = nextReqId()
    val lineRep = new ReadEvalPrint()

    private var _originalLine: String = null
    def withOriginalLine(s: String): this.type = { _originalLine = s ; this }
    def originalLine = if (_originalLine == null) line else _originalLine

    /** handlers for each tree in this request */
    val handlers: List[MemberHandler] = trees map (memberHandlers chooseHandler _)
    def defHandlers = handlers collect { case x: MemberDefHandler => x }

    /** all (public) names defined by these statements */
    val definedNames = handlers flatMap (_.definedNames)

    /** list of names used by this expression */
    val referencedNames: List[Name] = handlers flatMap (_.referencedNames)

    /** def and val names */
    def termNames = handlers flatMap (_.definesTerm)
    def typeNames = handlers flatMap (_.definesType)
    def importedSymbols = handlers flatMap {
      case x: ImportHandler => x.importedSymbols
      case _                => Nil
    }

    /** Code to import bound names from previous lines - accessPath is code to
      * append to objectName to access anything bound by request.
      */
    val ComputedImports(importsPreamble, importsTrailer, accessPath) =
      exitingTyper(importsCode(referencedNames.toSet))

    /** The unmangled symbol name, but supplemented with line info. */
    def disambiguated(name: Name): String = name + " (in " + lineRep + ")"

    /** the line of code to compute */
    def toCompute = line

    def fullPath(vname: String) = s"${lineRep.readPath}$accessPath.`$vname`"

    /** generate the source code for the object that computes this request */
    private object ObjectSourceCode extends CodeAssembler[MemberHandler] {
      def path = originalPath("$intp")
      def envLines = {
        if (!isReplPower) Nil // power mode only for now
        // $intp is not bound; punt, but include the line.
        else if (path == "$intp") List(
          "def $line = " + tquoted(originalLine),
          // "def $req = %s.requestForReqId(%s).orNull".format(path, reqId),
          "def $trees = Nil"
        )
        else List(
          "def $line  = " + tquoted(originalLine),
          "def $trees = Nil"
          // "def $trees = if ($req eq null) Nil else $req.trees".format(lineRep.readName, path, reqId)
        )
      }

      val preamble = """
        |object %s {
        |%s%s%s
      """.stripMargin.format(lineRep.readName, envLines.map("  " + _ + ";\n").mkString, importsPreamble, indentCode(toCompute))
      val postamble = importsTrailer + "\n}"
      val generate = (m: MemberHandler) => m extraCodeToEvaluate Request.this
    }

    private object ResultObjectSourceCode extends CodeAssembler[MemberHandler] {
      /** We only want to generate this code when the result
       *  is a value which can be referred to as-is.
       */
      val evalResult = Request.this.value match {
        case NoSymbol => ""
        case sym      => "lazy val %s = %s".format(lineRep.resultName, originalPath(sym))
      }
      // first line evaluates object to make sure constructor is run
      // initial "" so later code can uniformly be: + etc
      val preamble = """
      |object %s {
      |  %s
      |  val %s: String = %s {
      |    %s
      |    (""
      """.stripMargin.format(
        lineRep.evalName, evalResult, lineRep.printName,
        executionWrapper, lineRep.readName + accessPath
      )

      val postamble = """
      |    )
      |  }
      |}
      """.stripMargin
      val generate = (m: MemberHandler) => m resultExtractionCode Request.this
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

        // Assign symbols to the original trees
        // TODO - just use the new trees.
        defHandlers foreach { dh =>
          val name = dh.member.name
          definedSymbols get name foreach { sym =>
            dh.member setSymbol sym
            repldbg("Set symbol of " + name + " to " + symbolDefString(sym))
          }
        }

        // compile the result-extraction object
        withoutWarnings(lineRep compile ResultObjectSourceCode(handlers))
      }
    }

    lazy val resultSymbol = lineRep.resolvePathToSymbol(accessPath)
    def applyToResultMember[T](name: Name, f: Symbol => T) = exitingTyper(f(resultSymbol.info.nonPrivateDecl(name)))

    /* typeOf lookup with encoding */
    def lookupTypeOf(name: Name) = typeOf.getOrElse(name, typeOf(global.encode(name.toString)))
    def simpleNameOfType(name: TypeName) = (compilerTypeOf get name) map (_.typeSymbolDirect.simpleName)

    private def typeMap[T](f: Type => T) =
      mapFrom[Name, Name, T](termNames ++ typeNames)(x => f(cleanMemberDecl(resultSymbol, x)))

    /** Types of variables defined by this request. */
    lazy val compilerTypeOf = typeMap[Type](x => x) withDefaultValue NoType
    /** String representations of same. */
    lazy val typeOf         = typeMap[String](tp => exitingTyper(tp.toString))

    lazy val definedSymbols = (
      termNames.map(x => x -> applyToResultMember(x, x => x)) ++
      typeNames.map(x => x -> compilerTypeOf(x).typeSymbolDirect)
    ).toMap[Name, Symbol] withDefaultValue NoSymbol

    lazy val typesOfDefinedTerms = mapFrom[Name, Name, Type](termNames)(x => applyToResultMember(x, _.tpe))

    /** load and run the code using reflection */
    def loadAndRun: (String, Boolean) = {
      try   { ("" + (lineRep call sessionNames.print), true) }
      catch { case ex: Throwable => (lineRep.bindError(ex), false) }
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

  private var mostRecentWarnings: List[(global.Position, String)] = Nil
  def lastWarnings = mostRecentWarnings

  private lazy val importToGlobal  = global mkImporter ru
  private lazy val importToRuntime = ru mkImporter global
  private lazy val javaMirror = ru.rootMirror match {
    case x: ru.JavaMirror => x
    case _                => null
  }
  private implicit def importFromRu(sym: ru.Symbol): Symbol = importToGlobal importSymbol sym
  private implicit def importToRu(sym: Symbol): ru.Symbol   = importToRuntime importSymbol sym

  def classOfTerm(id: String): Option[JClass] = symbolOfTerm(id) match {
    case NoSymbol => None
    case sym      => Some(javaMirror runtimeClass importToRu(sym).asClass)
  }

  def typeOfTerm(id: String): Type = symbolOfTerm(id).tpe

  def valueOfTerm(id: String): Option[Any] = exitingTyper {
    def value() = {
      val sym0    = symbolOfTerm(id)
      val sym     = (importToRuntime importSymbol sym0).asTerm
      val module  = runtimeMirror.reflectModule(sym.owner.companionSymbol.asModule).instance
      val module1 = runtimeMirror.reflect(module)
      val invoker = module1.reflectField(sym)

      invoker.get
    }

    try Some(value()) catch { case _: Exception => None }
  }

  /** It's a bit of a shotgun approach, but for now we will gain in
   *  robustness. Try a symbol-producing operation at phase typer, and
   *  if that is NoSymbol, try again at phase flatten. I'll be able to
   *  lose this and run only from exitingTyper as soon as I figure out
   *  exactly where a flat name is sneaking in when calculating imports.
   */
  def tryTwice(op: => Symbol): Symbol = exitingTyper(op) orElse exitingFlatten(op)

  def signatureOf(sym: Symbol)           = typerOp sig sym
  def symbolOfPath(path: String): Symbol = exitingTyper(getPathIfDefined(path))
  def symbolOfIdent(id: String): Symbol  = symbolOfTerm(id) orElse symbolOfType(id)
  def symbolOfType(id: String): Symbol   = tryTwice(replScope lookup (id: TypeName))
  def symbolOfTerm(id: String): Symbol   = tryTwice(replScope lookup (id: TermName))
  def symbolOfName(id: Name): Symbol     = replScope lookup id

  def runtimeClassAndTypeOfTerm(id: String): Option[(JClass, Type)] = {
    classOfTerm(id) flatMap { clazz =>
      clazz.supers find (!_.isScalaAnonymous) map { nonAnon =>
        (nonAnon, runtimeTypeOfTerm(id))
      }
    }
  }

  def runtimeTypeOfTerm(id: String): Type = {
    typeOfTerm(id) andAlso { tpe =>
      val clazz      = classOfTerm(id) getOrElse { return NoType }
      val staticSym  = tpe.typeSymbol
      val runtimeSym = getClassIfDefined(clazz.getName)

      if ((runtimeSym != NoSymbol) && (runtimeSym != staticSym) && (runtimeSym isSubClass staticSym))
        runtimeSym.info
      else NoType
    }
  }

  def cleanTypeAfterTyper(sym: => Symbol): Type = {
    exitingTyper(
      normalizeNonPublic(
        dropNullaryMethod(
          sym.tpe_*
        )
      )
    )
  }
  def cleanMemberDecl(owner: Symbol, member: Name): Type =
    cleanTypeAfterTyper(owner.info nonPrivateDecl member)

  object exprTyper extends {
    val repl: IMain.this.type = imain
  } with ExprTyper { }

  def parse(line: String): Option[List[Tree]] = exprTyper.parse(line)

  def symbolOfLine(code: String): Symbol =
    exprTyper.symbolOfLine(code)

  def typeOfExpression(expr: String, silent: Boolean = true): Type =
    exprTyper.typeOfExpression(expr, silent)

  protected def onlyTerms(xs: List[Name]): List[TermName] = xs collect { case x: TermName => x }
  protected def onlyTypes(xs: List[Name]): List[TypeName] = xs collect { case x: TypeName => x }

  def definedTerms      = onlyTerms(allDefinedNames) filterNot isInternalTermName
  def definedTypes      = onlyTypes(allDefinedNames)
  def definedSymbols    = prevRequestList flatMap (_.defines) toSet
  def definedSymbolList = prevRequestList flatMap (_.defines) filterNot (s => isInternalTermName(s.name))

  // Terms with user-given names (i.e. not res0 and not synthetic)
  def namedDefinedTerms = definedTerms filterNot (x => isUserVarName("" + x) || directlyBoundNames(x))

  /** Translate a repl-defined identifier into a Symbol.
   */
  def apply(name: String): Symbol = types(name) orElse terms(name)
  def types(name: String): Symbol = replScope lookup (name: TypeName) orElse getClassIfDefined(name)
  def terms(name: String): Symbol = replScope lookup (name: TermName) orElse getModuleIfDefined(name)

  def types[T: global.TypeTag] : Symbol = typeOf[T].typeSymbol
  def terms[T: global.TypeTag] : Symbol = typeOf[T].termSymbol
  def apply[T: global.TypeTag] : Symbol = typeOf[T].typeSymbol

  lazy val DummyInfoSymbol = NoSymbol.newValue("replScopeDummy")
  private lazy val DummyInfo = TypeRef(NoPrefix, DummyInfoSymbol, Nil)
  private def enterDummySymbol(name: Name) = name match {
    case x: TermName => replScope enter (NoSymbol.newValue(x) setInfo DummyInfo)
    case x: TypeName => replScope enter (NoSymbol.newClass(x) setInfo DummyInfo)
  }

  private var _replScope: Scope = _
  private def resetReplScope() {
    _replScope = newScope
  }
  def initReplScope() {
    languageWildcardSyms foreach { clazz =>
      importableMembers(clazz) foreach { sym =>
        updateReplScope(sym, isDefined = false)
      }
    }
  }
  def replScope = {
    if (_replScope eq null)
      _replScope = newScope

    _replScope
  }
  def lookupAll(name: String) = (replScope.lookupAll(name: TermName) ++ replScope.lookupAll(name: TypeName)).toList
  def unlinkAll(name: String) = {
    val syms = lookupAll(name)
    syms foreach { sym =>
      replScope unlink sym
    }
    enterDummySymbol(name: TermName)
    enterDummySymbol(name: TypeName)
    syms
  }
  def isUnlinked(name: Name) = {
    symbolOfName(name) match {
      case NoSymbol => false
      case sym      => sym.info.typeSymbolDirect == DummyInfoSymbol
    }
  }

  private var executingRequest: Request = _
  private val prevRequests       = mutable.ListBuffer[Request]()
  private val directlyBoundNames = mutable.Set[Name]()

  def allHandlers    = prevRequestList flatMap (_.handlers)
  def allDefHandlers = allHandlers collect { case x: MemberDefHandler => x }
  def allDefSymbols  = allDefHandlers map (_.symbol) filter (_ ne NoSymbol)

  def lastRequest         = if (prevRequests.isEmpty) null else prevRequests.last
  def prevRequestList     = prevRequests.toList
  def allSeenTypes        = prevRequestList flatMap (_.typeOf.values.toList) distinct
  def allImplicits        = allHandlers filter (_.definesImplicit) flatMap (_.definedNames)
  def importHandlers      = allHandlers collect { case x: ImportHandler => x }

  def withoutUnwrapping(op: => Unit): Unit = {
    val saved = isettings.unwrapStrings
    isettings.unwrapStrings = false
    try op
    finally isettings.unwrapStrings = saved
  }

  def symbolDefString(sym: Symbol) = {
    TypeStrings.quieter(
      exitingTyper(sym.defString),
      sym.owner.name + ".this.",
      sym.owner.fullName + "."
    )
  }

  def showCodeIfDebugging(code: String) {
    /** Secret bookcase entrance for repl debuggers: end the line
     *  with "// show" and see what's going on.
     */
    def isShow = code.lines exists (_.trim endsWith "// show")
    if (isReplDebug || isShow) {
      beSilentDuring(parse(code)) foreach { ts =>
        ts foreach { t =>
          withoutUnwrapping(echo(asCompactString(t)))
        }
      }
    }
  }

  // debugging
  def debugging[T](msg: String)(res: T) = {
    repldbg(msg + " " + res)
    res
  }
}

/** Utility methods for the Interpreter. */
object IMain {
  // The two name forms this is catching are the two sides of this assignment:
  //
  // $line3.$read.$iw.$iw.Bippy =
  //   $line3.$read$$iw$$iw$Bippy@4a6a00ca
  private def removeLineWrapper(s: String) = s.replaceAll("""\$line\d+[./]\$(read|eval|print)[$.]""", "")
  private def removeIWPackages(s: String)  = s.replaceAll("""\$(iw|read|eval|print)[$.]""", "")
  def stripString(s: String)               = removeIWPackages(removeLineWrapper(s))

  trait CodeAssembler[T] {
    def preamble: String
    def generate: T => String
    def postamble: String

    def apply(contributors: List[T]): String = stringFromWriter { code =>
      code println preamble
      contributors map generate foreach (code println _)
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
      if (isTruncating && (maxStringLength != 0 && str.length > maxStringLength))
        (str take maxStringLength - 3) + "..."
      else str
    }
  }
  abstract class StrippingTruncatingWriter(out: JPrintWriter)
          extends JPrintWriter(out)
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

    def stripImpl(str: String): String = naming.unmangle(str)
  }
}
