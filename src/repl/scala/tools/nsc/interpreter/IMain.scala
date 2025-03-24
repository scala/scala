/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.interpreter

import java.io.{Closeable, PrintWriter, StringWriter}
import java.net.URL
import scala.collection.mutable, mutable.ListBuffer
import scala.language.implicitConversions
import scala.reflect.{ClassTag, classTag}
import scala.reflect.internal.{FatalError, Flags, MissingRequirementError, NoPhase, Precedence}
import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.reflect.internal.util.{AbstractFileClassLoader, BatchSourceFile, ListOfNil, Position, ReplBatchSourceFile, SourceFile}
import scala.reflect.runtime.{universe => ru}
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.interpreter.Results.{Error, Incomplete, Result, Success}
import scala.tools.nsc.interpreter.StdReplTags.tagOfStdReplVals
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.typechecker.{StructuredTypeStrings, TypeStrings}
import scala.tools.nsc.util.Exceptional.rootCause
import scala.tools.nsc.util.{stackTraceString, stringFromWriter}
import scala.tools.util.PathResolver
import scala.util.{Try => Trying}
import scala.util.chaining._
import scala.util.control.NonFatal

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
  *  exports members called "\$eval" and "\$print". To accommodate user expressions
  *  that read from variables or methods defined in previous statements, "import"
  *  statements are used.
  *
  *  This interpreter shares the strengths and weaknesses of using the
  *  full compiler-to-Java.  The main strength is that interpreted code
  *  behaves exactly as does compiled code, including running at full speed.
  *  The main weakness is that redefining classes and methods is not handled
  *  properly, because rebinding at the Java level is technically difficult.
  */
class IMain(val settings: Settings, parentClassLoaderOverride: Option[ClassLoader], compilerSettings: Settings, val reporter: ReplReporter)
  extends Repl with Imports with PresentationCompilation with Closeable {

  def this(interpreterSettings: Settings, reporter: ReplReporter) = this(interpreterSettings, None, interpreterSettings, reporter)

  import reporter.{debug => repldbg}

  private[interpreter] lazy val useMagicImport: Boolean = settings.YreplMagicImport.value

  private var bindExceptions                  = true        // whether to bind the lastException variable
  private var _executionWrapper               = ""          // code to be wrapped around all lines
  private var label                           = "<console>" // compilation unit name for reporting

  /** We're going to go to some trouble to initialize the compiler asynchronously.
    *  It's critical that nothing call into it until it's been initialized or we will
    *  run into unrecoverable issues, but the perceived repl startup time goes
    *  through the roof if we wait for it.  So we initialize it with a future and
    *  use a lazy val to ensure that any attempt to use the compiler object waits
    *  on the future.
    */
  private var _classLoader: AbstractFileClassLoader = null  // active classloader
  private var _runtimeMirror: ru.Mirror = null
  private var _runtimeClassLoader: URLClassLoader = null    // wrapper exposing addURL

  def compilerClasspath: Seq[java.net.URL] = (
    if (_initializeComplete) global.classPath.asURLs
    else new PathResolver(settings, global.closeableRegistry).resultAsURLs  // the compiler's classpath
    )

  // Run the code body with the given boolean settings flipped to true.
  def withoutWarnings[T](body: => T): T =
    reporter.withoutPrintingResults(IMain.withSuppressedSettings(settings, global)(body))

  def withSuppressedSettings(body: => Unit): Unit =
    IMain.withSuppressedSettings(settings, global)(body)

  // Apply a temporary label for compilation (for example, script name)
  override def withLabel[A](temp: String)(body: => A): A = {
    val saved = label
    label = temp
    try body finally label = saved
  }

  override def visibleSettings: List[Setting] = settings.visibleSettings
  override def userSetSettings: List[Setting] = settings.userSetSettings
  override def updateSettings(arguments: List[String]): Boolean = {
    val (ok, rest) = settings.processArguments(arguments, processAll = false)
    ok && rest.isEmpty
  }

  object replOutput extends ReplOutput(settings.Yreploutdir) { }

  override def outputDir = replOutput.dir

  // Used in a test case.
  def showDirectory: String = {
    val writer = new StringWriter()
    replOutput.show(new PrintWriter(writer))
    writer.toString
  }

  lazy val isClassBased: Boolean = settings.Yreplclassbased.value


  override def initializeComplete = _initializeComplete
  private[this] var _initializeComplete = false

  // initializes the compiler, returning false if something went wrong
  override def initializeCompiler(): Boolean = global != null

  lazy val global: Global = {
    compilerSettings.outputDirs.setSingleOutput(replOutput.dir)
    compilerSettings.exposeEmptyPackage.value = true

    // Can't use our own reporter until global is initialized
    val startupReporter = new StoreReporter(compilerSettings)

    val compiler = new Global(compilerSettings, startupReporter) with ReplGlobal

    try {
      val run = new compiler.Run()
      assert(run.typerPhase != NoPhase, "REPL requires a typer phase.")
      IMain.withSuppressedSettings(compilerSettings, compiler) {
        run compileSources List(new BatchSourceFile("<init>", "class $repl_$init { }"))
      }

      // there shouldn't be any errors yet; just in case, print them if we're debugging
      if (reporter.isDebug)
        startupReporter.infos foreach { Console.err.println }

      compiler.reporter = reporter
      _initializeComplete = true
      compiler
    }
    catch AbstractOrMissingHandler()
  }

  import global._
  import definitions.{ ObjectClass, termMember, dropNullaryMethod}

  override def classPathString = global.classPath.asClassPathString

  private def noFatal(body: => Symbol): Symbol = try body catch { case _: FatalError => NoSymbol }

  def getClassIfDefined(path: String)  = (
    noFatal(runtimeMirror staticClass path)
      orElse noFatal(rootMirror staticClass path)
    )
  def getModuleIfDefined(path: String) = (
    noFatal(runtimeMirror staticModule path)
      orElse noFatal(rootMirror staticModule path)
    )

  implicit class ReplTypeOps(tp: Type) {
    def andAlso(fn: Type => Type): Type = if (tp eq NoType) tp else fn(tp)
  }

  // TODO: If we try to make naming a lazy val, we run into big time
  // scalac unhappiness with what look like cycles.  It has not been easy to
  // reduce, but name resolution clearly takes different paths.
  object naming extends {
    val global: IMain.this.global.type = IMain.this.global
  } with Naming {
    // make sure we don't overwrite their unwisely named res3 etc.
    def freshUserTermName(): TermName = {
      val name = newTermName(freshUserVarName())
      if (replScope containsName name) freshUserTermName()
      else name
    }
    def isInternalTermName(name: Name) = isInternalVarName("" + name)
  }
  import naming._
  import Naming._

  object deconstruct extends {
    val global: IMain.this.global.type = IMain.this.global
  } with StructuredTypeStrings

  lazy val memberHandlers = new {
    val intp: IMain.this.type = IMain.this
  } with MemberHandlers
  import memberHandlers._


  override def quietRun(code: String): Result = reporter.withoutPrintingResults(interpret(code))

  /** takes AnyRef because it may be binding a Throwable or an Exceptional */
  private def withLastExceptionLock[T](body: => T, alt: => T): T = {
    assert(bindExceptions, "withLastExceptionLock called incorrectly.")
    bindExceptions = false

    try reporter.withoutPrintingResults(body) catch { case NonFatal(t) =>
      repldbg("withLastExceptionLock: " + rootCause(t))
      reporter.trace(stackTraceString(rootCause(t)))
      alt
    } finally bindExceptions = true
  }

  def executionWrapper = _executionWrapper
  def setExecutionWrapper(code: String) = _executionWrapper = code
  override def clearExecutionWrapper() = _executionWrapper = ""



  /**
    * Adds all specified jars to the compile and runtime classpaths.
    *
    * @note  Currently only supports jars, not directories.
    * @param urls The list of items to add to the compile and runtime classpaths.
    */
  override def addUrlsToClassPath(urls: URL*): Unit = {
    new Run //  force some initialization
    urls.foreach(_runtimeClassLoader.addURL) // Add jars to runtime classloader
    global.extendCompilerClassPath(urls: _*) // Add jars to compile-time classpath
  }

  protected def replClass: Class[_] = this.getClass

  /** Parent classloader.  Overridable. */
  protected def parentClassLoader: ClassLoader = {
    // might be null if we're on the boot classpath
    parentClassLoaderOverride.
      orElse(settings.explicitParentLoader).
      orElse(Option(replClass.getClassLoader())).
      getOrElse(ClassLoader.getSystemClassLoader)
  }

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
    _runtimeMirror = null
    ensureClassLoader()
  }
  final def ensureClassLoader(): Unit =
    if (_classLoader == null)
      _classLoader = makeClassLoader()

  override def classLoader: AbstractFileClassLoader = {
    ensureClassLoader()
    _classLoader
  }
  def runtimeMirror = {
    if (_runtimeMirror == null)
      _runtimeMirror = ru.runtimeMirror(classLoader)
    _runtimeMirror
  }

  def backticked(s: String): String = (
    (s split '.').toList map {
      case "_"                               => "`_`"
      case s if nme.keywords(newTermName(s)) => s"`$s`"
      case s                                 => s
    } mkString "."
    )
  def readRootPath(readPath: String) = getModuleIfDefined(readPath)

  abstract class PhaseDependentOps {
    def shift[T](op: => T): T

    def path(name: => Name): String = shift(path(symbolOfName(name)))
    def path(sym: Symbol): String = backticked(shift(sym.fullName))
    def sig(sym: Symbol): String  = shift(sym.defString)
  }
  object typerOp extends PhaseDependentOps {
    def shift[T](op: => T): T = exitingTyper(op)
  }
  object flatOp extends PhaseDependentOps {
    def shift[T](op: => T): T = exitingFlatten(op)
  }

  override def originalPath(name: String): String = originalPath(TermName(name))
  def originalPath(name: Name): String   = translateOriginalPath(typerOp path name)
  def originalPath(sym: Symbol): String  = translateOriginalPath(typerOp path sym)

  val readInstanceName = ".INSTANCE"
  def translateOriginalPath(p: String): String = {
    p.replace(sessionNames.read, sessionNames.read + readInstanceName)
  }
  def flatPath(sym: Symbol): String = {
    val sym1 = if (sym.isModule) sym.moduleClass else sym
    flatOp shift sym1.javaClassName
  }

  override def translatePath(path: String): Option[String] = {
    val sym = if (path endsWith "$") symbolOfTerm(path.init) else symbolOfIdent(path)
    sym.toOption map flatPath
  }

  /** If path represents a class resource in the default package,
    *  see if the corresponding symbol has a class file that is a REPL artifact
    *  residing at a different resource path. Translate X.class to \$line3/\$read\$\$iw\$\$iw\$X.class.
    */
  def translateSimpleResource(path: String): Option[String] = {
    if (!(path contains '/') && (path endsWith ".class")) {
      val name = path stripSuffix ".class"
      val sym = if (name endsWith "$") symbolOfTerm(name.init) else symbolOfIdent(name)
      def pathOf(s: String) = s"${s.replace('.', '/')}.class"
      sym.toOption map (s => pathOf(flatPath(s)))
    } else {
      None
    }
  }
  override def translateEnclosingClass(n: String): Option[String] = symbolOfTerm(n).enclClass.toOption map flatPath

  /** If unable to find a resource foo.class, try taking foo as a symbol in scope
   *  and use its java class name as a resource to load.
   *
   *  \$intp.classLoader classBytes "Bippy" or \$intp.classLoader getResource "Bippy.class" just work.
   */
  private class TranslatingClassLoader(parent: ClassLoader) extends AbstractFileClassLoader(replOutput.dir, parent) {
    override protected def findAbstractFile(name: String): AbstractFile = super.findAbstractFile(name) match {
      case null if _initializeComplete => translateSimpleResource(name).map(super.findAbstractFile).orNull
      case file => file
    }
    // if the name was mapped by findAbstractFile, supply null name to avoid name check in defineClass
    override protected def findClass(name: String): Class[_] = {
      val bytes = classBytes(name)
      if (bytes.length == 0)
        throw new ClassNotFoundException(name)
      else
        defineClass(/*name=*/null, bytes, 0, bytes.length, protectionDomain)
    }
  }
  private def makeClassLoader(): AbstractFileClassLoader =
    new TranslatingClassLoader(new URLClassLoader(compilerClasspath, parentClassLoader).tap(_runtimeClassLoader_=))

  def allDefinedNames: List[Name]  = exitingTyper(replScope.toList.map(_.name).sorted)
  def unqualifiedIds: List[String] = allDefinedNames.map(_.decode).sorted

  /** Most recent tree handled which wasn't wholly synthetic. */
  private def mostRecentlyHandledTree: Option[Tree] = {
    prevRequests.reverseIterator.map(_.handlers.reverseIterator.collectFirst {
      case x: MemberDefHandler if x.definesValue && !isInternalTermName(x.name) => x.member
    }).find(_.isDefined).flatten
  }

  private val logScope = scala.sys.props contains "scala.repl.scope"
  private def scopelog(msg: String) = if (logScope) Console.err.println(msg)

  private def updateReplScope(sym: Symbol, isDefined: Boolean): Unit = {
    def log(what: String): Unit = {
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

  def recordRequest(req: Request): Unit = if (req != null) {

    prevRequests += req

    // warning about serially defining companions.  It'd be easy
    // enough to just redefine them together but that may not always
    // be what people want so I'm waiting until I can do it better.
    exitingTyper {
      req.defines filterNot (s => req.defines contains s.companionSymbol) foreach { newSym =>
        val oldSym = replScope lookup newSym.name.companionName
        if (Seq(oldSym, newSym).permutations exists { case Seq(s1, s2) => s1.isClass && s2.isModule case _ => false }) {
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

  private[nsc] def replwarn(msg: => String): Unit = {
    if (!settings.nowarnings.value)
      reporter.printMessage(msg)
  }

  def compileSourcesKeepingRun(sources: SourceFile*) = {
    val run = new Run()
    assert(run.typerPhase != NoPhase, "REPL requires a typer phase.")
    run compileSources sources.toList
    (!reporter.hasErrors, run)
  }

  /** Compile an nsc SourceFile.  Returns true if there are
    *  no compilation errors, or false otherwise.
    */
  override def compileSources(sources: SourceFile*): Boolean =
    compileSourcesKeepingRun(sources: _*)._1

  /** Compile a string.  Returns true if there are no
    *  compilation errors, or false otherwise.
    */
  override def compileString(code: String): Boolean =
    compileSources(new BatchSourceFile("<script>", code))

  override def requestDefining(name: String): Option[Request] = {
    val sym = symbolOfIdent(name)
    prevRequestList collectFirst { case r if r.defines contains sym => r }
  }

  private[interpreter] def requestFromLine(input: String, synthetic: Boolean = false, fatally: Boolean = false): Either[Result, Request] = {
    // The `currentRun` was used for compiling the previous line, ensure to clear out suspended messages.
    // Also JLine and completion may run the parser in the same run before actual compilation.
    currentRun.reporting.clearSuspendedMessages()
    parse(input, fatally) flatMap {
      case (Nil, _, _) => Left(Error)
      case (trees, firstXmlPos, parserSource) =>
        executingRequest = new Request(input, trees, parserSource, firstXmlPos, synthetic = synthetic)
        reporter.currentRequest = executingRequest
        Right(executingRequest)
    }
  }

  // dealias non-public types so we don't see protected aliases like Self
  def dealiasNonPublic(tp: Type) = tp match {
    case TypeRef(_, sym, _) if sym.isAliasType && !sym.isPublic => tp.dealias
    case _                                                      => tp
  }

  // parseStats, returning status but no trees
  def parseString(line: String): Result =
    reporter.suppressOutput {
      parse(line).fold(e => e, _ => Success)
    }

  def tokenize(line: String): List[TokenData] = {
    import collection.mutable.ListBuffer
    val u = newUnitScanner(newCompilationUnit(line))
    u.init()
    val b = ListBuffer.empty[Int]
    while (u.token != 0) {
      b += u.lastOffset
      b += u.token
      b += u.offset
      u.nextToken()
    }
    b += u.lastOffset
    import scala.tools.nsc.ast.parser.Tokens.isIdentifier
    b.drop(1).grouped(3).flatMap(triple => triple.toList match {
      case List(token, start, end) => Some(TokenData(token, start, end, isIdentifier(token)))
      case _ => println(s"Skipping token ${scala.runtime.ScalaRunTime.stringOf(triple)}") ; None
    }).toList
  }

  /**
    *  Interpret one line of input. All feedback, including parse errors
    *  and evaluation results, are printed via the supplied compiler's
    *  reporter. Values defined are available for future interpreted strings.
    *
    *  The return value is whether the line was interpreted successfully,
    *  e.g. that there were no parse errors.
    */
  override def interpretFinally(line: String): Result = doInterpret(line, synthetic = false, fatally = true)
  override def interpret(line: String): Result = interpret(line, synthetic = false)
  def interpretSynthetic(line: String): Result = interpret(line, synthetic = true)
  override def interpret(line: String, synthetic: Boolean): Result = doInterpret(line, synthetic, fatally = false)
  private def doInterpret(line: String, synthetic: Boolean, fatally: Boolean): Result = {
    def loadAndRunReq(req: Request) = classLoader.asContext {
      val res = req.loadAndRun // TODO: move classLoader.asContext into loadAndRun ?

      reporter.printResult(res)

      if (res.isLeft) Error
      else {
        // Book-keeping.  Have to record synthetic requests too,
        // as they may have been issued for information, e.g. :type
        recordRequest(req)
        Success
      }
    }

    compile(line, synthetic, fatally).fold(identity, loadAndRunReq).tap(res =>
      // besides regular errors, clear deprecation and feature warnings
      // so they don't leak from last compilation run into next provisional parse
      if (res != Incomplete) {
        reporter.reset()
        currentRun.reporting.clearAllConditionalWarnings()
      }
    )
  }

  // create a Request and compile it if input is complete
  def compile(line: String, synthetic: Boolean): Either[Result, Request] = compile(line, synthetic, fatally = false)
  def compile(line: String, synthetic: Boolean, fatally: Boolean): Either[Result, Request] =
    if (global == null) Left(Error)
    else requestFromLine(line, synthetic, fatally).filterOrElse(_.compile, Error)

  /** Bind a specified name to a specified value.  The name may
    *  later be used by expressions passed to interpret.
    *
    *  A fresh `ReadEvalPrint`, which defines a `line` package, is used to compile
    *  a custom `eval` object that wraps the bound value.
    *
    *  If the bound value is successfully installed, then bind the name
    *  by interpreting `val name = \$line42.\$eval.value`.
    *
    *  @param name      the variable name to bind
    *  @param boundType the type of the variable, as a string
    *  @param value     the object value to bind to it
    *  @return          an indication of whether the binding succeeded
    */
  override def bind(name: String, boundType: String, value: Any, modifiers: List[String] = Nil): Result = {
    val bindRep = new ReadEvalPrint()
    bindRep.compile(s"""
                       |object ${bindRep.evalName} {
                       |  var value: $boundType = _
                       |  def set(x: _root_.scala.Any) = value = x.asInstanceOf[$boundType]
                       |}
      """.stripMargin
    )
    bindRep.callEither("set", value) match {
      case Left(ex) =>
        repldbg(s"Set failed in bind($name, $boundType, $value)")
        repldbg(stackTraceString(ex))
        Error
      case Right(_) =>
        val mods = if (modifiers.isEmpty) "" else modifiers.mkString("", " ", " ")
        val line = s"${mods}val $name = ${ bindRep.evalPath }.value"
        repldbg(s"Interpreting: $line")
        interpret(line)
    }
  }
  def directBind(name: String, boundType: String, value: Any): Result = {
    val result = bind(name, boundType, value)
    if (result == Success)
      directlyBoundNames += newTermName(name)
    result
  }
  def directBind(p: NamedParam): Result                                    = directBind(p.name, p.tpe, p.value)
  def directBind[T: ru.TypeTag : ClassTag](name: String, value: T): Result = directBind((name, value))

  def namedParam[T: ru.TypeTag : ClassTag](name: String, value: T): NamedParam = NamedParam[T](name, value)

  def rebind(p: NamedParam): Result = {
    val name     = p.name
    val newType  = p.tpe
    val tempName = freshInternalVarName()

    quietRun(s"val $tempName = $name")
    quietRun(s"val $name = $tempName.asInstanceOf[$newType]")
  }
  override def quietBind(p: NamedParam): Result                               = reporter.withoutPrintingResults(bind(p))
  override def bind(p: NamedParam): Result                                    = bind(p.name, p.tpe, p.value)
  def bind[T: ru.TypeTag : ClassTag](name: String, value: T): Result = bind((name, value))

  /** Reset this interpreter, forgetting all user-specified requests. */
  override def reset(): Unit = {
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
  override def close(): Unit = {
    reporter.flush()
    if (initializeComplete) {
      global.close()
    }
  }

  override lazy val power = new Power(this, new StdReplVals(this))(tagOfStdReplVals, classTag[StdReplVals])

  /** Here is where we:
   *
   *  1) Read some source code, and put it in the "read" object.
   *  2) Evaluate the read object, and put the result in the "eval" object.
   *  3) Create a String for human consumption, and put it in the "print" object.
   *
   *  Read! Eval! Print! Some of that not yet centralized here.
   */
  class ReadEvalPrint(val lineId: Int = freshLineId()) {
    val packageName = sessionNames.packageName(lineId)
    val readName    = sessionNames.read
    val evalName    = sessionNames.eval
    val printName   = sessionNames.print
    val resultName  = sessionNames.result

    def bindError(t: Throwable) = {
      import scala.tools.nsc.util.StackTraceOps

      if (!bindExceptions) // avoid looping if already binding
        throw t

      val unwrapped = rootCause(t)

      // Example input: $line3.$read$$iw$
      val classNameRegex = lineRegex
      def isWrapperCode(x: StackTraceElement) =
        x.getMethodName == nme.CONSTRUCTOR.decoded || x.getMethodName == "<clinit>" || x.getMethodName == printName && classNameRegex.pattern.matcher(x.getClassName).find()

      val stackTrace = unwrapped.stackTracePrefixString(!isWrapperCode(_))
      withLastExceptionLock[String]({
        directBind[Throwable]("lastException", unwrapped)(StdReplTags.tagOfThrowable, classTag[Throwable])
        stackTrace
      }, stackTrace)
    }

    // TODO: split it out into a package object and a regular
    // object and we can do that much less wrapping.
    def packageDecl = "package " + packageName

    def pathToInstance(name: String)   = packageName + "." + name + readInstanceName
    def pathTo(name: String)   = packageName + "." + name
    def packaged(code: String) = packageDecl + "\n\n" + code

    def readPathInstance  = pathToInstance(readName)
    def readPath = pathTo(readName)
    def evalPath = pathTo(evalName)

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

    class EvalException(msg: String, cause: Throwable) extends RuntimeException(msg, cause) { }

    private def evalError(path: String, ex: Throwable) =
      throw new EvalException("Failed to load '" + path + "': " + ex.getMessage, ex)

    private def load(path: String): Class[_] = {
      try Class.forName(path, true, classLoader)
      catch { case ex: Throwable => evalError(path, rootCause(ex)) }
    }

    lazy val evalClass = load(evalPath)

    def evalEither: Either[Throwable, AnyRef] = callEither(resultName) match {
      case Right(result)                 => Right(result)
      case Left(_: NullPointerException) => Right(null)
      case Left(e)                       => Left(rootCause(e))
    }

    /** The innermost object inside the wrapper, found by
      * following accessPath into the outer one.
      */
    def resolvePathToSymbol(fullAccessPath: String): Symbol = {
      val accessPath = fullAccessPath.stripPrefix(readPath)
      val readRoot = readRootPath(readPath) // the outermost wrapper
      (accessPath split '.').foldLeft(readRoot: Symbol) {
        case (sym, "")    => sym
        case (sym, name)  => exitingTyper(termMember(sym, name))
      }
    }
    /** We get a bunch of repeated warnings for reasons I haven't
      *  entirely figured out yet.  For now, squash.
      */
    private def updateRecentWarnings(run: Run): Unit = {
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
      val warnings = loop(run.reporting.allConditionalWarnings)
      if (warnings.nonEmpty)
        mostRecentWarnings = warnings
    }
    private def evalMethod(name: String) = evalClass.getMethods filter (_.getName == name) match {
      case Array()       => null
      case Array(method) => method
      case xs            => throw new IllegalStateException(s"Internal error: eval object $evalClass, ${xs.mkString("\n", "\n", "")}")
    }

    def compile(code: String): Boolean =
      compile(new CompilationUnit(new BatchSourceFile(label, packaged(code))))

    def compile(unit: CompilationUnit): Boolean = {
      val oldRunReporting = currentRun.reporting
      val run = new Run()
      // The unit is already parsed and won't be parsed again. This makes sure suspended warnings are not discarded.
      run.reporting.initFrom(oldRunReporting)
      assert(run.typerPhase != NoPhase, "REPL requires a typer phase.")

      run.compileUnits(unit :: Nil)
      val success = !reporter.hasErrors

      updateRecentWarnings(run)

      success
    }
  }

  @inline private final def tyParens[T](ts: Iterable[T]): String       = ts.mkString("[", ", ", "]")
  @inline private final def implicitParens[T](ts: Iterable[T]): String = ts.mkString("(implicit ", ", ", ")")
  @inline private final def parens[T](ts: Iterable[T]): String         = ts.mkString("(", ", ", ")")

  private def methodTypeAsDef(tp: Type): String = {

    def withoutImplicit(sym: Symbol): Symbol = sym.cloneSymbol(sym.owner, sym.flags & ~Flag.IMPLICIT)

    def formatParams(params: List[Symbol]): String = {
      if (params.headOption.exists(_.isImplicit)) implicitParens(params.map(withoutImplicit(_).defString))
      else parens(params.map(_.defString))
    }

    def loop(tpe: Type, acc: StringBuilder): StringBuilder = tpe match {
      case NullaryMethodType(resultType)  => acc ++= s": ${typeToCode(resultType.toString)}"
      case PolyType(tyParams, resultType) => loop(resultType, acc ++= tyParens(tyParams.map(_.defString)))
      case MethodType(params, resultType) => loop(resultType, acc ++= formatParams(params))
      case other                          => acc ++= s": ${typeToCode(other.toString)}"
    }

    loop(tp, new StringBuilder).toString
  }

  /** One line of code submitted by the user for interpretation */
  class Request(val line: String, origTrees: List[Tree], val parserSource: BatchSourceFile, firstXmlPos: Position = NoPosition,
                generousImports: Boolean = false, synthetic: Boolean = false, storeResultInVal: Boolean = true) extends ReplRequest {
    def defines    = defHandlers flatMap (_.definedSymbols)
    def definesTermNames: List[String] = defines collect { case s: TermSymbol => s.decodedName.toString }
    def imports    = importedSymbols
    def value      = Some(handlers.last) filter (h => h.definesValue) map (h => definedSymbols(h.definesTerm.get)) getOrElse NoSymbol
    val lineRep = new ReadEvalPrint()

    def eval: Either[Throwable, AnyRef] = lineRep.evalEither

    // The source file contents only has the code originally input by the user,
    // with unit's body holding the synthetic trees.
    // When emitting errors, be careful not to refer to the synthetic code
    // pad with a trailing " " so that the synthetic position for enclosing trees does not exactly coincide with the
    // position of the user-written code, these seems to confuse the presentation compiler.
    private val paddedLine = line + " "
    private val unit = new CompilationUnit(new ReplBatchSourceFile(if (synthetic) "<synthetic>" else label, paddedLine, parserSource))
    // a dummy position used for synthetic trees (needed for pres compiler to locate the trees for user input)
    private val wholeUnit = Position.range(unit.source, 0, 0, paddedLine.length)

    private def storeInVal(tree: Tree): Tree = {
      val resName = newTermName(if (synthetic) freshInternalVarName() else freshUserVarName())
      atPos(tree.pos.makeTransparent)(ValDef(NoMods, resName, TypeTree(), tree))
    }

    // Wrap last tree in a valdef to give user a nice handle for it (`resN`)
    val trees: List[Tree] = origTrees match {
      case xs if !storeResultInVal => xs
      case init :+ tree =>
        def loop(scrut: Tree): Tree = scrut match {
          case _: Assign                => tree
          case Apply(Select(_, op), _) if Precedence(op.decoded).level == 0 => tree
          case _: RefTree | _: TermTree => storeInVal(tree)
          case Annotated(_, arg)        => loop(arg)
          case _                        => tree
        }
        init :+ loop(tree)
      case xs =>
        xs // can get here in completion of erroneous code
    }

    /** handlers for each tree in this request */
    val handlers: List[MemberHandler] = trees.map(memberHandlers.chooseHandler(_))
    val definesValueClass = handlers.exists(_.definesValueClass)

    val isClassBased = IMain.this.isClassBased && !definesValueClass

    def defHandlers = handlers collect { case x: MemberDefHandler => x }

    /** list of names used by this expression */
    val referencedNames: List[Name] = handlers flatMap (_.referencedNames)

    /** def and val names */
    def termNames = handlers flatMap (_.definesTerm)
    def typeNames = handlers flatMap (_.definesType)
    def importedSymbols = handlers flatMap {
      case x: ImportHandler => x.importedSymbols
      case _                => Nil
    }

    /** The path of the value that contains the user code. */
    def fullAccessPath = s"${lineRep.readPathInstance}$accessPath"

    /** The path of the given member of the wrapping instance. */
    def fullPath(vname: String) = s"$fullAccessPath.`$vname`"

    /** Code to import bound names from previous lines - accessPath is code to
      * append to objectName to access anything bound by request.
      */
    lazy val ComputedImports(headerPreamble, importsPreamble, importsTrailer, accessPath) =
      exitingTyper(importsCode(referencedNames.toSet, this, generousImports))


    private val USER_CODE_PLACEHOLDER = newTermName("$user_code_placeholder$")
    private object spliceUserCode extends AstTransformer {
      var parents: List[Tree] = Nil
      override def transform(tree: Tree): Tree = {
        parents ::= tree
        try super.transform(tree)
        finally parents = parents.tail
      }

      override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] =
        stats flatMap {
          case Ident(USER_CODE_PLACEHOLDER) =>
            parents.foreach(p => p.setPos(wholeUnit))
            trees
          case t => List(transform(t))
        }
    }

    private def parseSynthetic(code: String): List[Tree] = {
      val stats = newUnitParser(code, "<synthetic>").parseStats()
      stats foreach {_.foreach(t => t.pos = t.pos.makeTransparent)}
      stats
    }

    /** generate the source code for the object that computes this request */
    def mkUnit: CompilationUnit = {
      val readName = newTermName(lineRep.readName)

      val stats = ListBuffer.empty[Tree]

      stats ++= parseSynthetic(headerPreamble)

      // the imports logic builds up a nesting of object $iw wrappers :-S
      // (have to parse importsPreamble + ... + importsTrailer at once)
      // This will be simplified when we stop wrapping to begin with.
      val syntheticStats =
        parseSynthetic(importsPreamble + s"`$USER_CODE_PLACEHOLDER`" + importsTrailer) ++
        (if (isClassBased) Nil else List(q"val INSTANCE = this")) // Add a .INSTANCE accessor to the read object, so access is identical to class-based

      // don't use empty list of parents, since that triggers a rangepos bug in typer (the synthetic typer tree violates overlapping invariant)
      val parents = List(atPos(wholeUnit.focus)(if (isClassBased) gen.rootScalaDot(tpnme.Serializable) else gen.rootScalaDot(tpnme.AnyRef)))

      val wrapperTempl = gen.mkTemplate(parents, noSelfType, NoMods, ListOfNil, syntheticStats, superPos = wholeUnit.focus)

      stats += (
        if (isClassBased) ClassDef(Modifiers(Flags.SEALED), readName.toTypeName, Nil, wrapperTempl)
        else ModuleDef(NoMods, readName, wrapperTempl))

      if (isClassBased)
        stats += atPos(wholeUnit.focus)(q"""object $readName { val INSTANCE = new ${tq"""${readName.toTypeName}"""} }""")

      val unspliced = PackageDef(atPos(wholeUnit.focus)(Ident(lineRep.packageName)), stats.toList)
      unit.body = spliceUserCode.transform(unspliced)
      unit.encounteredXml(firstXmlPos)

      // settings.Xprintpos.value = true
      showCode(asCompactString(unit.body))

      unit
    }

    // Secret bookcase entrance for repl debuggers: end the line
    // with "// show" and see what's going on.
    private def showCode(code: => String) =
      if (reporter.isDebug || (label == "<console>" && line.contains("// show")))
        reporter.withoutUnwrapping(reporter.withoutTruncating(reporter.echo(code)))


    // used for import wrapping (this will go away entirely when we use the more direct approach)
    def wrapperDef(iw: String) =
      if (isClassBased) s"sealed class $iw extends _root_.java.io.Serializable"
      else s"object $iw"

    import nme.{ INTERPRETER_IMPORT_WRAPPER => iw }
    /** Like postamble for an import wrapper. */
    def postwrap: String =
      if (isClassBased) s"val $iw = new $iw"
      else ""


    private def mkResultUnit(contributors: List[MemberHandler]): CompilationUnit = {
      // The symbol defined by the last member handler
      val resValSym = value

      val extractionCode = stringFromWriter { code =>
        code.println(s"""
           |${lineRep.packageDecl} {
           |object ${lineRep.evalName} {
           |  ${if (resValSym != NoSymbol) s"lazy val ${lineRep.resultName} = ${originalPath(resValSym)}" else ""}
           |  lazy val ${lineRep.printName}: _root_.java.lang.String = $executionWrapper {
           |    val _ = $fullAccessPath
           |""".stripMargin)
        if (contributors.lengthCompare(1) > 0) {
          code.println("val sb = new _root_.scala.StringBuilder")
          contributors foreach (x => code.println(s"""sb.append("" ${x.resultExtractionCode(this)})"""))
          code.println("sb.toString")
        } else {
          code.print(""""" """) // start with empty string
          contributors foreach (x => code.print(x.resultExtractionCode(this)))
          code.println()
        }
        code.println(s"""
           |  }
           |}}""".stripMargin)
        }

      showCode(extractionCode)

      new CompilationUnit(new BatchSourceFile("<synthetic>", extractionCode))
    }


    /** Compile the object file.  Returns whether the compilation succeeded.
     *  If all goes well, the "types" map is computed.
     */
    def compile: Boolean = {
      val compiledOK =
        if (origTrees.forall(_.isInstanceOf[Import])) {
          //settings.Wconf.value ::= "cat=unused-imports:s" // alternatively, suppression is effective
          val saved = settings.warnUnused.value
          settings.warnUnused.value -= settings.UnusedWarnings.Imports
          try lineRep.compile(mkUnit)
          finally settings.warnUnused.value = saved
        }
        else lineRep.compile(mkUnit)

      // compile the object containing the user's code
      compiledOK && {
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
        // TODO: can we remove dependency on reporter's printResults state?
        val handls = if (reporter.printResults) handlers else Nil
        withoutWarnings(lineRep compile mkResultUnit(handls))
      }
    }

    // the type symbol of the owner of the member that supplies the result value
    lazy val resultSymbol = lineRep.resolvePathToSymbol(fullAccessPath)

    def applyToResultMember[T](name: Name, f: Symbol => T) = exitingTyper(f(resultSymbol.info.nonPrivateDecl(name)))

    /* typeOf lookup with encoding */
    def lookupTypeOf(name: Name) = typeOf.getOrElse(name, typeOf(global.encode(name.toString)))

    private def typeMap[T](f: Type => T) =
      mapFrom[Name, Name, T](termNames ++ typeNames)(x => f(cleanMemberDecl(resultSymbol, x)))

    /** Types of variables defined by this request. */
    lazy val compilerTypeOf = typeMap[Type](x => x) withDefaultValue NoType
    /** String representations of same. */
    lazy val typeOf         = typeMap[String](tp => exitingTyper {
      tp.toString.stripPrefix("INSTANCE.")
    })
    /** String representations as if a method type. */
    private[this] lazy val defTypeOfMap = typeMap[String](tp => exitingTyper(methodTypeAsDef(tp)))

    def defTypeOf(name: Name)(implicit show: Name => String): String = show(name) + defTypeOfMap(name)

    lazy val definedSymbols = (
      termNames.map(x => x -> applyToResultMember(x, x => x)) ++
        typeNames.map(x => x -> compilerTypeOf(x).typeSymbolDirect)
      ).toMap[Name, Symbol] withDefaultValue NoSymbol

    lazy val typesOfDefinedTerms = mapFrom[Name, Name, Type](termNames)(x => applyToResultMember(x, _.tpe))

    /** load and run the code using reflection */
    def loadAndRun: Either[String, String] = {
      try   { Right("" + (lineRep call sessionNames.print)) }
      catch { case ex: Throwable => Left(lineRep.bindError(ex)) }
    }

    override def toString = s"Request(line=$line, ${trees.size} trees)"
  }

  override def finalize = close

  /** Returns the name of the most recent interpreter result.
    *  Mostly this exists so you can conveniently invoke methods on
    *  the previous result.
    */
  override def mostRecentVar: String =
    if (mostRecentlyHandledTree.isEmpty) ""
    else "" + (mostRecentlyHandledTree.get match {
      case x: ValOrDefDef           => x.name
      case Assign(Ident(name), _)   => name
      case ModuleDef(_, name, _)    => name
      case _                        => naming.mostRecentVar
    })

  private var mostRecentWarnings: List[(global.Position, String)] = Nil
  override def lastWarnings: List[(global.Position, String)] = mostRecentWarnings

  private lazy val importToGlobal  = global mkImporter ru
  private lazy val importToRuntime = ru.internal createImporter global
  private lazy val javaMirror = ru.rootMirror match {
    case x: ru.JavaMirror => x
    case _                => null
  }
  private implicit def importFromRu(sym: ru.Symbol): Symbol = importToGlobal importSymbol sym
  private implicit def importToRu(sym: Symbol): ru.Symbol   = importToRuntime importSymbol sym

  def classOfTerm(id: String): Option[Class[_]] = symbolOfTerm(id) match {
    case NoSymbol => None
    case sym      => Some(javaMirror runtimeClass importToRu(sym).asClass)
  }

  def typeOfTerm(id: String): Type = symbolOfTerm(id).tpe

  // Given the fullName of the symbol, reflectively drill down the path
  def valueOfTerm(id: String): Option[Any] = exitingTyper {
    def value(fullName: String) = {
      val runtimeMirror = this.runtimeMirror
      import runtimeMirror.universe.{Symbol, InstanceMirror, TermName}
      val pkg :: path = fullName.split('.').toList: @unchecked
      val top = runtimeMirror.staticPackage(pkg)
      def loop(inst: InstanceMirror, cur: Symbol, path: List[String]): Option[Any] = {
        def mirrored =
          if (inst != null) inst
          else runtimeMirror.reflect(runtimeMirror.reflectModule(cur.asModule).instance)

        path match {
          case last :: Nil  =>
            cur.typeSignature.decls.find(x => x.name.toString == last && x.isAccessor).map { m =>
              mirrored.reflectMethod(m.asMethod).apply()
            }
          case next :: path =>
            val sym = cur.typeSignature.member(TermName(next))
            val inst1 =
              if (sym.isModule)
                if (inst == null) null
                else runtimeMirror.reflect(runtimeMirror.reflectModule(sym.asModule).instance)
              else if (sym.isAccessor) runtimeMirror.reflect(mirrored.reflectMethod(sym.asMethod).apply())
              else throw new AssertionError(s"not a module or accessor ($sym) in: $fullName")
            loop(inst1, sym, path)
          case Nil => None
        }
      }
      loop(inst = null, top, path)
    }
    Option(symbolOfTerm(id)).filter(_.exists).flatMap(s => Trying(value(originalPath(s))).toOption.flatten)
  }

  /** It's a bit of a shotgun approach, but for now we will gain in
   *  robustness. Try a symbol-producing operation at phase typer, and
   *  if that is NoSymbol, try again at phase flatten. I'll be able to
   *  lose this and run only from exitingTyper as soon as I figure out
   *  exactly where a flat name is sneaking in when calculating imports.
   */
  def tryTwice(op: => Symbol): Symbol = exitingTyper(op) orElse exitingFlatten(op)

  def symbolOfIdent(id: String): Symbol  = symbolOfType(id) orElse symbolOfTerm(id)
  def symbolOfType(id: String): Symbol   = tryTwice(replScope lookup TypeName(id))
  def symbolOfTerm(id: String): Symbol   = tryTwice(replScope lookup TermName(id))
  def symbolOfName(id: Name): Symbol     = replScope lookup id

  def runtimeClassAndTypeOfTerm(id: String): Option[(Class[_], Type)] = {
    // Sadly isAnonymousClass does not return true for scala anonymous
    // classes because our naming scheme is not doing well against the
    // jvm's many assumptions.
    def isScalaAnonymous(clazz: Class[_]) =
      try clazz.isAnonymousClass || (clazz.getName contains "$anon$")
      catch { case _: java.lang.InternalError => false }  // good ol' "Malformed class name"


    def supers(clazz: Class[_]): List[Class[_]] = {
      def loop(x: Class[_]): List[Class[_]] = x.getSuperclass match {
        case null   => List(x)
        case sc     => x :: (x.getInterfaces.toList flatMap loop) ++ loop(sc)
      }
      loop(clazz).distinct
    }

    classOfTerm(id) flatMap { clazz =>
      supers(clazz) find (cls => !isScalaAnonymous(cls)) map { nonAnon =>
        (nonAnon, runtimeTypeOfTerm(id))
      }
    }
  }

  def runtimeTypeOfTerm(id: String): Type = {
    def conformingRuntimeType(tpe: Type, clazz: Class[_]): Type = {
      val staticSym  = tpe.typeSymbol
      val runtimeSym = getClassIfDefined(clazz.getName)

      if (runtimeSym != NoSymbol && runtimeSym != staticSym && runtimeSym.isSubClass(staticSym))
        runtimeSym.info
      else NoType
    }
    typeOfTerm(id).andAlso(tpe => classOfTerm(id).map(conformingRuntimeType(tpe, _)).getOrElse(NoType))
  }

  def cleanTypeAfterTyper(sym: => Symbol): Type = {
    exitingTyper(
      dealiasNonPublic(
        dropNullaryMethod(
          sym.tpe_*
        )
      )
    )
  }
  // this is harder than getting the typed trees and fixing up the string to emit that reports types
  def cleanMemberDecl(owner: Symbol, member: Name): Type =
    cleanTypeAfterTyper(owner.info nonPrivateDecl member)

  object exprTyper extends {
    val repl: IMain.this.type = IMain.this
  } with ExprTyper { }

  /** Parse a line into and return parsing result (error, incomplete or success with list of trees) */
  def parse(line: String): Either[Result, (List[Tree], Position, BatchSourceFile)] = parse(line, fatally = false)
  def parse(line: String, fatally: Boolean): Either[Result, (List[Tree], Position, BatchSourceFile)] = {
    var isIncomplete = false
    val handler = if (fatally) null else (_: Position, _: String) => isIncomplete = true
    currentRun.parsing.withIncompleteHandler(handler) {
      val source = newSourceFile(line, label)
      val unit = new CompilationUnit(source)
      val trees = newUnitParser(unit).parseStats()
      if (!isIncomplete)
        runReporting.summarizeErrors()
      if (reporter.hasErrors) Left(Error)
      else if (isIncomplete) Left(Incomplete)
      else if (reporter.hasWarnings && settings.fatalWarnings.value) Left(Error)
      else Right((trees, unit.firstXmlPos, source))
    }.tap(_ => if (!isIncomplete) reporter.reset())
  }

  /** Does code start with a package definition? */
  def isPackaged(line: String): Boolean =
    !reporter.hasErrors && reporter.suppressOutput {
      reporter.reset()
      val tree = newUnitParser(line).parse()
      val res  = !reporter.hasErrors && {
        tree match {
          case PackageDef(Ident(id), _) => id != nme.EMPTY_PACKAGE_NAME
          case _ => false
        }
      }
      if (reporter.hasErrors) reporter.reset()
      res
    }

  def symbolOfLine(code: String): Symbol =
    exprTyper.symbolOfLine(code)

  def typeOfExpression(expr: String, silent: Boolean = true): Type =
    exprTyper.typeOfExpression(expr, silent)

  private def getterToResultTp(sym: Symbol) = if (sym.isGetter) sym.info.resultType else sym.info

  override def implicitsCommandInternal(line: String): (List[String], String) = {
    val implicits = collection.mutable.ListBuffer.empty[String]

    // If an argument is given, only show a source with that
    // in its name somewhere.
    val args     = line split "\\s+"
    val filtered = intp.implicitSymbolsBySource filter {
      case (source, syms) =>
        (args contains "-v") || {
          if (line == "") (source.fullName.toString != "scala.Predef")
          else (args exists (source.name.toString contains _))
        }
    }

    filtered foreach {
      case (source, syms) =>
        implicits += "/* " + syms.size + " implicit members imported from " + source.fullName + " */"

        // This groups the members by where the symbol is defined
        val byOwner = syms groupBy (_.owner)
        val sortedOwners = byOwner.toList sortBy { case (owner, _) => exitingTyper(source.info.baseClasses indexOf owner) }

        sortedOwners foreach {
          case (owner, members) =>
            // Within each owner, we cluster results based on the final result type
            // if there are more than a couple, and sort each cluster based on name.
            // This is really just trying to make the 100 or so implicits imported
            // by default into something readable.
            val memberGroups: List[List[Symbol]] = {
              val groups = members.groupBy(_.tpe.finalResultType).toList
              val (big, small) = groups partition (_._2.size > 3)
              val xss = (
                (big sortBy (_._1.toString) map (_._2)) :+
                  (small flatMap (_._2))
                )

              xss map (xs => xs sortBy (_.name.toString))
            }

            val ownerMessage = if (owner == source) " defined in " else " inherited from "
            implicits += "  /* " + members.size + ownerMessage + owner.fullName + " */"

            memberGroups foreach { group =>
              group foreach (s => implicits += "  " + intp.symbolDefString(s))
              implicits += ""
            }
        }
        implicits += ""
    }

    (implicits.toList,
      if (filtered.nonEmpty)
        "" // collected in implicits
      else if (global.settings.nopredef.value || global.settings.noimports.value)
        "No implicits have been imported."
      else
        "No implicits have been imported other than those in Predef."
    )
  }

  override def kindCommandInternal(typeString: String, verbose: Boolean): String = {
    import scala.util.control.Exception.catching
    val catcher = catching(classOf[MissingRequirementError],
      classOf[ScalaReflectionException])

    def kindMsg(tpe: Type, kind: Kind, verbose: Boolean): String = {
      val msg = exitingTyper(tpe.toString) + "'s kind is " + kind.scalaNotation
      if (verbose) msg +"\n"+ kind.starNotation +"\n"+ kind.description
      else msg
    }

    val tpe: Type = exprTyper.typeOfTypeString(typeString)
    (tpe match {
      case NoType =>
        Some(s"<console>: error: type $typeString was not found")
      // This is a special handling for type lambdas
      case TypeRef(pre, sym, args) if args contains WildcardType =>
        catcher opt {
          val kind = exitingTyper {
            val sym = tpe.typeSymbol.asClass
            val owner = sym.owner
            val kind0 = intp.global.inferKind(NoPrefix)(TypeRef(pre, sym, Nil), owner)
            kind0 match {
              case TypeConKind(bounds, kargs) if args.size == kargs.size =>
                TypeConKind(bounds, (args.toList zip kargs.toList) flatMap {
                  case (WildcardType, karg) => List(karg)
                  case _ => Nil
                })
              case k => k
            }
          }
          kindMsg(tpe, kind, verbose)
        }
      case _ =>
        catcher opt {
          val kind = exitingTyper {
            val sym = tpe.typeSymbol.asClass
            val owner = sym.owner
            intp.global.inferKind(NoPrefix)(tpe, owner)
          }
          kindMsg(tpe, kind, verbose)
        }
    }).getOrElse("")
  }

  /** TODO -
    *  -n normalize
    *  -l label with case class parameter names
    *  -c complete - leave nothing out
    */
  override def typeCommandInternal(expr: String, verbose: Boolean): (String, String) = {

    // first assume that most user entering into :type command would put in an existing term name
    // rather than some expression that requires additional interpretation.
    (symbolOfTerm(expr) orElse symbolOfLine(expr)) match {
      case NoSymbol => ("", "")
      case sym => exitingTyper {
        (getterToResultTp(sym).toString, if (verbose) deconstruct.show(getterToResultTp(sym)).toString else "")
      }
    }

  }

  def importsCommandInternal(tokens: List[String]): List[String] = {
    val handlers  = languageWildcardHandlers ++ importHandlers

    handlers.filterNot(_.importedSymbols.isEmpty).zipWithIndex map {
      case (handler, idx) =>
        val (types, terms) = handler.importedSymbols partition (_.name.isTypeName)
        val imps           = handler.implicitSymbols
        val found          = tokens filter (handler importsSymbolNamed _)
        val typeMsg        = if (types.isEmpty) "" else s"${types.size} types"
        val termMsg        = if (terms.isEmpty) "" else s"${terms.size} terms"
        val implicitMsg    = if (imps.isEmpty) "" else s"${imps.size} are implicit"
        val foundMsg       = if (found.isEmpty) "" else found.mkString(" // imports: ", ", ", "")
        val statsMsg       = List(typeMsg, termMsg, implicitMsg).filterNot(_ == "").mkString("(", ", ", ")")

        f"${idx + 1}%2d) ${handler.importString}%-30s $statsMsg$foundMsg"
    }
  }

  protected def onlyTerms(xs: List[Name]): List[TermName] = xs collect { case x: TermName => x }
  protected def onlyTypes(xs: List[Name]): List[TypeName] = xs collect { case x: TypeName => x }

  def definedTerms = onlyTerms(allDefinedNames) filterNot isInternalTermName
  def definedTypes: List[String]      = onlyTypes(allDefinedNames) map (_.toString)
  def definedSymbolList = prevRequestList flatMap (_.defines) filterNot (s => isInternalTermName(s.name))

  // Terms with user-given names (i.e. not res0 and not synthetic)
  override def namedDefinedTerms: List[String] = definedTerms filterNot (x => isUserVarName("" + x) || directlyBoundNames(x)) map (_.toString)

  private var _replScope: Scope = _
  private def resetReplScope(): Unit = {
    _replScope = newScope
  }
  def replScope = {
    if (_replScope eq null)
      _replScope = newScope

    _replScope
  }

  private var executingRequest: Request = _
  private val prevRequests       = mutable.ListBuffer[Request]()
  private val directlyBoundNames = mutable.Set[Name]()

  def allHandlers     = prevRequestList flatMap (_.handlers)
  def lastRequest     = if (prevRequests.isEmpty) null else prevRequests.last
  def prevRequestList = prevRequests.toList
  def importHandlers  = allHandlers collect { case x: ImportHandler => x }

  def symbolDefString(sym: Symbol): String = {
    TypeStrings.quieter(
      exitingTyper(sym.defString),
      s"${sym.owner.name}.this.",
      sym.owner.fullName + "."
    )
  }

  def replStrings: ReplStrings = reporter
  def nameToCode(s: String)    = replStrings.nameToCode(s)
  def typeToCode(s: String)    = replStrings.typeToCode(s)

  // debugging
  def debugging[T](msg: String)(res: T) = {
    repldbg(msg + " " + res)
    res
  }
}

/** Utility methods for the Interpreter. */
object IMain {
  /** Dummy identifier fragment inserted at the cursor before presentation compilation.
   *  Needed to support completion of `global.def<TAB>`.
   */
  final val DummyCursorFragment = "_CURSOR_"

  /** Temporarily suppress some noisy settings.
   */
  private[interpreter] def withSuppressedSettings[A](settings: Settings, global: => Global)(body: => A): A = {
    import settings.{reporter => _, _}
    val wasWarning = !nowarn.value
    val oldMaxWarn = maxwarns.value
    val noisy = List(Xprint, Ytyperdebug, browse)
    val current = (Xprint.value, Ytyperdebug.value, browse.value)
    val noisesome = wasWarning || noisy.exists(!_.isDefault)
    if (/*isDebug ||*/ !noisesome) body
    else {
      Xprint.value = List.empty
      browse.value = List.empty
      Ytyperdebug.value = false
      if (wasWarning) nowarn.value = true
      try body
      finally {
        Xprint.value       = current._1
        Ytyperdebug.value  = current._2
        browse.value       = current._3
        if (wasWarning) {
          nowarn.value = false
          maxwarns.value = oldMaxWarn
        }
        // ctl-D in repl can result in no compiler
        val g = global
        if (g != null) {
          g.printTypings = current._2
        }
      }
    }
  }
}
