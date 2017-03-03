/* NSC -- new Scala compiler
 * Copyright 2005-2016 LAMP/EPFL
 */
package scala
package tools.nsc
package interpreter

import scala.language.dynamics

import scala.beans.BeanProperty
import scala.collection.JavaConverters._
import scala.reflect.classTag
import scala.reflect.internal.util.Position
import scala.tools.nsc.util.stringFromReader
import javax.script._, ScriptContext.{ ENGINE_SCOPE, GLOBAL_SCOPE }
import java.io.{ Closeable, Reader }

/* A REPL adaptor for the javax.script API. */
class Scripted(@BeanProperty val factory: ScriptEngineFactory, settings: Settings, out: JPrintWriter)
  extends AbstractScriptEngine with Compilable {

  def createBindings: Bindings = new SimpleBindings

  // dynamic context bound under this name
  final val ctx = "$ctx"

  // the underlying interpreter, tweaked to handle dynamic bindings
  val intp = new IMain(settings, out) {
    import global.{ Name, TermName }

    /* Modify the template to snag definitions from dynamic context.
     * So object $iw { x + 42 } becomes object $iw { def x = $ctx.x ; x + 42 }
     */
    override protected def importsCode(wanted: Set[Name], wrapper: Request#Wrapper, definesClass: Boolean, generousImports: Boolean) = {

      // cull references that can be satisfied from the current dynamic context
      val contextual = wanted & contextNames

      if (contextual.nonEmpty) {
        val neededContext = (wanted &~ contextual) + TermName(ctx)
        val ComputedImports(header, preamble, trailer, path) = super.importsCode(neededContext, wrapper, definesClass, generousImports)
        val adjusted = contextual.map { n =>
            val valname = n.decodedName
            s"""def `$valname` = $ctx.`$valname`
                def `${valname}_=`(x: _root_.java.lang.Object) = $ctx.`$valname` = x"""
          }.mkString(preamble, "\n", "\n")
        ComputedImports(header, adjusted, trailer, path)
      }
      else super.importsCode(wanted, wrapper, definesClass, generousImports)
    }

    // names available in current dynamic context
    def contextNames: Set[Name] = {
      val ctx = compileContext
      val terms = for {
        scope   <- ctx.getScopes.asScala
        binding <- Option(ctx.getBindings(scope)) map (_.asScala) getOrElse Nil
        key = binding._1
      } yield (TermName(key): Name)
      terms.to[Set]
    }

    // save first error for exception; console display only if debugging
    override lazy val reporter: ReplReporter = new ReplReporter(this) {
      override def display(pos: Position, msg: String, severity: Severity): Unit =
        if (isReplDebug) super.display(pos, msg, severity)
      override def error(pos: Position, msg: String): Unit = {
        if (firstError.isEmpty) firstError = Some((pos, msg))
        super.error(pos, msg)
      }
      override def reset() = { super.reset() ; firstError = None }
    }
  }
  intp.initializeSynchronous()

  var compileContext: ScriptContext = getContext

  val scriptContextRep = new intp.ReadEvalPrint

  def dynamicContext_=(ctx: ScriptContext): Unit = scriptContextRep.callEither("set", ctx)

  def dynamicContext: ScriptContext = scriptContextRep.callEither("value") match {
    case Right(ctx: ScriptContext) => ctx
    case Left(e) => throw e
    case Right(other) => throw new ScriptException(s"Unexpected value for context: $other")
  }

  if (intp.isInitializeComplete) {
    // compile the dynamic ScriptContext object holder
    val ctxRes = scriptContextRep compile s"""
      |import _root_.javax.script._
      |object ${scriptContextRep.evalName} {
      |  var value: ScriptContext = _
      |  def set(x: _root_.scala.Any) = value = x.asInstanceOf[ScriptContext]
      |}
    """.stripMargin
    if (!ctxRes) throw new ScriptException("Failed to compile ctx")
    dynamicContext = getContext

    // Bridge dynamic references and script context
    val dynRes = intp compileString s"""
     |package scala.tools.nsc.interpreter
     |import _root_.scala.language.dynamics
     |import _root_.javax.script._, ScriptContext.ENGINE_SCOPE
     |object dynamicBindings extends _root_.scala.Dynamic {
     |  def context: ScriptContext = ${ scriptContextRep.evalPath }.value
     |  // $ctx.x retrieves the attribute x
     |  def selectDynamic(field: _root_.java.lang.String): _root_.java.lang.Object = context.getAttribute(field)
     |  // $ctx.x = v
     |  def updateDynamic(field: _root_.java.lang.String)(value: _root_.java.lang.Object) = context.setAttribute(field, value, ENGINE_SCOPE)
     |}
     |""".stripMargin
    if (!dynRes) throw new ScriptException("Failed to compile dynamicBindings")
    intp beQuietDuring {
      intp interpret s"val $ctx: _root_.scala.tools.nsc.interpreter.dynamicBindings.type = _root_.scala.tools.nsc.interpreter.dynamicBindings"
      intp bind ("$engine" -> (this: ScriptEngine with Compilable))
    }
  }

  // Set the context for dynamic resolution and run the body.
  // Defines attributes available for evaluation.
  // Avoid reflective access if using default context.
  def withScriptContext[A](context: ScriptContext)(body: => A): A =
    if (context eq getContext) body else {
      val saved = dynamicContext
      dynamicContext = context
      try body
      finally dynamicContext = saved
    }
  // Defines attributes available for compilation.
  def withCompileContext[A](context: ScriptContext)(body: => A): A = {
    val saved = compileContext
    compileContext = context
    try body
    finally compileContext = saved
  }

  // not obvious that ScriptEngine should accumulate code text
  private var code = ""

  private var firstError: Option[(Position, String)] = None

  /* All scripts are compiled. The supplied context defines what references
   * not in REPL history are allowed, though a different context may be
   * supplied for evaluation of a compiled script.
   */
  def compile(script: String, context: ScriptContext): CompiledScript =
    withCompileContext(context) {
      val cat = code + script
      intp.compile(cat, synthetic = false) match {
        case Right(req)          =>
          code = ""
          new WrappedRequest(req)
        case Left(IR.Incomplete) =>
          code = cat + "\n"
          new CompiledScript {
            def eval(context: ScriptContext): Object = null
            def getEngine: ScriptEngine = Scripted.this
          }
        case Left(_)             =>
          code = ""
          throw firstError map {
            case (pos, msg) => new ScriptException(msg, script, pos.line, pos.column)
          } getOrElse new ScriptException("compile-time error")
      }
    }

  // documentation
  //protected var context: ScriptContext
  //def getContext: ScriptContext = context

  /* Compile with the default context. All references must be resolvable. */
  @throws[ScriptException]
  def compile(script: String): CompiledScript = compile(script, context)

  @throws[ScriptException]
  def compile(reader: Reader): CompiledScript = compile(stringFromReader(reader), context)

  /* Compile and evaluate with the given context. */
  @throws[ScriptException]
  def eval(script: String, context: ScriptContext): Object = compile(script, context).eval(context)

  @throws[ScriptException]
  def eval(reader: Reader, context: ScriptContext): Object = compile(stringFromReader(reader), context).eval(context)

  private class WrappedRequest(val req: intp.Request) extends CompiledScript {
    var first = true

    private def evalEither(r: intp.Request, ctx: ScriptContext) = {
      if (ctx.getWriter == null && ctx.getErrorWriter == null && ctx.getReader == null) r.lineRep.evalEither
      else {
        val closeables = Array.ofDim[Closeable](2)
        val w = if (ctx.getWriter == null) Console.out else {
          val v = new WriterOutputStream(ctx.getWriter)
          closeables(0) = v
          v
        }
        val e = if (ctx.getErrorWriter == null) Console.err else {
          val v = new WriterOutputStream(ctx.getErrorWriter)
          closeables(1) = v
          v
        }
        val in = if (ctx.getReader == null) Console.in else ctx.getReader
        try {
          Console.withOut(w) {
            Console.withErr(e) {
              Console.withIn(in) {
                r.lineRep.evalEither
              }
            }
          }
        } finally {
          closeables foreach (c => if (c != null) c.close())
        }
      }
    }

    /* First time, cause lazy evaluation of a memoized result.
     * Subsequently, instantiate a new object for evaluation.
     * Per the API: Checked exception types thrown by underlying scripting implementations
     * must be wrapped in instances of ScriptException.
     */
    @throws[ScriptException]
    override def eval(context: ScriptContext) = withScriptContext(context) {
      if (first) {
        val result = evalEither(req, context) match {
          case Left(e: RuntimeException) => throw e
          case Left(e: Exception)        => throw new ScriptException(e)
          case Left(e)                   => throw e
          case Right(result)             => result.asInstanceOf[Object]
        }
        intp recordRequest req
        first = false
        result
      } else {
        val defines = req.defines
        if (defines.isEmpty) {
          Scripted.this.eval(s"new ${req.lineRep.readPath}")
          intp recordRequest duplicate(req)
          null
        } else {
          val instance = s"val $$INSTANCE = new ${req.lineRep.readPath};"
          val newline  = (defines map (s => s"val ${s.name} = $$INSTANCE${req.accessPath}.${s.name}")).mkString(instance, ";", ";")
          val newreq   = intp.requestFromLine(newline).right.get
          val ok = newreq.compile

          val result = evalEither(newreq, context) match {
            case Left(e: RuntimeException) => throw e
            case Left(e: Exception)        => throw new ScriptException(e)
            case Left(e)                   => throw e
            case Right(result)             => intp recordRequest newreq ; result.asInstanceOf[Object]
          }
          result
        }
      }
    }

    def duplicate(req: intp.Request) = new intp.Request(req.line, req.trees)

    def getEngine: ScriptEngine = Scripted.this
  }
}

object Scripted {
  import IMain.{ defaultSettings, defaultOut }
  import java.util.Arrays.asList
  import scala.util.Properties.versionString

  class Factory extends ScriptEngineFactory {
    @BeanProperty val engineName      = "Scala REPL"

    @BeanProperty val engineVersion   = "2.0"

    @BeanProperty val extensions      = asList("scala")

    @BeanProperty val languageName    = "Scala"

    @BeanProperty val languageVersion = versionString

    @BeanProperty val mimeTypes       = asList("application/x-scala")

    @BeanProperty val names           = asList("scala")

    def getMethodCallSyntax(obj: String, m: String, args: String*): String = args.mkString(s"$obj.$m(", ", ", ")")

    def getOutputStatement(toDisplay: String): String = s"Console.println($toDisplay)"

    def getParameter(key: String): Object = key match {
      case ScriptEngine.ENGINE           => engineName
      case ScriptEngine.ENGINE_VERSION   => engineVersion
      case ScriptEngine.LANGUAGE         => languageName
      case ScriptEngine.LANGUAGE_VERSION => languageVersion
      case ScriptEngine.NAME             => names.get(0)
      case _ => null
    }

    def getProgram(statements: String*): String = statements.mkString("object Main extends _root_.scala.App {\n\t", "\n\t", "\n}")

    def getScriptEngine: ScriptEngine = {
      val settings = new Settings()
      settings.usemanifestcp.value = true
      Scripted(this, settings)
    }
  }

  def apply(factory: ScriptEngineFactory = new Factory, settings: Settings = defaultSettings, out: JPrintWriter = defaultOut) = {
    settings.Yreplclassbased.value = true
    settings.usejavacp.value       = true
    val s = new Scripted(factory, settings, out)
    s.setBindings(s.createBindings, ScriptContext.ENGINE_SCOPE)
    s
  }
}

import java.io.Writer
import java.nio.{ ByteBuffer, CharBuffer }
import java.nio.charset.{ Charset, CodingErrorAction }
import CodingErrorAction.{ REPLACE => Replace }

/* An OutputStream that decodes bytes and flushes to the writer. */
class WriterOutputStream(writer: Writer) extends OutputStream {
  val decoder = Charset.defaultCharset.newDecoder
  decoder onMalformedInput Replace
  decoder onUnmappableCharacter Replace

  val byteBuffer = ByteBuffer.allocate(64)
  val charBuffer = CharBuffer.allocate(64)

  override def write(b: Int): Unit = {
    byteBuffer.put(b.toByte)
    byteBuffer.flip()
    val result = decoder.decode(byteBuffer, charBuffer, /*eoi=*/ false)
    if (byteBuffer.remaining == 0) byteBuffer.clear()
    if (charBuffer.position > 0) {
      charBuffer.flip()
      writer write charBuffer.toString
      charBuffer.clear()
    }
  }
  override def close(): Unit = {
    decoder.decode(byteBuffer, charBuffer, /*eoi=*/ true)
    decoder.flush(charBuffer)
  }
  override def toString = charBuffer.toString
}
