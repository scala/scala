/* NSC -- new Scala compiler
 * Copyright 2005-2015 LAMP/EPFL
 */
package scala
package tools.nsc
package interpreter

import scala.language.dynamics

import scala.beans.BeanProperty
import scala.collection.JavaConverters._
import scala.reflect.classTag
import scala.tools.nsc.util.stringFromReader
import javax.script._, ScriptContext.{ ENGINE_SCOPE, GLOBAL_SCOPE }
import java.io.{ Reader, StringWriter }

/* A REPL adaptor for the javax.script API. */
class Scripted(@BeanProperty val factory: ScriptEngineFactory, settings: Settings, out: JPrintWriter)
  extends AbstractScriptEngine with Compilable {

  def createBindings: Bindings = new SimpleBindings

  // Bridge dynamic references and script context
  class DynamicContext extends Dynamic {

    var context: ScriptContext = Scripted.this.getContext

    // $ctx.x retrieves the attribute x
    def selectDynamic(field: String): Object = context.getAttribute(field)

    // $ctx.x = v
    def updateDynamic(field: String)(value: Object) = context.setAttribute(field, value, ENGINE_SCOPE)

    // set the context for dynamic resolution and run the body
    def apply[A](context: ScriptContext)(body: => A): A = {
      val saved = this.context
      this.context = context
      try body
      finally this.context = saved
    }
  }

  // Defines attributes available both for compilation and evaluation
  val dynamicContext = new DynamicContext

  // the underlying interpreter
  val intp = new IMain(settings, out) {
    import global.{ Name, TermName }

    // dynamic context bound under this name
    final val ctx = "$ctx"

    /* Modify the template to snag definitions from dynamic context.
     * So object $iw { x + 42 } becomes object $iw { def x = $ctx.x ; x + 42 }
     */
    override protected def importsCode(wanted: Set[Name], wrapper: Request#Wrapper, definesClass: Boolean, generousImports: Boolean) = {

      // cull references that can be satisfied from the current dynamic context
      val contextual = wanted & contextNames

      if (contextual.nonEmpty) {
        val neededContext = (wanted &~ contextual) + TermName(ctx)
        val ComputedImports(header, preamble, trailer, path) = super.importsCode(neededContext, wrapper, definesClass, generousImports)
        val adjusted = contextual.map(n =>
            s"""def ${n.decodedName} = $ctx.${n.decodedName}
                def ${n.decodedName}_=(x: Object) = $ctx.${n.decodedName} = x"""
          ).mkString(preamble, "\n", "\n")
        ComputedImports(header, adjusted, trailer, path)
      }
      else super.importsCode(wanted, wrapper, definesClass, generousImports)
    }

    // names available in current dynamic context
    def contextNames: Set[Name] = (
      for {
        scope   <- dynamicContext.context.getScopes.asScala
        binding <- Option(dynamicContext.context.getBindings(scope)) map (_.asScala) getOrElse Nil
        k = binding._1
      } yield (TermName(k) : Name)
    ).to[Set]
  }

  private var bound = false
  private var code = ""

  /* All scripts are compiled. The supplied context defines what references
   * not in REPL history are allowed, though a different context may be
   * supplied for evaluation of a compiled script.
   */
  def compile(script: String, context: ScriptContext): CompiledScript = {
    // TODO a mechanism for post-init actions
    if (!bound) {
      //intp quietBind ("engine" -> this.asInstanceOf[ScriptEngine])
      //intp quietBind NamedParam[DynamicContext]("$ctx", dynamicContext)
      intp quietBind NamedParamClass(intp.ctx, "scala.tools.nsc.interpreter.Scripted#DynamicContext", dynamicContext)
      intp quietBind NamedParam[IMain]("$intp", intp)(StdReplTags.tagOfIMain, classTag[IMain])
      bound = true
    }
    dynamicContext(context) {
      val cat = code + script
      intp.compile(cat, false) match {
        case Left(result) => result match {
          case IR.Incomplete =>
            code = cat + "\n"
            new CompiledScript {
              def eval(context: ScriptContext): Object = null
              def getEngine: ScriptEngine = Scripted.this
            }
          case _ =>
            code = ""
            throw new ScriptException("compile-time error")
        }
        case Right(req)   =>
          code = ""
          new WrappedRequest(req)
      }
    }
  }

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

    // TODO SI-8422 support context reader, writer and error writer
    private def evalEither(r: intp.Request, ctx: ScriptContext) = {
      def ctxPrintStream = Console.out
      if (ctx.getWriter != null) {
        Console.withOut(ctxPrintStream)(r.lineRep.evalEither)
      } else {
        r.lineRep.evalEither
      }
    }

    /* First time, cause lazy evaluation of a memoized result.
     * Subsequently, instantiate a new object for evaluation.
     * Per the API: Checked exception types thrown by underlying scripting implementations
     * must be wrapped in instances of ScriptException.
     */
    @throws[ScriptException]
    override def eval(context: ScriptContext) = dynamicContext(context) {
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

    def getProgram(statements: String*): String = statements.mkString("object Main extends App {\n\t", "\n\t", "\n}")

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
