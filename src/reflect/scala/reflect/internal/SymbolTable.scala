/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import scala.annotation.elidable
import scala.collection.{ mutable, immutable }
import util._

abstract class SymbolTable extends macros.Universe
                              with Collections
                              with Names
                              with Symbols
                              with Types
                              with Kinds
                              with ExistentialsAndSkolems
                              with FlagSets
                              with Scopes
                              with Mirrors
                              with Definitions
                              with Constants
                              with BaseTypeSeqs
                              with InfoTransformers
                              with transform.Transforms
                              with StdNames
                              with AnnotationInfos
                              with AnnotationCheckers
                              with Trees
                              with Printers
                              with Positions
                              with TypeDebugging
                              with Importers
                              with Required
                              with CapturedVariables
                              with StdAttachments
                              with StdCreators
                              with BuildUtils
{

  val gen = new TreeGen { val global: SymbolTable.this.type = SymbolTable.this }
  lazy val treeBuild = gen

  def log(msg: => AnyRef): Unit
  def warning(msg: String): Unit     = Console.err.println(msg)
  def globalError(msg: String): Unit = abort(msg)
  def abort(msg: String): Nothing    = throw new FatalError(supplementErrorMessage(msg))

  def shouldLogAtThisPhase = false

  @deprecated("Give us a reason", "2.10.0")
  def abort(): Nothing = abort("unknown error")

  /** Override with final implementation for inlining. */
  def debuglog(msg:  => String): Unit = if (settings.debug.value) log(msg)
  def debugwarn(msg: => String): Unit = if (settings.debug.value) Console.err.println(msg)
  def throwableAsString(t: Throwable): String = "" + t

  /** Prints a stack trace if -Ydebug or equivalent was given, otherwise does nothing. */
  def debugStack(t: Throwable): Unit  = debugwarn(throwableAsString(t))

  /** Overridden when we know more about what was happening during a failure. */
  def supplementErrorMessage(msg: String): String = msg

  private[scala] def printCaller[T](msg: String)(result: T) = {
    Console.err.println("%s: %s\nCalled from: %s".format(msg, result,
      (new Throwable).getStackTrace.drop(2).take(15).mkString("\n")))

    result
  }

  private[scala] def printResult[T](msg: String)(result: T) = {
    Console.err.println(msg + ": " + result)
    result
  }
  @inline
  final private[scala] def logResult[T](msg: => String)(result: T): T = {
    log(msg + ": " + result)
    result
  }
  @inline
  final private[scala] def logResultIf[T](msg: => String, cond: T => Boolean)(result: T): T = {
    if (cond(result))
      log(msg + ": " + result)

    result
  }

  // For too long have we suffered in order to sort NAMES.
  // I'm pretty sure there's a reasonable default for that.
  // Notice challenge created by Ordering's invariance.
  implicit def lowPriorityNameOrdering[T <: Names#Name]: Ordering[T] =
    SimpleNameOrdering.asInstanceOf[Ordering[T]]

  private object SimpleNameOrdering extends Ordering[Names#Name] {
    def compare(n1: Names#Name, n2: Names#Name) = (
      if (n1 eq n2) 0
      else n1.toString compareTo n2.toString
    )
  }

  /** Dump each symbol to stdout after shutdown.
   */
  final val traceSymbolActivity = sys.props contains "scalac.debug.syms"
  object traceSymbols extends {
    val global: SymbolTable.this.type = SymbolTable.this
  } with util.TraceSymbolActivity

  /** Check that the executing thread is the compiler thread. No-op here,
   *  overridden in interactive.Global. */
  @elidable(elidable.WARNING)
  def assertCorrectThread() {}

  /** Are we compiling for Java SE? */
  // def forJVM: Boolean

  /** Are we compiling for .NET? */
  def forMSIL: Boolean = false

  /** A last effort if symbol in a select <owner>.<name> is not found.
   *  This is overridden by the reflection compiler to make up a package
   *  when it makes sense (i.e. <owner> is a package and <name> is a term name).
   */
  def missingHook(owner: Symbol, name: Name): Symbol = NoSymbol

  /** Returns the mirror that loaded given symbol */
  def mirrorThatLoaded(sym: Symbol): Mirror

  /** A period is an ordinal number for a phase in a run.
   *  Phases in later runs have higher periods than phases in earlier runs.
   *  Later phases have higher periods than earlier phases in the same run.
   */
  type Period = Int
  final val NoPeriod = 0

  /** An ordinal number for compiler runs. First run has number 1. */
  type RunId = Int
  final val NoRunId = 0

  // sigh, this has to be public or atPhase doesn't inline.
  var phStack: List[Phase] = Nil
  private[this] var ph: Phase = NoPhase
  private[this] var per = NoPeriod

  final def atPhaseStack: List[Phase] = phStack
  final def phase: Phase = {
    if (Statistics.hotEnabled)
      Statistics.incCounter(SymbolTableStats.phaseCounter)
    ph
  }

  def atPhaseStackMessage = atPhaseStack match {
    case Nil    => ""
    case ps     => ps.reverseMap("->" + _).mkString("(", " ", ")")
  }

  final def phase_=(p: Phase) {
    //System.out.println("setting phase to " + p)
    assert((p ne null) && p != NoPhase, p)
    ph = p
    per = period(currentRunId, p.id)
  }
  final def pushPhase(ph: Phase): Phase = {
    val current = phase
    phase = ph
    phStack ::= ph
    current
  }
  final def popPhase(ph: Phase) {
    phStack = phStack.tail
    phase = ph
  }

  /** The current compiler run identifier. */
  def currentRunId: RunId

  /** The run identifier of the given period. */
  final def runId(period: Period): RunId = period >> 8

  /** The phase identifier of the given period. */
  final def phaseId(period: Period): Phase#Id = period & 0xFF

  /** The period at the start of run that includes `period`. */
  final def startRun(period: Period): Period = period & 0xFFFFFF00

  /** The current period. */
  final def currentPeriod: Period = {
    //assert(per == (currentRunId << 8) + phase.id)
    per
  }

  /** The phase associated with given period. */
  final def phaseOf(period: Period): Phase = phaseWithId(phaseId(period))

  final def period(rid: RunId, pid: Phase#Id): Period =
    (rid << 8) + pid

  /** Are we later than given phase in compilation? */
  final def isAtPhaseAfter(p: Phase) =
    p != NoPhase && phase.id > p.id

  /** Perform given operation at given phase. */
  @inline final def atPhase[T](ph: Phase)(op: => T): T = {
    val saved = pushPhase(ph)
    try op
    finally popPhase(saved)
  }


  /** Since when it is to be "at" a phase is inherently ambiguous,
   *  a couple unambiguously named methods.
   */
  @inline final def beforePhase[T](ph: Phase)(op: => T): T = atPhase(ph)(op)
  @inline final def afterPhase[T](ph: Phase)(op: => T): T  = atPhase(ph.next)(op)
  @inline final def afterCurrentPhase[T](op: => T): T      = atPhase(phase.next)(op)
  @inline final def beforePrevPhase[T](op: => T): T        = atPhase(phase.prev)(op)

  @inline final def atPhaseNotLaterThan[T](target: Phase)(op: => T): T =
    if (isAtPhaseAfter(target)) atPhase(target)(op) else op

  final def isValid(period: Period): Boolean =
    period != 0 && runId(period) == currentRunId && {
      val pid = phaseId(period)
      if (phase.id > pid) infoTransformers.nextFrom(pid).pid >= phase.id
      else infoTransformers.nextFrom(phase.id).pid >= pid
    }

  final def isValidForBaseClasses(period: Period): Boolean = {
    def noChangeInBaseClasses(it: InfoTransformer, limit: Phase#Id): Boolean = (
      it.pid >= limit ||
      !it.changesBaseClasses && noChangeInBaseClasses(it.next, limit)
    );
    period != 0 && runId(period) == currentRunId && {
      val pid = phaseId(period)
      if (phase.id > pid) noChangeInBaseClasses(infoTransformers.nextFrom(pid), phase.id)
      else noChangeInBaseClasses(infoTransformers.nextFrom(phase.id), pid)
    }
  }

  def openPackageModule(container: Symbol, dest: Symbol) {
    // unlink existing symbols in the package
    for (member <- container.info.decls.iterator) {
      if (!member.isPrivate && !member.isConstructor) {
        // todo: handle overlapping definitions in some way: mark as errors
        // or treat as abstractions. For now the symbol in the package module takes precedence.
        for (existing <- dest.info.decl(member.name).alternatives)
          dest.info.decls.unlink(existing)
      }
    }
    // enter non-private decls the class
    for (member <- container.info.decls.iterator) {
      if (!member.isPrivate && !member.isConstructor) {
        dest.info.decls.enter(member)
      }
    }
    // enter decls of parent classes
    for (p <- container.parentSymbols) {
      if (p != definitions.ObjectClass) {
        openPackageModule(p, dest)
      }
    }
  }

  /** Convert array parameters denoting a repeated parameter of a Java method
   *  to `JavaRepeatedParamClass` types.
   */
  def arrayToRepeated(tp: Type): Type = tp match {
    case MethodType(params, rtpe) =>
      val formals = tp.paramTypes
      assert(formals.last.typeSymbol == definitions.ArrayClass, formals)
      val method = params.last.owner
      val elemtp = formals.last.typeArgs.head match {
        case RefinedType(List(t1, t2), _) if (t1.typeSymbol.isAbstractType && t2.typeSymbol == definitions.ObjectClass) =>
          t1 // drop intersection with Object for abstract types in varargs. UnCurry can handle them.
        case t =>
          t
      }
      val newParams = method.newSyntheticValueParams(formals.init :+ definitions.javaRepeatedType(elemtp))
      MethodType(newParams, rtpe)
    case PolyType(tparams, rtpe) =>
      PolyType(tparams, arrayToRepeated(rtpe))
  }

  abstract class SymLoader extends LazyType {
    def fromSource = false
  }

  /** if there's a `package` member object in `pkgClass`, enter its members into it. */
  def openPackageModule(pkgClass: Symbol) {

    val pkgModule = pkgClass.info.decl(nme.PACKAGEkw)
    def fromSource = pkgModule.rawInfo match {
      case ltp: SymLoader => ltp.fromSource
      case _ => false
    }
    if (pkgModule.isModule && !fromSource) {
      // println("open "+pkgModule)//DEBUG
      openPackageModule(pkgModule, pkgClass)
    }
  }

  object perRunCaches {
    import scala.runtime.ScalaRunTime.stringOf
    import scala.collection.generic.Clearable

    // Weak references so the garbage collector will take care of
    // letting us know when a cache is really out of commission.
    private val caches = WeakHashSet[Clearable]()

    def recordCache[T <: Clearable](cache: T): T = {
      caches += cache
      cache
    }

    def clearAll() = {
      debuglog("Clearing " + caches.size + " caches.")
      caches foreach (_.clear)
    }

    def newWeakMap[K, V]()        = recordCache(mutable.WeakHashMap[K, V]())
    def newMap[K, V]()            = recordCache(mutable.HashMap[K, V]())
    def newSet[K]()               = recordCache(mutable.HashSet[K]())
    def newWeakSet[K <: AnyRef]() = recordCache(new WeakHashSet[K]())
  }

  /** The set of all installed infotransformers. */
  var infoTransformers = new InfoTransformer {
    val pid = NoPhase.id
    val changesBaseClasses = true
    def transform(sym: Symbol, tpe: Type): Type = tpe
  }

  /** The phase which has given index as identifier. */
  val phaseWithId: Array[Phase]

  /** Is this symbol table a part of a compiler universe?
   */
  def isCompilerUniverse = false

  /**
   * Adds the `sm` String interpolator to a [[scala.StringContext]].
   */
  implicit val StringContextStripMarginOps: StringContext => StringContextStripMarginOps = util.StringContextStripMarginOps

  def importPrivateWithinFromJavaFlags(sym: Symbol, jflags: Int): Symbol = {
    import ClassfileConstants._
    if ((jflags & (JAVA_ACC_PRIVATE | JAVA_ACC_PROTECTED | JAVA_ACC_PUBLIC)) == 0)
      // See ticket #1687 for an example of when topLevelClass is NoSymbol: it
      // apparently occurs when processing v45.3 bytecode.
      if (sym.enclosingTopLevelClass != NoSymbol)
        sym.privateWithin = sym.enclosingTopLevelClass.owner

    // protected in java means package protected. #3946
    if ((jflags & JAVA_ACC_PROTECTED) != 0)
      if (sym.enclosingTopLevelClass != NoSymbol)
        sym.privateWithin = sym.enclosingTopLevelClass.owner

    sym
  }
}

object SymbolTableStats {
  val phaseCounter = Statistics.newCounter("#phase calls")
}
