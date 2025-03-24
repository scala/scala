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

package scala
package reflect
package internal

import java.net.URLClassLoader

import scala.annotation.{elidable, nowarn, tailrec}
import scala.collection.mutable
import util._
import java.util.concurrent.TimeUnit

import scala.reflect.internal.settings.MutableSettings
import scala.reflect.internal.{TreeGen => InternalTreeGen}
import scala.reflect.io.AbstractFile

abstract class SymbolTable extends macros.Universe
                              with Collections
                              with Names
                              with Symbols
                              with Types
                              with Variances
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
                              with CapturedVariables
                              with StdAttachments
                              with StdCreators
                              with ReificationSupport
                              with PrivateWithin
                              with pickling.Translations
                              with FreshNames
                              with Internals
                              with Reporting
{

  val gen = new InternalTreeGen { val global: SymbolTable.this.type = SymbolTable.this }

  trait ReflectStats extends BaseTypeSeqsStats
                        with TypesStats
                        with SymbolTableStats
                        with TreesStats
                        with SymbolsStats
                        with ScopeStats { self: Statistics => }

  /** Some statistics (normally disabled) set with -Ystatistics */
  val statistics: Statistics with ReflectStats

  def log(msg: => AnyRef): Unit

  protected def elapsedMessage(msg: String, startNs: Long) =
    msg + " in " + (TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startNs)) + "ms"

  def informProgress(msg: String)            = if (settings.verbose.value) inform("[" + msg + "]")
  def informTime(msg: String, startNs: Long) = if (settings.verbose.value) informProgress(elapsedMessage(msg, startNs))
  @inline final def informingProgress[T](msg: => String)(fn: => T) : T = {
    val verbose: Boolean = settings.verbose.value
    val start = if (verbose) System.nanoTime() else 0L
    try fn finally if (verbose) informTime(msg, start)
  }

  def shouldLogAtThisPhase = false
  def isPastTyper = false

  @inline final def isDeveloper: Boolean = settings.isDebug || settings.isDeveloper

  def picklerPhase: Phase
  def erasurePhase: Phase

  def settings: MutableSettings

  def isSymbolLockTracingEnabled: Boolean = isDeveloper

  /** Override with final implementation for inlining. */
  def debuglog(msg:  => String): Unit = if (settings.isDebug) log(msg)

  /** dev-warns if dev-warning is enabled and `cond` is true; no-op otherwise */
  @inline final def devWarningIf(cond: => Boolean)(msg: => String): Unit =
    if (isDeveloper && cond) devWarning(msg)
  def devWarning(msg: => String): Unit = if (isDeveloper) Console.err.println(msg)
  def throwableAsString(t: Throwable): String = "" + t
  def throwableAsString(t: Throwable, maxFrames: Int): String = t.getStackTrace take maxFrames mkString "\n  at "

  @inline final def devWarningDumpStack(msg: => String, maxFrames: Int): Unit =
    devWarning(msg + "\n" + throwableAsString(new Throwable, maxFrames))

  /** Prints a stack trace if -Ydebug or equivalent was given, otherwise does nothing. */
  def debugStack(t: Throwable): Unit  = devWarning(throwableAsString(t))

  private[scala] def printCaller[T](msg: String)(result: T) = {
    Console.err.println("%s: %s\nCalled from: %s".format(msg, result,
      (new Throwable).getStackTrace.drop(2).take(50).mkString("\n")))

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
  final private[scala] def debuglogResult[T](msg: => String)(result: T): T = {
    debuglog(msg + ": " + result)
    result
  }
  @inline
  final private[scala] def devWarningResult[T](msg: => String)(result: T): T = {
    devWarning(msg + ": " + result)
    result
  }
  @inline
  final private[scala] def logResultIf[T](msg: => String, cond: T => Boolean)(result: T): T = {
    if (cond(result))
      log(msg + ": " + result)

    result
  }
  @inline
  final private[scala] def debuglogResultIf[T](msg: => String, cond: T => Boolean)(result: T): T = {
    if (cond(result))
      debuglog(msg + ": " + result)

    result
  }

  @inline
  final def assert(assertion: Boolean, message: => Any): Unit =
    if (!assertion) throwAssertionError(message)

  @deprecated("consider supplying an explanatory message", since = "2.12.5")
  final def assert(assertion: Boolean): Unit = assert(assertion, "")

  @inline
  final def require(requirement: Boolean, message: => Any): Unit =
    if (!requirement) throwRequirementError(message)

  @deprecated("consider supplying an explanatory message", since = "2.12.5")
  final def require(requirement: Boolean): Unit = require(requirement, "")

  // extracted from `assert`/`require` to make them as small (and inlineable) as possible
  private[internal] def throwAssertionError(msg: Any): Nothing =
    throw new java.lang.AssertionError(s"assertion failed: ${supplementErrorMessage(String valueOf msg)}")
  private[internal] def throwRequirementError(msg: Any): Nothing =
    throw new java.lang.IllegalArgumentException(s"requirement failed: ${supplementErrorMessage(String valueOf msg)}")

  @inline final def findSymbol(xs: IterableOnce[Symbol])(p: Symbol => Boolean): Symbol = {
    xs.iterator find p getOrElse NoSymbol
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
  final val traceSymbolActivity = System.getProperty("scalac.debug.syms") != null
  object traceSymbols extends {
    val global: SymbolTable.this.type = SymbolTable.this
  } with util.TraceSymbolActivity

  val treeInfo: TreeInfo { val global: SymbolTable.this.type }

  /** Check that the executing thread is the compiler thread. No-op here,
   *  overridden in interactive.Global. */
  @elidable(elidable.WARNING)
  def assertCorrectThread(): Unit = {}

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

  private[this] val phStack: collection.mutable.Stack[Phase] = new collection.mutable.Stack()
  private[this] var ph: Phase = NoPhase
  private[this] var per = NoPeriod

  final def atPhaseStack: List[Phase] = phStack.toList
  final def phase: Phase = {
    ph
  }

  def atPhaseStackMessage = atPhaseStack match {
    case Nil    => ""
    case ps     => ps.reverseIterator.map("->" + _).mkString("(", " ", ")")
  }

  final def phase_=(p: Phase): Unit = {
    ph = p
    per = period(currentRunId, p.id)
  }
  final def pushPhase(ph: Phase): Phase = {
    val current = phase
    phase = ph
    if (keepPhaseStack) {
      phStack.push(ph)
    }
    current
  }
  final def popPhase(ph: Phase): Unit = {
    if (keepPhaseStack) {
      phStack.pop()
    }
    phase = ph
  }
  var keepPhaseStack: Boolean = false

  /** The current compiler run identifier. */
  def currentRunId: RunId

  /** The run identifier of the given period. */
  final def runId(period: Period): RunId = period >> 8

  /** The phase identifier of the given period. */
  final def phaseId(period: Period): Phase#Id = period & 0xFF

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
  @inline final def enteringPhase[T](ph: Phase)(op: => T): T = {
    if (ph eq phase) op // opt
    else {
      val saved = pushPhase(ph)
      try op
      finally popPhase(saved)
    }
  }

  final def findPhaseWithName(phaseName: String): Phase = {
    var ph = phase
    while (ph != NoPhase && ph.name != phaseName) {
      ph = ph.prev
    }
    if (ph eq NoPhase) phase else ph
  }
  final def enteringPhaseWithName[T](phaseName: String)(body: => T): T = {
    val phase = findPhaseWithName(phaseName)
    enteringPhase(phase)(body)
  }

  def slowButSafeEnteringPhase[T](ph: Phase)(op: => T): T = {
    if (isCompilerUniverse) enteringPhase(ph)(op)
    else op
  }

  @inline final def exitingPhase[T](ph: Phase)(op: => T): T = enteringPhase(ph.next)(op)
  @inline final def enteringPrevPhase[T](op: => T): T       = enteringPhase(phase.prev)(op)

  @inline final def enteringPhaseNotLaterThan[T](target: Phase)(op: => T): T =
    if (isAtPhaseAfter(target)) enteringPhase(target)(op) else op

  def slowButSafeEnteringPhaseNotLaterThan[T](target: Phase)(op: => T): T =
    if (isCompilerUniverse) enteringPhaseNotLaterThan(target)(op) else op

  final def isValid(period: Period): Boolean =
    period != 0 && runId(period) == currentRunId && {
      val pid = phaseId(period)
      if (phase.id > pid) nextFrom(pid).pid >= phase.id
      else nextFrom(phase.id).pid >= pid
    }

  final def isValidForBaseClasses(period: Period): Boolean = {
    @tailrec
    def noChangeInBaseClasses(it: InfoTransformer, limit: Phase#Id): Boolean = (
      it.pid >= limit ||
      !it.changesBaseClasses && noChangeInBaseClasses(it.next, limit)
    )
    period != 0 && runId(period) == currentRunId && {
      val pid = phaseId(period)
      if (phase.id > pid) noChangeInBaseClasses(nextFrom(pid), phase.id)
      else noChangeInBaseClasses(nextFrom(phase.id), pid)
    }
  }

  def openPackageModule(container: Symbol, dest: Symbol): Unit = {
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
    for (p <- container.parentSymbolsIterator) {
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
    case PolyType(tparams, rtpe) => PolyType(tparams, arrayToRepeated(rtpe))
    case x                       => throw new MatchError(x)
  }

  abstract class SymLoader extends LazyType {
    def fromSource = false
  }

  /** if there's a `package` member object in `pkgClass`, enter its members into it. */
  def openPackageModule(pkgClass: Symbol, force: Boolean = false): Unit = {
    val pkgModule  = pkgClass.packageObject
    def fromSource = pkgModule.rawInfo match {
      case ltp: SymLoader => ltp.fromSource
      case _ => false
    }
    if (pkgModule.isModule && !fromSource)
      openPackageModule(pkgModule, pkgClass)
  }

  object perRunCaches {
    import scala.collection.mutable.Clearable

    // Weak references so the garbage collector will take care of
    // letting us know when a cache is really out of commission.
    import java.lang.ref.WeakReference
    private[this] var caches = List[WeakReference[Clearable]]()
    private[this] var javaCaches = List[JavaClearable[_]]()

    def recordCache[T <: Clearable](cache: T): cache.type = {
      cache match {
        case jc: JavaClearable[_] =>
          javaCaches ::= jc
        case _ =>
          caches ::= new WeakReference(cache)
      }
      cache
    }

    /** Closes the provided classloader at the conclusion of this Run */
    final def recordClassloader(loader: ClassLoader): loader.type = {
      def attemptClose(loader: ClassLoader): Unit = {
        loader match {
          case u: URLClassLoader => debuglog("Closing classloader " + u); u.close()
          case _ =>
        }
      }
      caches ::= new WeakReference((new Clearable {
        def clear(): Unit = {
          attemptClose(loader)
        }
      }))
      loader
    }

    /**
     * Removes a cache from the per-run caches. This is useful for testing: it allows running the
     * compiler and then inspect the state of a cache.
     */
    def unrecordCache[T <: Clearable](cache: T): Unit = {
      cache match {
        case jc: JavaClearable[_] =>
          javaCaches = javaCaches.filterNot(cache == _)
        case _ =>
          caches = caches.filterNot(_.get eq cache)
      }
    }

    def clearAll(): Unit = {
      debuglog("Clearing " + (caches.size + javaCaches.size) + " caches.")
      caches foreach (ref => Option(ref.get).foreach(_.clear()))
      caches = caches.filterNot(_.get == null)

      javaCaches foreach (_.clear())
      javaCaches = javaCaches.filter(_.isValid)
    }

    def newWeakMap[K, V]()        = recordCache(mutable.WeakHashMap.empty[K, V])
    def newMap[K, V]()            = recordCache(mutable.HashMap.empty[K, V])
    def newSet[K]()               = recordCache(mutable.HashSet.empty[K])
    def newWeakSet[K <: AnyRef]() = recordCache(WeakHashSet.empty[K])

    @nowarn("cat=deprecation")
    def newAnyRefMap[K <: AnyRef, V]() = recordCache(mutable.AnyRefMap.empty[K, V])
    @nowarn("cat=deprecation")
    def newAnyRefMap[K <: AnyRef, V](default: K => V) = recordCache(mutable.AnyRefMap.withDefault[K, V](default))

    /**
      * Register a cache specified by a factory function and (optionally) a cleanup function.
      *
      * @return A function that will return cached value, or create a fresh value when a new run is started.
      */
    def newGeneric[T](f: => T, cleanup: T => Unit = (x: Any) => ()): () => T = {
      val NoCached: T = null.asInstanceOf[T]
      var cached: T = NoCached
      var cachedRunId = NoRunId
      val clearable = new Clearable with (() => T)  {
        def clear(): Unit = {
          if (cached != NoCached)
            cleanup(cached)
          cached = NoCached
        }
        def apply(): T = {
          if (currentRunId != cachedRunId || cached == NoCached) {
            cached = f
            cachedRunId = currentRunId
          }
          cached
        }
      }
      recordCache(clearable)
    }
  }

  /** The set of all installed infotransformers. */
  var infoTransformers = new InfoTransformer {
    val pid = NoPhase.id
    val changesBaseClasses = true
    def transform(sym: Symbol, tpe: Type): Type = tpe
  }
  var nextFrom: Array[InfoTransformer] = null

  private final val MaxPhases = 256
  /** The phase which has given index as identifier. */
  final val phaseWithId: Array[Phase] = Array.fill(MaxPhases)(NoPhase)

  /** Is this symbol table a part of a compiler universe?
   */
  def isCompilerUniverse = false

  @deprecated("use enteringPhase", "2.10.0") // Used in sbt 0.12.4
  @inline final def atPhase[T](ph: Phase)(op: => T): T = enteringPhase(ph)(op)

  protected[scala] def currentRunProfilerBeforeCompletion(root: Symbol, associatedFile: AbstractFile): Unit = ()
  protected[scala] def currentRunProfilerAfterCompletion(root: Symbol, associatedFile: AbstractFile): Unit = ()
}

trait SymbolTableStats {
  self: TypesStats with Statistics =>

  // Defined here because `SymbolLoaders` is defined in `scala.tools.nsc`
  // and only has access to the `statistics` definition from `scala.reflect`.
  val classReadNanos = newSubTimer("time classfilereading", typerNanos)
}
