/* NSC -- new scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import scala.collection.{ mutable, immutable }
import util._

abstract class SymbolTable extends api.Universe
                              with Collections
                              with Names
                              with Symbols
                              with Types
                              with Kinds
                              with Scopes
                              with Definitions
                              with Constants
                              with BaseTypeSeqs
                              with InfoTransformers
                              with transform.Transforms
                              with StdNames
                              with AnnotationInfos
                              with AnnotationCheckers
                              with Trees
                              with TreePrinters
                              with Positions
                              with TypeDebugging
                              with Importers
                              with Required
{
  def rootLoader: LazyType
  def log(msg: => AnyRef): Unit
  def abort(msg: String): Nothing = throw new FatalError(msg)
  def abort(): Nothing = abort("unknown error")

  /** Override with final implementation for inlining. */
  def debuglog(msg:  => String): Unit = if (settings.debug.value) log(msg)
  def debugwarn(msg: => String): Unit = if (settings.debug.value) Console.err.println(msg)

  /** Are we compiling for Java SE? */
  // def forJVM: Boolean

  /** Are we compiling for .NET? */
  def forMSIL: Boolean = false

  /** A last effort if symbol in a select <owner>.<name> is not found.
   *  This is overridden by the reflection compiler to make up a package
   *  when it makes sense (i.e. <owner> is a package and <name> is a term name).
   */
  def missingHook(owner: Symbol, name: Name): Symbol = NoSymbol

  /** A period is an ordinal number for a phase in a run.
   *  Phases in later runs have higher periods than phases in earlier runs.
   *  Later phases have higher periods than earlier phases in the same run.
   */
  type Period = Int
  final val NoPeriod = 0

  /** An ordinal number for compiler runs. First run has number 1. */
  type RunId = Int
  final val NoRunId = 0

  private var ph: Phase = NoPhase
  private var per = NoPeriod

  final def phase: Phase = ph

  final def phase_=(p: Phase) {
    //System.out.println("setting phase to " + p)
    assert((p ne null) && p != NoPhase)
    ph = p
    per = (currentRunId << 8) + p.id
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
    (currentRunId << 8) + pid

  /** Perform given operation at given phase. */
  @inline final def atPhase[T](ph: Phase)(op: => T): T = {
    val current = phase
    phase = ph
    try op
    finally phase = current
  }

  @inline final def afterPhase[T](ph: Phase)(op: => T): T =
    atPhase(ph.next)(op)

  @inline final def atPhaseNotLaterThan[T](target: Phase)(op: => T): T =
    if (target != NoPhase && phase.id > target.id) atPhase(target)(op) else op

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
    for (pt <- container.info.parents; p = pt.typeSymbol) {
      if (p != definitions.ObjectClass && p != definitions.ScalaObjectClass) {
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
      assert(formals.last.typeSymbol == definitions.ArrayClass)
      val method = params.last.owner
      val elemtp = formals.last.typeArgs.head match {
        case RefinedType(List(t1, t2), _) if (t1.typeSymbol.isAbstractType && t2.typeSymbol == definitions.ObjectClass) =>
          t1 // drop intersection with Object for abstract types in varargs. UnCurry can handle them.
        case t =>
          t
      }
      val newParams = method.newSyntheticValueParams(
        formals.init :+ appliedType(definitions.JavaRepeatedParamClass.typeConstructor, List(elemtp)))
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
    import java.lang.ref.WeakReference
    import scala.runtime.ScalaRunTime.stringOf

    // We can allow ourselves a structural type, these methods
    // amount to a few calls per run at most.  This does suggest
    // a "Clearable" trait may be useful.
    private type Clearable = {
      def size: Int
      def clear(): Unit
    }
    // Weak references so the garbage collector will take care of
    // letting us know when a cache is really out of commission.
    private val caches = mutable.HashSet[WeakReference[Clearable]]()

    private def dumpCaches() {
      println(caches.size + " structures are in perRunCaches.")
      caches.zipWithIndex foreach { case (ref, index) =>
        val cache = ref.get()
        println("(" + index + ")" + (
          if (cache == null) " has been collected."
          else " has " + cache.size + " entries:\n" + stringOf(cache)
        ))
      }
    }
    // if (settings.debug.value) {
    //   println(Signallable("dump compiler caches")(dumpCaches()))
    // }

    def recordCache[T <: Clearable](cache: T): T = {
      caches += new WeakReference(cache)
      cache
    }

    def clearAll() = {
      if (settings.debug.value) {
        val size = caches flatMap (ref => Option(ref.get)) map (_.size) sum;
        log("Clearing " + caches.size + " caches totalling " + size + " entries.")
      }
      caches foreach { ref =>
        val cache = ref.get()
        if (cache == null)
          caches -= ref
        else
          cache.clear()
      }
    }

    def newWeakMap[K, V]() = recordCache(mutable.WeakHashMap[K, V]())
    def newMap[K, V]()     = recordCache(mutable.HashMap[K, V]())
    def newSet[K]()        = recordCache(mutable.HashSet[K]())
  }

  /** Break into repl debugger if assertion is true. */
  // def breakIf(assertion: => Boolean, args: Any*): Unit =
  //   if (assertion)
  //     ILoop.break(args.toList)

  /** The set of all installed infotransformers. */
  var infoTransformers = new InfoTransformer {
    val pid = NoPhase.id
    val changesBaseClasses = true
    def transform(sym: Symbol, tpe: Type): Type = tpe
  }

  /** The phase which has given index as identifier. */
  val phaseWithId: Array[Phase]
}
