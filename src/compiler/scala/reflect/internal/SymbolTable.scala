/* NSC -- new scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import scala.collection.{ mutable, immutable }
import util._

abstract class SymbolTable extends api.Universe
                              with Names
                              with Symbols
                              with Types
                              with Scopes
                              with Definitions
                              with Constants
                              with BaseTypeSeqs
                              with InfoTransformers
                              with StdNames
                              with AnnotationInfos
                              with AnnotationCheckers
                              with Trees
                              with TreePrinters
                              with Positions
                              with TypeDebugging
                              with Required
{
  def rootLoader: LazyType
  def log(msg: => AnyRef): Unit
  def abort(msg: String): Nothing = throw new Error(msg)
  def abort(): Nothing = throw new Error()

  /** Are we compiling for Java SE? */
  // def forJVM: Boolean

  /** Are we compiling for .NET? */
  def forMSIL: Boolean = false

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
  protected var per = NoPeriod

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
  final def atPhase[T](ph: Phase)(op: => T): T = {
    val current = phase
    phase = ph
    try op
    finally phase = current
  }
  final def afterPhase[T](ph: Phase)(op: => T): T =
    atPhase(ph.next)(op)

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

  object perRunCaches {
    import java.lang.ref.WeakReference
    import scala.tools.util.Signallable
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
    if (settings.debug.value) {
      println(Signallable("dump compiler caches")(dumpCaches()))
    }

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
