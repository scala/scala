/* NSC -- new scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab
import nsc.ast.Trees

import util._

abstract class SymbolTable extends Names
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
{
  def settings: Settings
  def rootLoader: LazyType
  def log(msg: AnyRef)

  /** Are we compiling for Java SE ? */
  def forJVM: Boolean

  /** Are we compiling for .NET ? */
  def forMSIL: Boolean

  protected def trackTypeIDE(sym : Symbol) : Boolean = true
  def compare(sym : Symbol, name : Name) = sym.name == name
  def verifyAndPrioritize[T](g : Symbol => Symbol)(pt : Type)(f : => T) = f
  def trackSetInfo[T <: Symbol](sym : T)(info : Type) : T = {
    sym.setInfo(info); sym
  }
  def notifyImport(what : Name, container : Type, from : Name, to : Name) : Unit = {}
  def sanitize(tree : Tree) : Tree = tree
  def attachSource(symbol : ClassSymbol, file : io.AbstractFile) : Unit = {
    assert(symbol != null)
  }
  def prepareReset(symbol : Symbol, tpe : LazyType) : Unit = {
    assert(symbol != null)
  }

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

  /** The run identifier of the given period */
  final def runId(period: Period): RunId = period >> 8

  /** The phase identifier of the given period */
  final def phaseId(period: Period): Phase#Id = period & 0xFF

  /** The period at the start of run that includes `period' */
  final def startRun(period: Period): Period = period & 0xFFFFFF00

  /** The current period */
  final def currentPeriod: Period = {
    //assert(per == (currentRunId << 8) + phase.id)
    per
  }

  /** The phase associated with given period */
  final def phaseOf(period: Period): Phase = phaseWithId(phaseId(period))

  final def period(rid: RunId, pid: Phase#Id): Period =
    (currentRunId << 8) + pid

  /** Perform given operation at given phase */
  final def atPhase[T](ph: Phase)(op: => T): T = {
    val current = phase
    try {
      phase = ph
      op
    } finally {
      phase = current
    }
  }

  /** Break into repl debugger if assertion is true */
  // def breakIf(assertion: => Boolean, args: Any*): Unit =
  //   if (assertion)
  //     Interpreter.break(args.toList)

  /** The set of all installed infotransformers */
  var infoTransformers = new InfoTransformer {
    val pid = NoPhase.id
    val changesBaseClasses = true
    def transform(sym: Symbol, tpe: Type): Type = tpe
  }

  /** The phase which has given index as identifier */
  val phaseWithId: Array[Phase]
}
