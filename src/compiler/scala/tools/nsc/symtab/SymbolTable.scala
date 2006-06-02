/* NSC -- new scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.symtab

import util._

abstract class SymbolTable extends Names
                              with Symbols
                              with Types
                              with Scopes
                              with Definitions
                              with Constants
                              with InfoTransformers
                              with StdNames
{
  def settings: Settings
  def rootLoader: LazyType
  def log(msg: Object): unit

  /** Are we compiling for the J2ME CLDC platform? */
  def forCLDC: Boolean

  private var ph: Phase = NoPhase
  private var period = 0

  def phase: Phase = ph

  def phase_=(p: Phase): unit = {
    //System.out.println("setting phase to " + p)
    assert(p != null && p != NoPhase)
    ph = p
    period = (currentRunId << 8) + p.id
  }

  /** An ordinal number for compiler runs. First run has number 1. */
  type RunId = int

  val NoRunId = 0

  /** The current compiler run identifier. */
  def currentRunId: RunId

  /** A period is an ordinal number for a phase in a run.
   *  Phases in later runs have higher periods than phases in earlier runs.
   *  Later phases have higher periods than earlier phases in the same run.
   */
  type Period = int
  val NoPeriod = -1

  /** The run identifier of the given period */
  def runId(period: Period): RunId = period >> 8

  /** The phase identifier of the given period */
  def phaseId(period: Period): Phase#Id = period & 0xFF

  /** The current period */
  def currentPeriod: Period = {
    //assert(period == (currentRunId << 8) + phase.id)
    period
  }

  /** Perform given operation at given phase */
  def atPhase[T](ph: Phase)(op: => T): T = {
    val current = phase
    phase = ph
    val result = op
    phase = current
    result
  }

  /** The set of all installed infotransformers */
  var infoTransformers = new InfoTransformer {
    val pid = NoPhase.id
    val changesBaseClasses = true
    def transform(sym: Symbol, tpe: Type): Type = tpe
  }

  /** The phase which has given index as identifier */
  val phaseWithId: Array[Phase]
}
