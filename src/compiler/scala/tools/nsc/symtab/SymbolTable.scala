/* NSC -- new scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.symtab;

import util._;

abstract class SymbolTable extends Names
                              with Symbols
                              with Types
                              with Scopes
                              with Definitions
                              with Constants
                              with InfoTransformers
                              with StdNames
{
  def settings: Settings;
  def rootLoader: LazyType;
  def log(msg: Object): unit;

  def forCLDC: Boolean;

  private var ph: Phase = NoPhase;
  def phase: Phase = ph;
  def phase_=(p: Phase): unit = {
    //System.out.println("setting phase to " + p);
    assert(p != null && p != NoPhase);
    ph = p
  }

  final val NoRun = null;

  /** The current compiler run. */
  def currentRun: CompilerRun;

  def atPhase[T](ph: Phase)(op: => T): T = {
    val current = phase;
    phase = ph;
    val result = op;
    phase = current;
    result
  }

  var infoTransformers = new InfoTransformer {
    val pid = NoPhase.id;
    val changesBaseClasses = true;
    def transform(sym: Symbol, tpe: Type): Type = tpe;
  }

  val phaseWithId: Array[Phase];
}
