package scala.tools.nsc.symtab;

import util._;

abstract class SymbolTable extends Names
			      with Symbols
			      with Types
                              with Scopes
                              with Definitions
			      with InfoTransformers
                              with StdNames {
  def settings: Settings;
  def rootLoader: LazyType;

  private var ph: Phase = NoPhase;
  def phase: Phase = ph;
  def phase_=(p: Phase): unit = {
    //System.out.println("setting phase to " + p);
    assert(p != null);
    ph = p
  }

  def atPhase[T](ph: Phase)(def op: T): T = {
    val current = phase;
    phase = ph;
    val result = op;
    phase = current;
    result
  }

  var infoTransformers = new InfoTransformer {
    val phase = NoPhase;
    def transform(sym: Symbol, tpe: Type): Type = tpe;
  }
}
