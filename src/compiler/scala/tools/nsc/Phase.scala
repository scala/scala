/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc;

import symtab.Flags;

object Phase {
  var phaseCount = 0;

}

abstract class Phase(val prev: Phase) {

  Phase.phaseCount = Phase.phaseCount + 1
  Console.println("creating phase: "+Phase.phaseCount)
  override def finalize() = {
    Phase.phaseCount = Phase.phaseCount - 1
    Console.println("collecting phase: "+Phase.phaseCount)
  }

  type Id = int;

  val id: Id = if (prev == null) 0 else prev.id + 1;

  def newFlags: long = 0l;
  private var fmask: long =
    if (prev == null) Flags.InitialFlags else prev.flagMask | newFlags;
  def flagMask: long = fmask;

  private var nx: Phase = this;
  if (prev != null) prev.nx = this;

  def next: Phase = nx;

  def name: String;
  def description: String = name;
  def erasedTypes: boolean = false;
  def flatClasses: boolean = false;
  def run: unit;

  override def toString() = name;
}


