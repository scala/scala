/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc;

abstract class Phase(val prev: Phase) {
  val id: int = if (prev == null) 0 else prev.id + 1;

  private var nx: Phase = this;
  if (prev != null) prev.nx = this;

  def next: Phase = nx;

  def name: String;
  def description: String = name;
  def erasedTypes: boolean = false;

  def run: unit;

  override def toString() = name;
}


