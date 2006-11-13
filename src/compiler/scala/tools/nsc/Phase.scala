/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import symtab.Flags

abstract class Phase(val prev: Phase) {

  type Id = int

  val id: Id = if (prev eq null) 0 else prev.id + 1

  def newFlags: long = 0l
  private var fmask: long =
    if (prev eq null) Flags.InitialFlags else prev.flagMask | newFlags
  def flagMask: long = fmask

  private var nx: Phase = this
  if (prev ne null) prev.nx = this

  def next: Phase = nx

  def name: String
  def description: String = name
  def erasedTypes: boolean = false
  def flatClasses: boolean = false
  def run: unit

  override def toString() = name
}


