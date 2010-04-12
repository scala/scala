/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import symtab.Flags

abstract class Phase(val prev: Phase) {

  type Id = Int

  val id: Id = if (prev eq null) 0 else prev.id + 1

  /** New flags visible after this phase has completed */
  def nextFlags: Long = 0l

  /** New flags visible once this phase has started */
  def newFlags: Long = 0l

  private var fmask: Long =
    if (prev eq null) Flags.InitialFlags else prev.flagMask | prev.nextFlags | newFlags
  def flagMask: Long = fmask

  private var nx: Phase = this
  if (prev ne null) prev.nx = this

  def next: Phase = nx

  def name: String
  def description: String = name
  // Will running with -Ycheck:name work?
  def checkable: Boolean = true
  def devirtualized: Boolean = false
  def erasedTypes: Boolean = false
  def flatClasses: Boolean = false
  def keepsTypeParams = true
  def run: Unit

  override def toString() = name
}


