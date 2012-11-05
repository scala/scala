/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

abstract class Phase(val prev: Phase) {
  if ((prev ne null) && (prev ne NoPhase))
    prev.nx = this

  type Id = Int
  val id: Id = if (prev eq null) 0 else prev.id + 1

  /** New flags visible after this phase has completed */
  def nextFlags: Long = 0l

  /** New flags visible once this phase has started */
  def newFlags: Long = 0l

  val fmask = (
    if (prev eq null) Flags.InitialFlags
    else prev.flagMask | prev.nextFlags | newFlags
  )
  def flagMask: Long = fmask

  private var nx: Phase = this

  def next: Phase = nx
  def hasNext = next != this
  def iterator = Iterator.iterate(this)(_.next) takeWhile (p => p.next != p)

  def name: String
  def description: String = name
  // Will running with -Ycheck:name work?
  def checkable: Boolean = true
  def specialized: Boolean = false
  def erasedTypes: Boolean = false
  def flatClasses: Boolean = false
  def refChecked: Boolean = false

  /** This is used only in unsafeTypeParams, and at this writing is
   *  overridden to false in parser, namer, typer, and erasure. (And NoPhase.)
   */
  def keepsTypeParams = true
  def run(): Unit

  override def toString() = name
  override def hashCode = id.## + name.##
  override def equals(other: Any) = other match {
    case x: Phase   => id == x.id && name == x.name
    case _          => false
  }
}

object NoPhase extends Phase(null) {
  def name = "<no phase>"
  override def keepsTypeParams = false
  def run() { throw new Error("NoPhase.run") }
}

object SomePhase extends Phase(NoPhase) {
  def name = "<some phase>"
  def run() { throw new Error("SomePhase.run") }
}
