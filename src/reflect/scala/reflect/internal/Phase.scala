/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package reflect
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

  private var nx: Phase = NoPhase

  // does anyone rely on next == this for terminus?
  def next: Phase = if (nx eq NoPhase) this else nx
  def hasNext = next != this
  // this definition excludes the terminal phase
  //def iterator = Iterator.iterate(this)(_.nx) takeWhile (p => p.next != p)
  def iterator = Iterator.iterate(this)(_.nx) takeWhile (_ ne NoPhase)

  def name: String
  def description: String = name
  // Will running with -Ycheck:name work?
  def checkable: Boolean = true

  // NOTE: sbt injects its own phases which extend this class, and not GlobalPhase, so we must implement this logic here
  private val _erasedTypes = ((prev ne null) && (prev ne NoPhase)) && (prev.name == "erasure" || prev.erasedTypes)
  def erasedTypes: Boolean = _erasedTypes // overridden in back-end
  final val flatClasses: Boolean   = ((prev ne null) && (prev ne NoPhase)) && (prev.name == "flatten"    || prev.flatClasses)
  final val specialized: Boolean   = ((prev ne null) && (prev ne NoPhase)) && (prev.name == "specialize" || prev.specialized)
  final val refChecked: Boolean    = ((prev ne null) && (prev ne NoPhase)) && (prev.name == "refchecks"  || prev.refChecked)

  // are we past the fields phase, so that:
  //   - we should allow writing to vals (as part of type checking trait setters)
  //   - modules have module accessors
  final val assignsFields: Boolean = ((prev ne null) && (prev ne NoPhase)) && (prev.name == "fields"     || prev.assignsFields)

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
