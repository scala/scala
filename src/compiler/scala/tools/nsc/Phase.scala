/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import symtab.Flags
import util.TableDef

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
  if ((prev ne null) && (prev ne NoPhase)) prev.nx = this

  def next: Phase = nx

  def name: String
  def description: String = name
  // Will running with -Ycheck:name work?
  def checkable: Boolean = true
  // def devirtualized: Boolean = false
  def specialized: Boolean = false
  def erasedTypes: Boolean = false
  def flatClasses: Boolean = false
  def refChecked: Boolean = false
  def keepsTypeParams = true
  def run(): Unit

  override def toString() = name
  override def hashCode = id.## + name.##
  override def equals(other: Any) = other match {
    case x: Phase   => id == x.id && name == x.name
    case _          => false
  }
}

object Phase {
  val MaxPhases = 64

  /** A class for tracking something about each phase.
   */
  class Model[T: Manifest] {
    case class Cell(ph: Phase, value: T) {
      def name = ph.name
      def id = ph.id
    }
    val values                            = new Array[Cell](MaxPhases + 1)
    def results                           = values filterNot (_ == null)
    def apply(ph: Phase): T               = values(ph.id).value
    def update(ph: Phase, value: T): Unit = values(ph.id) = Cell(ph, value)
  }
  /** A class for recording the elapsed time of each phase in the
   *  interests of generating a classy and informative table.
   */
  class TimingModel extends Model[Long] {
    var total: Long = 0
    def table() = {
      total = results map (_.value) sum;
      new Format.Table(results sortBy (-_.value))
    }
    object Format extends TableDef[Cell] {
      >> ("phase"   -> (_.name)) >+ "  "
      << ("id"      -> (_.id))  >+ "  "
      >> ("ms"      -> (_.value)) >+ "  "
      << ("share"   -> (_.value.toDouble * 100 / total formatted "%.2f"))
    }
    def formatted = "" + table()
  }
}

