/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import symtab.Flags
import util.TableDef

object Phases {
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

