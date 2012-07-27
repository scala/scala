/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.swing

import java.awt.{GridBagConstraints, GridBagLayout}

object GridBagPanel {
  object Fill extends Enumeration {
    val None = Value(GridBagConstraints.NONE)
    val Horizontal = Value(GridBagConstraints.HORIZONTAL)
    val Vertical = Value(GridBagConstraints.VERTICAL)
    val Both = Value(GridBagConstraints.BOTH)
  }
  object Anchor extends Enumeration {
    val North = Value(GridBagConstraints.NORTH)
    val NorthEast = Value(GridBagConstraints.NORTHEAST)
    val East = Value(GridBagConstraints.EAST)
    val SouthEast = Value(GridBagConstraints.SOUTHEAST)
    val South = Value(GridBagConstraints.SOUTH)
    val SouthWest = Value(GridBagConstraints.SOUTHWEST)
    val West = Value(GridBagConstraints.WEST)
    val NorthWest = Value(GridBagConstraints.NORTHWEST)
    val Center = Value(GridBagConstraints.CENTER)

    val PageStart = Value(GridBagConstraints.PAGE_START)
    val PageEnd = Value(GridBagConstraints.PAGE_END)
    val LineStart = Value(GridBagConstraints.LINE_START)
    val LineEnd = Value(GridBagConstraints.LINE_END)
    val FirstLineStart = Value(GridBagConstraints.FIRST_LINE_START)
    val FirstLineEnd = Value(GridBagConstraints.FIRST_LINE_END)
    val LastLineStart = Value(GridBagConstraints.LAST_LINE_START)
    val LastLineEnd = Value(GridBagConstraints.LAST_LINE_END)
  }
}

/**
 * A panel that arranges its children in a grid. Layout details can be
 * given for each cell of the grid.
 *
 * @see java.awt.GridBagLayout
 */
class GridBagPanel extends Panel with LayoutContainer {
  override lazy val peer = new javax.swing.JPanel(new GridBagLayout) with SuperMixin
  import GridBagPanel._

  private def layoutManager = peer.getLayout.asInstanceOf[GridBagLayout]

  /**
   * Convenient conversion from xy-coords given as pairs to
   * grid bag constraints.
   */
  implicit def pair2Constraints(p: (Int, Int)): Constraints = {
    val c = new Constraints
    c.gridx = p._1
    c.gridy = p._2
    c
  }

  class Constraints(val peer: GridBagConstraints) extends Proxy {
    def self = peer
    def this(gridx: Int, gridy: Int,
             gridwidth: Int, gridheight: Int,
             weightx: Double, weighty: Double,
             anchor: Int, fill: Int, insets: Insets,
             ipadx: Int, ipady: Int) =
      this(new GridBagConstraints(gridx, gridy,
                                  gridwidth, gridheight,
                                  weightx, weighty,
                                  anchor, fill, insets,
                                  ipadx, ipady))
    def this() = this(new GridBagConstraints())
    def gridx: Int = peer.gridx
    def gridx_=(x: Int) { peer.gridx = x }
    def gridy: Int = peer.gridy
    def gridy_=(y: Int) { peer.gridy = y }
    def grid: (Int, Int) = (gridx, gridy)
    def grid_=(c: (Int, Int)) = {
      gridx = c._1
      gridy = c._2
    }

    def gridwidth: Int = peer.gridwidth
    def gridwidth_=(w: Int) { peer.gridwidth = w }
    def gridheight: Int = peer.gridheight
    def gridheight_=(h: Int) { peer.gridheight = h }
    def weightx: Double = peer.weightx
    def weightx_=(x: Double) { peer.weightx = x }
    def weighty: Double = peer.weighty
    def weighty_=(y: Double) { peer.weighty = y }
    def anchor: Anchor.Value = Anchor(peer.anchor)
    def anchor_=(a: Anchor.Value) { peer.anchor = a.id }
    def fill: Fill.Value = Fill(peer.fill)
    def fill_=(f: Fill.Value) { peer.fill = f.id }
    def insets: Insets = peer.insets
    def insets_=(i: Insets) { peer.insets = i }
    def ipadx: Int = peer.ipadx
    def ipadx_=(x: Int) { peer.ipadx = x }
    def ipady: Int = peer.ipady
    def ipady_=(y: Int) { peer.ipady = y }
  }

  protected def constraintsFor(comp: Component) =
    new Constraints(layoutManager.getConstraints(comp.peer))

  protected def areValid(c: Constraints): (Boolean, String) = (true, "")
  protected def add(c: Component, l: Constraints) { peer.add(c.peer, l.peer) }
}
