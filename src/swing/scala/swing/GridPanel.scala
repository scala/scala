/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

object GridPanel {
  val Adapt = 0
}

/**
 * A panel that lays out its contents in a uniform grid.
 *
 * @see java.awt.GridLayout
 */
class GridPanel(rows0: Int, cols0: Int) extends Panel with SequentialContainer.Wrapper {
  override lazy val peer =
    new javax.swing.JPanel(new java.awt.GridLayout(rows0, cols0)) with SuperMixin

  /*type Constraints = (Int, Int)

  protected def constraintsFor(comp: Component) = {
    assert(peer.getComponentOrientation.isHorizontal)
    val idx = contents.indexOf(comp)
    val (r, c) = (((idx-1)/columns)+1, ((idx-1)%columns)+1)
    if (peer.getComponentOrientation.isLeftToRight) (r, c)
    else (r, columns-c+1)
  }

  protected def add(c: Component, l: Constraints) { peer.add(c.peer, (l._1-1)*columns+l._2) }
  protected def areValid(c: Constraints): (Boolean, String) =
    ((c._1 > 0 && c._2 > 0), "Grid coordinates (row,col) must be >= 1 but where " + c)*/

  private def layoutManager = peer.getLayout.asInstanceOf[java.awt.GridLayout]

  def rows: Int = layoutManager.getRows
  def rows_=(n: Int) { layoutManager.setRows(n) }
  def columns: Int = layoutManager.getColumns
  def columns_=(n: Int) { layoutManager.setColumns(n) }

  def vGap: Int = layoutManager.getVgap
  def vGap_=(n: Int) { layoutManager.setVgap(n) }
  def hGap: Int = layoutManager.getHgap
  def hGap_=(n: Int) { layoutManager.setHgap(n) }
}
