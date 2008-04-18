package scala.swing

trait Selectable {
  def peer: javax.swing.JComponent {
    def setSelected(b: Boolean)
    def isSelected(): Boolean
  }

  def selected: Boolean = peer.isSelected
  def selected_=(b: Boolean) = peer.setSelected(b)
}
