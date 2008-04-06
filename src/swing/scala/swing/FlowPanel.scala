package scala.swing

class FlowPanel(orientation: Orientation)(content0: Component*) extends IndexedPanel {
  override lazy val peer = new javax.swing.JPanel(new java.awt.FlowLayout(orientation.peer))
  content ++ content0
}
