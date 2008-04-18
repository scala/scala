package scala.swing

class BoxPanel(orientation: Orientation) extends Panel with IndexedContainer {
  override lazy val peer = {
    val p = new javax.swing.JPanel
    val l = new javax.swing.BoxLayout(p, orientation.peer)
    p.setLayout(l)
    p
  }
}
