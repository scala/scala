package swing

class BoxPanel(orientation: Orientation)(content0: Component*) extends IndexedPanel {
  override lazy val peer = {
    val p = new javax.swing.JPanel
    val l = new javax.swing.BoxLayout(p, orientation.peer)
    p.setLayout(l)
    p
  }
  content ++ content0
}
