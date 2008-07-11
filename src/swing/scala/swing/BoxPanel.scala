package scala.swing

/**
 * A panel that lays out its contents one after the other,
 * either horizontally or vertically.
 *
 * @see javax.swing.BoxLayout
 */
class BoxPanel(orientation: Orientation.Value) extends Panel with SequentialContainer.Wrapper {
  override lazy val peer = {
    val p = new javax.swing.JPanel with SuperMixin
    val l = new javax.swing.BoxLayout(p, orientation.id)
    p.setLayout(l)
    p
  }
}
