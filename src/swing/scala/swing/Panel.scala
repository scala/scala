package scala.swing

abstract class Panel extends Container {
  override lazy val peer: javax.swing.JPanel = new javax.swing.JPanel

  def layoutManager: java.awt.LayoutManager = peer.getLayout
}
