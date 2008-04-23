package scala.swing

/**
 * @see javax.swing.JPanel
 */
abstract class Panel extends Component with Container.Wrapper {
  override lazy val peer: javax.swing.JPanel = new javax.swing.JPanel
}
