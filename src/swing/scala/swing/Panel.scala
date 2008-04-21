package scala.swing

/**
 * @see javax.swing.JPanel
 */
abstract class Panel(override val peer: javax.swing.JPanel) extends Component(peer) with Container.Wrapper
