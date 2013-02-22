package scala.swing

import scala.swing.InternalFrame

/** Internal frame is de-iconified.
 *
 * @param source of event
 *
 * @author myst3r10n
 */
case class InternalFrameDeiconified(
  override val source: InternalFrame)
  extends InternalFrameEvent(source)

