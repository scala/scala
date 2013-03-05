package scala.swing

import scala.swing.InternalFrame

/** Internal frame is de-activated.
 *
 * @param source of event
 *
 * @author myst3r10n
 */
case class InternalFrameDeactivated(
  override val source: InternalFrame)
  extends InternalFrameEvent(source)

