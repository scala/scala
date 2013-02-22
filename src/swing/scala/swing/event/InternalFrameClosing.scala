package scala.swing

import scala.swing.InternalFrame

/** Internal frame is in the process of being closed.
 *
 * @param source of event
 *
 * @author myst3r10n
 */
case class InternalFrameClosing(
  override val source: InternalFrame)
  extends InternalFrameEvent(source)

