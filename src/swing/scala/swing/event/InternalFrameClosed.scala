package scala.swing

import scala.swing.InternalFrame

/** Internal frame has been closed.
 *
 * @param source of event
 *
 * @author myst3r10n
 */
case class InternalFrameClosed(
  override val source: InternalFrame)
  extends InternalFrameEvent(source)

