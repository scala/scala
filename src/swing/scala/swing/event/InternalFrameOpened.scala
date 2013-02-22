package scala.swing

import scala.swing.InternalFrame

/** Internal frame has been opened.
 *
 * @param source of event
 *
 * @author myst3r10n
 */
case class InternalFrameOpened(
  override val source: InternalFrame)
  extends InternalFrameEvent(source)

