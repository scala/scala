package scala.swing

import scala.swing.InternalFrame

/** Internal frame has been maximized.
 *
 * @param source of event
 *
 * @author myst3r10n
 */
case class InternalFrameMaximized(
  override val source: InternalFrame)
  extends InternalFrameEvent(source)

