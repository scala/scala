package scala.swing

import scala.swing.InternalFrame

/** Internal frame is iconified.
 *
 * @param source of event
 *
 * @author myst3r10n
 */
case class InternalFrameIconified(
  override val source: InternalFrame)
  extends InternalFrameEvent(source)

