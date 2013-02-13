package scala.swing

import moreswing.swing.InternalFrame

/** Internal frame is activated.
 *
 * @param source of event
 *
 * @author myst3r10n
 */
case class InternalFrameActivated(
  override val source: InternalFrame)
  extends InternalFrameEvent(source)

