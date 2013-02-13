package scala.swing

import moreswing.swing.InternalFrame

/** Internal frame has been unmaximized.
 *
 * @param source of event
 *
 * @author myst3r10n
 */
case class InternalFrameUnmaximized(
  override val source: InternalFrame)
  extends InternalFrameEvent(source)

