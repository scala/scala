package scala.swing

import moreswing.swing.InternalFrame

/** Internal frame has been shifted.
 *
 * @param from internal frame
 * @param to internal frame
 *
 * @author myst3r10n
 */
case class InternalFrameShifted(
  val from: InternalFrame,
  val to: InternalFrame)
  extends InternalFrameEvent(from)

