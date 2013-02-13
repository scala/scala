package scala.swing

import moreswing.swing.InternalFrame

import scala.swing.event.UIEvent

/** Internal frame event.
 *
 * @param source of event
 *
 * @author myst3r10n
 */
abstract class InternalFrameEvent(
  override val source: InternalFrame)
  extends UIEvent

