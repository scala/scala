package scala.swing

import javax.swing. { DesktopManager, JDesktopPane }

import scala.swing.Component

/** It's [[javax.swing.JDesktopPane]].
 *
 * @author myst3r10n
 */
class DesktopPane extends Component {

  /** The [[javax.swing.JDesktopPane]] object. */
  override lazy val peer = new JDesktopPane

  /** Add the specified internal frame to this container.
   *
   * @param f The iternal frame.
   */
  def += (f: InternalFrame) { peer.add(f.peer) }

  /** @see [[javax.swing.JDesktopPane]]. */
  def accessibleContext = peer.getAccessibleContext

  /** @see [[javax.swing.JDesktopPane]]. */
  def allFrames =
    for(iframe <- peer.getAllFrames)
      yield(new InternalFrame { override lazy val peer = iframe } )

  /** @see [[javax.swing.JDesktopPane]]. */
  def allFramesInLayer(layer: Int) =
    for(iframe <- peer.getAllFramesInLayer(layer))
      yield(new InternalFrame { override lazy val peer = iframe } )

  /** @see [[javax.swing.JDesktopPane]]. */
  def componentZOrder(comp: Component, index: Int) { peer.setComponentZOrder(comp.peer, index) }

  /** @see [[javax.swing.JDesktopPane]]. */
  def desktopManager = peer.getDesktopManager

  /** @see [[javax.swing.JDesktopPane.setDesktopManager]]. */
  def desktopManager_= (d: DesktopManager) { peer.setDesktopManager(d) }

  /** @see [[javax.swing.JDesktopPane]]. */
  def dragMode = peer.getDragMode

  /** Inserts the specified internal frame to this container at the given position.
   * 
   * @param n The position at which to insert the internal frame, or -1 to append the internal frame to the end.
   * @param f The internal frame to be added.
   */
  def insert(n: Int, f: InternalFrame) { peer.add(f.peer, n)  }

  /** @see [[javax.swing.JDesktopPane]]. */
  def remove(index: Int) { peer.remove(index) }

  /** @see [[javax.swing.JDesktopPane]]. */
  def remove(comp: Component) { peer.remove(comp.peer) }

  /** Removes all the components from this container. */
  def removeAll { peer.removeAll }

  /** @see [[javax.swing.JDesktopPane]]. */
  def selectedFrame =
    if(DesktopPane.this.peer.getSelectedFrame == null)
      null
    else
      new InternalFrame { override lazy val peer = DesktopPane.this.peer.getSelectedFrame }

  /** @see [[javax.swing.JDesktopPane]]. */
  def selectedFrame_= (f: InternalFrame) { peer.setSelectedFrame(f.peer) }

  /** @see [[javax.swing.JDesktopPane]]. */
  def selectFrame_= (forward: Boolean) { peer.selectFrame(forward) }

  /** @see [[javax.swing.JDesktopPane]]. */
  def ui = peer.getUI

  /** @see [[javax.swing.JDesktopPane]]. */
  def uiClassID = peer.getUIClassID

  /** @see [[javax.swing.JDesktopPane]]. */
  def updateUI = peer.updateUI

}
