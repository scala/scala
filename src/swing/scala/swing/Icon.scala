package scala.swing

import javax.swing._

/**
 * Helper object for icons.
 */
object Icon {
  /**
   * The empty icon. Use this icon instead of <code>null</code> to indicate
   * that you don't want an icon.
   */
  case object Empty extends Icon {
    def getIconHeight: Int = 0
    def getIconWidth: Int = 0
    def paintIcon(c: java.awt.Component, g: java.awt.Graphics, x: Int, y: Int) {}
  }

  def unwrap(icon: Icon): Icon = if (icon == null) Empty else icon
  def wrap(icon: Icon): Icon = if (icon == Empty) null else icon
}
