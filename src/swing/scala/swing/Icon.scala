package scala.swing

import javax.swing._

object Icon {
  case object Empty extends Icon {
    def getIconHeight: Int = 0
    def getIconWidth: Int = 0
    def paintIcon(c: java.awt.Component, g: java.awt.Graphics, x: Int, y: Int) {}
  }
}
